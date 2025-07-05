const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const BlockId = @import("block_id.zig").BlockId;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockKnown = @import("block_writer.zig").beginBlockKnown;
const endBlock = @import("block_writer.zig").endBlock;
const endBlockKnown = @import("block_writer.zig").endBlockKnown;
const writeFixups = @import("block_writer.zig").writeFixups;
const Maxs = @import("extract.zig").Maxs;
const xor_key = @import("extract.zig").xor_key;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const plan = @import("plan.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    events: *sync.Channel(plan.Event, 16),
) !void {
    var receiver: OrderedReceiver = .init(events);
    defer receiver.deinit(gpa);

    runInner(gpa, output_dir, index_name, &receiver) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);

        // Consume all events, so the planner threads don't hang trying to send
        // them, so the thread pool is able to deinit, so the app doesn't hang.
        // This leaks memory, but it's better than nothing.
        while (true) switch (try receiver.next(gpa)) {
            .project_end => break,
            else => {}, // Memory is leaked here
        };

        return error.AddedToDiagnostic;
    };
}

pub fn runInner(
    gpa: std.mem.Allocator,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    receiver: *OrderedReceiver,
) !void {
    var index: Index = try .init(gpa);
    defer index.deinit(gpa);

    const target_message = try receiver.next(gpa);
    const target = switch (target_message) {
        .target => |t| t,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    const cx: Cx = .{
        .gpa = gpa,
        .output_dir = output_dir,
        .index_name = index_name,
        .receiver = receiver,
        .target = target,
        .index = &index,
    };

    while (true) switch (try receiver.next(gpa)) {
        .nop => {},
        .disk_start => |num| try emitDisk(&cx, num),
        .index_start => try emitIndex(&cx),
        .project_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };
}

const Cx = struct {
    gpa: std.mem.Allocator,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    receiver: *OrderedReceiver,
    target: games.Target,
    index: *Index,
};

const OrderedReceiver = struct {
    channel: *sync.Channel(plan.Event, 16),
    // TODO: optimize mem usage
    buffer: std.ArrayListUnmanaged(?plan.Payload),
    index: u16,

    fn init(channel: *sync.Channel(plan.Event, 16)) OrderedReceiver {
        return .{
            .channel = channel,
            .buffer = .empty,
            .index = 0,
        };
    }

    fn deinit(self: *OrderedReceiver, gpa: std.mem.Allocator) void {
        self.buffer.deinit(gpa);
    }

    fn next(self: *OrderedReceiver, gpa: std.mem.Allocator) !plan.Payload {
        while (true) {
            if (self.index < self.buffer.items.len) {
                if (self.buffer.items[self.index]) |result| {
                    self.index += 1;
                    return result;
                }
            }

            try self.receive(gpa);
        }
    }

    fn receive(self: *OrderedReceiver, gpa: std.mem.Allocator) !void {
        const event = self.channel.receive();
        try utils.growArrayList(?plan.Payload, &self.buffer, gpa, event.index + 1, null);
        std.debug.assert(self.buffer.items[event.index] == null);
        self.buffer.items[event.index] = event.payload;
    }
};

fn emitDisk(cx: *const Cx, disk_number: u8) !void {
    var out_name_buf: [games.longest_index_name_len + 1]u8 = undefined;
    const out_name = std.fmt.bufPrintZ(&out_name_buf, "{s}", .{cx.index_name}) catch unreachable;
    games.pointPathToDisk(cx.target, out_name, disk_number);

    const out_file = try cx.output_dir.createFileZ(out_name, .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    const lecf_start = try beginBlock(&out, .LECF);

    while (true) switch (try cx.receiver.next(cx.gpa)) {
        .room_start => |room_number| try emitRoom(cx, disk_number, &out, &fixups, room_number),
        .disk_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    try endBlock(&out, &fixups, lecf_start);

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

fn emitRoom(
    cx: *const Cx,
    disk_number: u8,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    room_number: u8,
) !void {
    const lflf_start = try beginBlock(out, .LFLF);

    try utils.growMultiArrayList(Room, &cx.index.rooms, cx.gpa, room_number + 1, .zero);
    cx.index.rooms.set(room_number, .{
        .offset = @intCast(out.bytes_written),
        .disk = disk_number,
    });

    while (true) switch (try cx.receiver.next(cx.gpa)) {
        .glob => |*b| try emitGlob(cx, out, room_number, b),
        .glob_start => |*b| try emitGlobBlock(cx, out, fixups, room_number, b),
        .raw_block => |*b| try emitRawBlock(cx.gpa, out, b),
        .room_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    try endBlock(out, fixups, lflf_start);
}

fn emitRawBlock(
    gpa: std.mem.Allocator,
    out: anytype,
    raw_block: *const @FieldType(plan.Payload, "raw_block"),
) !void {
    var data_mut = raw_block.data;
    defer data_mut.deinit(gpa);

    try writeBlock(out, raw_block.block_id, raw_block.data.items);
}

fn emitGlob(
    cx: *const Cx,
    out: anytype,
    room_number: u8,
    glob: *const @FieldType(plan.Payload, "glob"),
) !void {
    var data_mut = glob.data;
    defer data_mut.deinit(cx.gpa);

    const start: u32 = @intCast(out.bytes_written);
    try writeBlock(out, glob.block_id, glob.data.items);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(cx, room_number, glob.block_id, glob.glob_number, start, size);
}

fn emitGlobBlock(
    cx: *const Cx,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    room_number: u8,
    glob: *const @FieldType(plan.Payload, "glob_start"),
) !void {
    const start = try beginBlock(out, glob.block_id);

    while (true) switch (try cx.receiver.next(cx.gpa)) {
        .raw_block => |*b| try emitRawBlock(cx.gpa, out, b),
        .glob_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    try endBlock(out, fixups, start);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(cx, room_number, glob.block_id, glob.glob_number, start, size);
}

fn addGlobToIndex(
    cx: *const Cx,
    room_number: u8,
    block_id: BlockId,
    glob_number: u16,
    offset_in_disk: u32,
    size: u32,
) !void {
    const kind = Symbols.GlobKind.fromBlockId(block_id) orelse return error.BadData;
    const directory = switch (kind) {
        .room_image => &cx.index.directories.room_images,
        .room => &cx.index.directories.rooms,
        .script => &cx.index.directories.scripts,
        .sound => &cx.index.directories.sounds,
        .costume => &cx.index.directories.costumes,
        .charset => &cx.index.directories.charsets,
        .image => &cx.index.directories.images,
        .talkie => &cx.index.directories.talkies,
    };

    const zero: DirectoryEntry = .{
        .room = 0,
        .offset = 0,
        .size = games.directoryNonPresentLen(cx.target),
    };
    try utils.growMultiArrayList(DirectoryEntry, directory, cx.gpa, glob_number + 1, zero);

    if (directory.items(.room)[glob_number] != 0)
        @panic("TODO");

    const offset_in_room = offset_in_disk - cx.index.rooms.items(.offset)[room_number];
    const write_size = if (block_id == .MULT and !games.writeMultLen(cx.target))
        std.math.maxInt(u32)
    else
        size;
    directory.set(glob_number, .{
        .room = room_number,
        .offset = offset_in_room,
        .size = write_size,
    });
}

fn writeBlock(out: anytype, block_id: BlockId, data: []const u8) !void {
    const start = try beginBlockKnown(out, block_id, @intCast(data.len));
    try out.writer().writeAll(data);
    endBlockKnown(out, start);
}

const Index = struct {
    rooms: std.MultiArrayList(Room),
    directories: Directories,

    fn init(gpa: std.mem.Allocator) !Index {
        var index: Index = .{
            .rooms = .empty,
            .directories = .{},
        };

        // Globs start at 1, so 0 doesn't exist, so SCUMM sets the sizes in the
        // 0 entries to 0xffff_ffff.
        inline for (comptime std.meta.fieldNames(Directories)) |field| {
            // (except for DIRR, for some reason)
            const size = comptime if (std.mem.eql(u8, field, "rooms")) 0 else 0xffff_ffff;
            try @field(index.directories, field).append(gpa, .{
                .room = 0,
                .offset = 0,
                .size = size,
            });
        }

        return index;
    }

    fn deinit(self: *Index, gpa: std.mem.Allocator) void {
        self.directories.deinit(gpa);
        self.rooms.deinit(gpa);
    }
};

const Room = struct {
    offset: u32,
    disk: u8,

    pub const zero: Room = .{ .offset = 0, .disk = 0 };
};

const Directories = struct {
    room_images: std.MultiArrayList(DirectoryEntry) = .{},
    rooms: std.MultiArrayList(DirectoryEntry) = .{},
    scripts: std.MultiArrayList(DirectoryEntry) = .{},
    sounds: std.MultiArrayList(DirectoryEntry) = .{},
    costumes: std.MultiArrayList(DirectoryEntry) = .{},
    charsets: std.MultiArrayList(DirectoryEntry) = .{},
    images: std.MultiArrayList(DirectoryEntry) = .{},
    talkies: std.MultiArrayList(DirectoryEntry) = .{},

    fn deinit(self: *Directories, gpa: std.mem.Allocator) void {
        self.talkies.deinit(gpa);
        self.images.deinit(gpa);
        self.charsets.deinit(gpa);
        self.costumes.deinit(gpa);
        self.sounds.deinit(gpa);
        self.scripts.deinit(gpa);
        self.rooms.deinit(gpa);
        self.room_images.deinit(gpa);
    }
};

const DirectoryEntry = struct {
    room: u8,
    offset: u32,
    size: u32,
};

fn emitIndex(cx: *const Cx) !void {
    const out_file = try cx.output_dir.createFileZ(cx.index_name, .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    // SCUMM outputs sequential room numbers for these whether or not the room
    // actually exists.
    for (cx.index.directories.room_images.items(.room), 0..) |*room, i|
        room.* = @intCast(i);
    for (cx.index.directories.rooms.items(.room), 0..) |*room, i|
        room.* = @intCast(i);

    while (true) switch (try cx.receiver.next(cx.gpa)) {
        .index_maxs => |data| try writeMaxs(cx, &out, data),
        .index_block => |id| switch (id) {
            .DIRI => try writeDirectory(&out, &fixups, .DIRI, &cx.index.directories.room_images),
            .DIRR => try writeDirectory(&out, &fixups, .DIRR, &cx.index.directories.rooms),
            .DIRS => try writeDirectory(&out, &fixups, .DIRS, &cx.index.directories.scripts),
            .DIRN => try writeDirectory(&out, &fixups, .DIRN, &cx.index.directories.sounds),
            .DIRC => try writeDirectory(&out, &fixups, .DIRC, &cx.index.directories.costumes),
            .DIRF => try writeDirectory(&out, &fixups, .DIRF, &cx.index.directories.charsets),
            .DIRM => try writeDirectory(&out, &fixups, .DIRM, &cx.index.directories.images),
            .DIRT => try writeDirectory(&out, &fixups, .DIRT, &cx.index.directories.talkies),
            .DLFL => {
                const start = try beginBlock(&out, .DLFL);
                try out.writer().writeInt(u16, @intCast(cx.index.rooms.len), .little);
                try out.writer().writeAll(std.mem.sliceAsBytes(cx.index.rooms.items(.offset)));
                try endBlock(&out, &fixups, start);
            },
            .DISK => {
                const start = try beginBlock(&out, .DISK);
                try out.writer().writeInt(u16, @intCast(cx.index.rooms.len), .little);
                try out.writer().writeAll(cx.index.rooms.items(.disk));
                try endBlock(&out, &fixups, start);
            },
            .RNAM => unreachable,
        },
        .raw_block => |*rb| try emitRawBlock(cx.gpa, &out, rb),
        .index_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

fn writeMaxs(cx: *const Cx, out: anytype, data_mut: std.ArrayListUnmanaged(u8)) !void {
    var data = data_mut;
    defer data.deinit(cx.gpa);

    if (data.items.len != games.maxsLen(cx.target.pickAnyGame()))
        return error.BadData;

    // Overwrite some of the fields I know how to generate
    const maxs: *align(1) Maxs = @ptrCast(data.items);
    maxs.rooms = @intCast(cx.index.rooms.len);
    maxs.scripts = @intCast(cx.index.directories.scripts.len);
    maxs.sounds = @intCast(cx.index.directories.sounds.len);
    maxs.charsets = @intCast(cx.index.directories.charsets.len);
    maxs.costumes = @intCast(cx.index.directories.costumes.len);
    maxs.images = @intCast(cx.index.directories.images.len);
    if (games.hasTalkies(cx.target.pickAnyGame()))
        maxs.talkies = @intCast(cx.index.directories.talkies.len);

    const start = try beginBlockKnown(out, .MAXS, @intCast(data.items.len));
    try out.writer().writeAll(data.items);
    endBlockKnown(out, start);
}

fn writeDirectory(
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    block_id: BlockId,
    directory: *const std.MultiArrayList(DirectoryEntry),
) !void {
    const start = try beginBlock(out, block_id);
    try out.writer().writeInt(u16, @intCast(directory.len), .little);
    const slice = directory.slice();
    try out.writer().writeAll(slice.items(.room));
    try out.writer().writeAll(std.mem.sliceAsBytes(slice.items(.offset)));
    try out.writer().writeAll(std.mem.sliceAsBytes(slice.items(.size)));
    try endBlock(out, fixups, start);
}
