const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const writeFixups = @import("block_writer.zig").writeFixups;
const xor_key = @import("build.zig").xor_key;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");
const plan = @import("plan.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    events: *sync.Channel(plan.Event, 16),
) !void {
    var receiver: OrderedReceiver = .init(events);
    defer receiver.deinit(gpa);

    runInner(gpa, output_dir, index_name, game, &receiver) catch |err| {
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
    game: games.Game,
    receiver: *OrderedReceiver,
) !void {
    var index: Index = try .init(gpa);
    defer index.deinit(gpa);

    while (true) switch (try receiver.next(gpa)) {
        .disk_start => |num| try emitDisk(gpa, output_dir, index_name, game, receiver, num, &index),
        .index_start => try emitIndex(gpa, receiver, output_dir, index_name, &index),
        .project_end => break,
        .err => return error.Reported,
        else => unreachable,
    };
}

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

fn emitDisk(
    gpa: std.mem.Allocator,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    receiver: *OrderedReceiver,
    disk_number: u8,
    index: *Index,
) !void {
    var out_name_buf: pathf.Path = .{};
    const out_name = try pathf.append(&out_name_buf, index_name);
    games.pointPathToDisk(game, out_name.full(), disk_number);

    const out_file = try output_dir.createFileZ(out_name.full(), .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    const lecf_start = try beginBlock(&out, "LECF");

    while (true) switch (try receiver.next(gpa)) {
        .room_start => |room_number| try emitRoom(gpa, receiver, disk_number, &out, &fixups, room_number, index),
        .disk_end => break,
        .err => return error.Reported,
        else => unreachable,
    };

    try endBlock(&out, &fixups, lecf_start);

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

fn emitRoom(
    gpa: std.mem.Allocator,
    receiver: *OrderedReceiver,
    disk_number: u8,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    room_number: u8,
    index: *Index,
) !void {
    const lflf_start = try beginBlock(out, "LFLF");

    try utils.growMultiArrayList(Room, &index.rooms, gpa, room_number + 1, .zero);
    index.rooms.set(room_number, .{
        .offset = @intCast(out.bytes_written),
        .disk = disk_number,
    });

    while (true) switch (try receiver.next(gpa)) {
        .glob => |*b| try emitGlob(gpa, out, fixups, index, room_number, b),
        .glob_start => |*b| try emitGlobBlock(gpa, receiver, out, fixups, index, room_number, b),
        .room_end => break,
        .err => return error.Reported,
        else => unreachable,
    };

    try endBlock(out, fixups, lflf_start);
}

fn emitRawBlock(
    gpa: std.mem.Allocator,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    raw_block: *const @FieldType(plan.Payload, "raw_block"),
) !void {
    var data_mut = raw_block.data;
    defer data_mut.deinit(gpa);

    try writeBlock(out, fixups, raw_block.block_id, raw_block.data.items);
}

fn emitGlob(
    gpa: std.mem.Allocator,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    index: *Index,
    room_number: u8,
    glob: *const @FieldType(plan.Payload, "glob"),
) !void {
    var data_mut = glob.data;
    defer data_mut.deinit(gpa);

    const start: u32 = @intCast(out.bytes_written);
    try writeBlock(out, fixups, glob.block_id, glob.data.items);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(gpa, index, room_number, glob.block_id, glob.glob_number, start, size);
}

fn emitGlobBlock(
    gpa: std.mem.Allocator,
    receiver: *OrderedReceiver,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    index: *Index,
    room_number: u8,
    glob: *const @FieldType(plan.Payload, "glob_start"),
) !void {
    const start = try beginBlockImpl(out, glob.block_id);

    while (true) switch (try receiver.next(gpa)) {
        .raw_block => |*b| try emitRawBlock(gpa, out, fixups, b),
        .glob_end => break,
        .err => return error.Reported,
        else => unreachable,
    };

    try endBlock(out, fixups, start);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(gpa, index, room_number, glob.block_id, glob.glob_number, start, size);
}

fn addGlobToIndex(
    gpa: std.mem.Allocator,
    index: *Index,
    room_number: u8,
    block_id: BlockId,
    glob_number: u16,
    offset_in_disk: u32,
    size: u32,
) !void {
    const directory = switch (block_id) {
        // XXX: this list is duplicated in extract
        blockId("RMIM") => &index.directories.room_images,
        blockId("RMDA") => &index.directories.rooms,
        blockId("SCRP") => &index.directories.scripts,
        blockId("DIGI"), blockId("TALK") => &index.directories.sounds,
        blockId("AKOS") => &index.directories.costumes,
        blockId("CHAR") => &index.directories.charsets,
        blockId("AWIZ"), blockId("MULT") => &index.directories.images,
        blockId("TLKE") => &index.directories.talkies,
        else => return error.BadData,
    };
    try utils.growMultiArrayList(DirectoryEntry, directory, gpa, glob_number + 1, .zero);
    if (directory.items(.room)[glob_number] != 0)
        @panic("TODO");
    const offset_in_room = offset_in_disk - index.rooms.items(.offset)[room_number];
    directory.set(glob_number, .{
        .room = room_number,
        .offset = offset_in_room,
        .size = size,
    });
}

fn writeBlock(
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    block_id: BlockId,
    data: []const u8,
) !void {
    const start = try beginBlockImpl(out, block_id);
    try out.writer().writeAll(data);
    try endBlock(out, fixups, start);
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
            comptime if (std.mem.eql(u8, field, "rooms")) continue;
            try @field(index.directories, field).append(gpa, .{
                .room = 0,
                .offset = 0,
                .size = 0xffff_ffff,
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

    pub const zero: DirectoryEntry = .{
        .room = 0,
        .offset = 0,
        .size = 0,
    };
};

fn emitIndex(
    gpa: std.mem.Allocator,
    receiver: *OrderedReceiver,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    index: *Index,
) !void {
    const out_file = try output_dir.createFileZ(index_name, .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    // SCUMM outputs sequential room numbers for these whether or not the room
    // actually exists.
    for (index.directories.room_images.items(.room), 0..) |*room, i|
        room.* = @intCast(i);
    for (index.directories.rooms.items(.room), 0..) |*room, i|
        room.* = @intCast(i);

    while (true) switch (try receiver.next(gpa)) {
        .index_block => |id| switch (id) {
            .DIRI => try writeDirectory(&out, &fixups, blockId("DIRI"), &index.directories.room_images),
            .DIRR => try writeDirectory(&out, &fixups, blockId("DIRR"), &index.directories.rooms),
            .DIRS => try writeDirectory(&out, &fixups, blockId("DIRS"), &index.directories.scripts),
            .DIRN => try writeDirectory(&out, &fixups, blockId("DIRN"), &index.directories.sounds),
            .DIRC => try writeDirectory(&out, &fixups, blockId("DIRC"), &index.directories.costumes),
            .DIRF => try writeDirectory(&out, &fixups, blockId("DIRF"), &index.directories.charsets),
            .DIRM => try writeDirectory(&out, &fixups, blockId("DIRM"), &index.directories.images),
            .DIRT => try writeDirectory(&out, &fixups, blockId("DIRT"), &index.directories.talkies),
            .DLFL => {
                const start = try beginBlock(&out, "DLFL");
                try out.writer().writeInt(u16, @intCast(index.rooms.len), .little);
                try out.writer().writeAll(std.mem.sliceAsBytes(index.rooms.items(.offset)));
                try endBlock(&out, &fixups, start);
            },
            .DISK => {
                const start = try beginBlock(&out, "DISK");
                try out.writer().writeInt(u16, @intCast(index.rooms.len), .little);
                try out.writer().writeAll(index.rooms.items(.disk));
                try endBlock(&out, &fixups, start);
            },
            .RNAM => unreachable,
        },
        .raw_block => |*rb| try emitRawBlock(gpa, &out, &fixups, rb),
        .index_end => break,
        .err => return error.Reported,
        else => unreachable,
    };

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

fn writeDirectory(
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    block_id: BlockId,
    directory: *const std.MultiArrayList(DirectoryEntry),
) !void {
    const start = try beginBlockImpl(out, block_id);
    try out.writer().writeInt(u16, @intCast(directory.len), .little);
    const slice = directory.slice();
    try out.writer().writeAll(slice.items(.room));
    try out.writer().writeAll(std.mem.sliceAsBytes(slice.items(.offset)));
    try out.writer().writeAll(std.mem.sliceAsBytes(slice.items(.size)));
    try endBlock(out, fixups, start);
}
