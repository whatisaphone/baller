const std = @import("std");

const build_options = @import("build_options");

const Ast = @import("Ast.zig");
const Blinkenlights = @import("Blinkenlights.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const BlockId = @import("block_id.zig").BlockId;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockKnown = @import("block_writer.zig").beginBlockKnown;
const endBlock = @import("block_writer.zig").endBlock;
const endBlockKnown = @import("block_writer.zig").endBlockKnown;
const fxbc = @import("block_writer.zig").fxbc;
const writeFixups = @import("block_writer.zig").writeFixups;
const Options = @import("build.zig").Options;
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
    blinken: *Blinkenlights,
    emit_blink: Blinkenlights.NodeId,
    project: *const Project,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    options: *const Options,
    events: *sync.Channel(sync.OrderedEvent(plan.Payload), sync.max_concurrency),
) !void {
    defer blinken.removeNode(emit_blink);

    var receiver: sync.OrderedReceiver(plan.Payload, sync.max_concurrency) = .init(events);
    defer receiver.deinit(gpa);

    runInner(
        gpa,
        diagnostic,
        blinken,
        emit_blink,
        project,
        output_dir,
        index_name,
        options,
        &receiver,
    ) catch |err| {
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
    diagnostic: *Diagnostic,
    blinken: *Blinkenlights,
    emit_blink: Blinkenlights.NodeId,
    project: *const Project,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    options: *const Options,
    receiver: *sync.OrderedReceiver(plan.Payload, sync.max_concurrency),
) !void {
    const target_message = try receiver.next(gpa);
    const target = switch (target_message) {
        .target => |t| t,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    var index: Index = try .init(gpa, target);
    defer index.deinit(gpa);

    const cx: Cx = .{
        .gpa = gpa,
        .diagnostic = diagnostic,
        .blinken = blinken,
        .project = project,
        .output_dir = output_dir,
        .index_name = index_name,
        .options = options,
        .receiver = receiver,
        .target = target,
        .index = &index,
    };

    while (true) switch (try receiver.next(gpa)) {
        .nop => {},
        .disk_start => |num| try emitDisk(&cx, num, emit_blink),
        .index_start => try emitIndex(&cx),
        .project_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };
}

const Cx = struct {
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    blinken: *Blinkenlights,
    project: *const Project,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    options: *const Options,
    receiver: *sync.OrderedReceiver(plan.Payload, sync.max_concurrency),
    target: games.Target,
    index: *Index,
};

fn emitDisk(cx: *const Cx, disk_number: u8, emit_blink: Blinkenlights.NodeId) !void {
    var out_name_buf: [games.longest_index_name_len + 1]u8 = undefined;
    const out_name = std.fmt.bufPrintZ(&out_name_buf, "{s}", .{cx.index_name}) catch unreachable;
    games.pointPathToDisk(cx.target, out_name, disk_number);

    const blink = cx.blinken.addNode(emit_blink);
    defer cx.blinken.removeNode(blink);
    cx.blinken.setText(blink, out_name);

    const out_file = try cx.output_dir.createFileZ(out_name, .{});
    defer out_file.close();
    var out_buf: [4096]u8 = undefined;
    var out_direct = out_file.writer(&out_buf);
    var out_xor: io.XorWriter = .init(&out_direct.interface, xor_key, &.{});
    const out = &out_xor.interface;

    var fixups: std.ArrayList(Fixup) = .empty;
    defer fixups.deinit(cx.gpa);

    const lecf_start = try beginBlock(out, .LECF);

    while (true) switch (try cx.receiver.next(cx.gpa)) {
        .room_start => |room_number| try emitRoom(cx, disk_number, out, &fixups, room_number),
        .disk_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    try endBlock(cx.gpa, out, &fixups, lecf_start);

    try out_direct.interface.flush();

    try writeFixups(&out_direct, out, fixups.items);
}

fn emitRoom(
    cx: *const Cx,
    disk_number: u8,
    out: *std.io.Writer,
    fixups: *std.ArrayList(Fixup),
    room_number: u8,
) !void {
    const lflf_start = try beginBlock(out, .LFLF);

    try utils.growMultiArrayList(Room, &cx.index.rooms, cx.gpa, room_number + 1, .zero);
    cx.index.rooms.set(room_number, .{
        .offset = fxbc.pos(out),
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

    try endBlock(cx.gpa, out, fixups, lflf_start);
}

fn emitRawBlock(
    gpa: std.mem.Allocator,
    out: *std.io.Writer,
    raw_block: *const @FieldType(plan.Payload, "raw_block"),
) !void {
    defer gpa.free(raw_block.data);

    try writeBlock(out, raw_block.block_id, raw_block.data);
}

fn emitGlob(
    cx: *const Cx,
    out: *std.io.Writer,
    room_number: u8,
    glob: *const @FieldType(plan.Payload, "glob"),
) !void {
    defer cx.gpa.free(glob.data);

    const start = fxbc.pos(out);
    try writeBlock(out, glob.block_id, glob.data);
    const end = fxbc.pos(out);
    const size = end - start;

    try addGlobToIndex(cx, room_number, glob.node_index, glob.block_id, glob.glob_number, start, size);
}

fn emitGlobBlock(
    cx: *const Cx,
    out: *std.io.Writer,
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

    try endBlock(cx.gpa, out, fixups, start);
    const end = fxbc.pos(out);
    const size = end - start;

    try addGlobToIndex(cx, room_number, glob.node_index, glob.block_id, glob.glob_number, start, size);
}

fn addGlobToIndex(
    cx: *const Cx,
    room_number: u8,
    node_index: Ast.NodeIndex,
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

    if (directory.items(.room)[glob_number] != 0) {
        cx.diagnostic.errAt(
            .node(&cx.project.files.items[room_number].?, node_index),
            "duplicate glob number {}",
            .{glob_number},
        );
        return error.AddedToDiagnostic;
    }

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

fn writeBlock(out: *std.io.Writer, block_id: BlockId, data: []const u8) !void {
    const start = try beginBlockKnown(out, block_id, @intCast(data.len));
    try out.writeAll(data);
    endBlockKnown(out, start);
}

const Index = struct {
    rooms: std.MultiArrayList(Room),
    directories: Directories,

    fn init(gpa: std.mem.Allocator, target: games.Target) !Index {
        var index: Index = .{
            .rooms = .empty,
            .directories = .{},
        };
        errdefer index.deinit(gpa);

        try index.rooms.ensureTotalCapacity(gpa, 32);

        try index.directories.room_images.ensureTotalCapacity(gpa, index.rooms.capacity);
        try index.directories.rooms.ensureTotalCapacity(gpa, index.rooms.capacity);
        try index.directories.scripts.ensureTotalCapacity(gpa, 128);
        try index.directories.sounds.ensureTotalCapacity(gpa, 2048);
        try index.directories.costumes.ensureTotalCapacity(gpa, 512);
        try index.directories.charsets.ensureTotalCapacity(gpa, 8);
        try index.directories.images.ensureTotalCapacity(gpa, 1024);
        if (games.hasTalkies(target.pickAnyGame()))
            try index.directories.talkies.ensureTotalCapacity(gpa, 2048);

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
    var out_buf: [4096]u8 = undefined;
    var out_direct = out_file.writer(&out_buf);
    var out_xor: io.XorWriter = .init(&out_direct.interface, xor_key, &.{});
    const out = &out_xor.interface;

    var fixups: std.ArrayList(Fixup) = .empty;
    defer fixups.deinit(cx.gpa);

    // SCUMM outputs sequential room numbers for these whether or not the room
    // actually exists.
    for (cx.index.directories.room_images.items(.room), 0..) |*room, i|
        room.* = @intCast(i);
    for (cx.index.directories.rooms.items(.room), 0..) |*room, i|
        room.* = @intCast(i);

    while (true) switch (try cx.receiver.next(cx.gpa)) {
        .index_maxs => |data| try writeMaxs(cx, out, data),
        .index_block => |id| switch (id) {
            .DIRI => try writeDirectory(cx.gpa, out, &fixups, .DIRI, &cx.index.directories.room_images),
            .DIRR => try writeDirectory(cx.gpa, out, &fixups, .DIRR, &cx.index.directories.rooms),
            .DIRS => try writeDirectory(cx.gpa, out, &fixups, .DIRS, &cx.index.directories.scripts),
            .DIRN => try writeDirectory(cx.gpa, out, &fixups, .DIRN, &cx.index.directories.sounds),
            .DIRC => try writeDirectory(cx.gpa, out, &fixups, .DIRC, &cx.index.directories.costumes),
            .DIRF => try writeDirectory(cx.gpa, out, &fixups, .DIRF, &cx.index.directories.charsets),
            .DIRM => try writeDirectory(cx.gpa, out, &fixups, .DIRM, &cx.index.directories.images),
            .DIRT => try writeDirectory(cx.gpa, out, &fixups, .DIRT, &cx.index.directories.talkies),
            .DLFL => {
                const start = try beginBlock(out, .DLFL);
                try out.writeInt(u16, @intCast(cx.index.rooms.len), .little);
                try out.writeAll(std.mem.sliceAsBytes(cx.index.rooms.items(.offset)));
                try endBlock(cx.gpa, out, &fixups, start);
            },
            .DISK => {
                const start = try beginBlock(out, .DISK);
                try out.writeInt(u16, @intCast(cx.index.rooms.len), .little);
                try out.writeAll(cx.index.rooms.items(.disk));
                try endBlock(cx.gpa, out, &fixups, start);
            },
            .RNAM => unreachable,
        },
        .raw_block => |*rb| {
            if (rb.block_id == .INIB and cx.options.write_version)
                try writeVersionIntoInib(cx.gpa, out, &fixups, rb)
            else
                try emitRawBlock(cx.gpa, out, rb);
        },
        .index_end => break,
        .err => return error.AddedToDiagnostic,
        else => unreachable,
    };

    try out_direct.interface.flush();

    try writeFixups(&out_direct, out, fixups.items);
}

fn writeMaxs(cx: *const Cx, out: *std.io.Writer, data: []u8) !void {
    defer cx.gpa.free(data);

    if (data.len != games.maxsLen(cx.target.pickAnyGame()))
        return error.BadData;

    // Overwrite some of the fields I know how to generate
    const maxs: *align(1) Maxs = @ptrCast(data);
    maxs.rooms = @intCast(cx.index.rooms.len);
    maxs.scripts = @intCast(cx.index.directories.scripts.len);
    maxs.sounds = @intCast(cx.index.directories.sounds.len);
    maxs.charsets = @intCast(cx.index.directories.charsets.len);
    maxs.costumes = @intCast(cx.index.directories.costumes.len);
    maxs.images = @intCast(cx.index.directories.images.len);
    if (games.hasTalkies(cx.target.pickAnyGame()))
        maxs.talkies = @intCast(cx.index.directories.talkies.len);

    const start = try beginBlockKnown(out, .MAXS, @intCast(data.len));
    try out.writeAll(data);
    endBlockKnown(out, start);
}

fn writeDirectory(
    gpa: std.mem.Allocator,
    out: *std.io.Writer,
    fixups: *std.ArrayList(Fixup),
    block_id: BlockId,
    directory: *const std.MultiArrayList(DirectoryEntry),
) !void {
    const start = try beginBlock(out, block_id);
    try out.writeInt(u16, @intCast(directory.len), .little);
    const slice = directory.slice();
    try out.writeAll(slice.items(.room));
    try out.writeAll(std.mem.sliceAsBytes(slice.items(.offset)));
    try out.writeAll(std.mem.sliceAsBytes(slice.items(.size)));
    try endBlock(gpa, out, fixups, start);
}

fn writeVersionIntoInib(
    gpa: std.mem.Allocator,
    out: *std.io.Writer,
    fixups: *std.ArrayList(Fixup),
    raw_block: *const @FieldType(plan.Payload, "raw_block"),
) !void {
    std.debug.assert(raw_block.block_id == .INIB);

    // Discard the data since we're replacing it
    gpa.free(raw_block.data);

    const inib_start = try beginBlock(out, .INIB);

    const note_start = try beginBlock(out, .NOTE);
    // double-xor so it's readable in the raw file
    var xor: io.XorWriter = .init(out, xor_key, &.{});
    try xor.interface.print(
        "\r\nBuilt with Baller {s} <https://baller.whatisaph.one/>\r\n",
        .{build_options.version},
    );
    try out.writeByte(0);
    try endBlock(gpa, out, fixups, note_start);

    try endBlock(gpa, out, fixups, inib_start);
}
