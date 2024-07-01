const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const parseBlockId = @import("block_id.zig").parseBlockId;
const fs = @import("fs.zig");
const io = @import("io.zig");

pub const xor_key = 0x69;

pub fn runCli(allocator: std.mem.Allocator) !void {
    if (std.os.argv.len != 1 + 1 + 2)
        return error.CommandLine;

    const project_txt_path = std.mem.sliceTo(std.os.argv[2], 0);
    const output_path = std.mem.sliceTo(std.os.argv[3], 0);

    try run(allocator, &.{
        .project_txt_path = project_txt_path,
        .output_path = output_path,
    });
}

const Build = struct {
    project_txt_path: [:0]const u8,
    output_path: [:0]const u8,
};

pub fn run(allocator: std.mem.Allocator, args: *const Build) !void {
    const project_txt_path = args.project_txt_path;

    var output_path_buf = std.BoundedArray(u8, 4095){};
    try output_path_buf.appendSlice(args.output_path);
    try output_path_buf.append(0);
    const output_path = output_path_buf.buffer[0 .. output_path_buf.len - 1 :0];

    if (!std.mem.endsWith(u8, output_path, ".he0"))
        return error.CommandLine;

    // Create output dir. Borrow the slash temporarily to get the dir name
    const output_path_slash = std.mem.lastIndexOfScalar(u8, output_path, '/') orelse
        return error.CommandLine;
    output_path[output_path_slash] = 0;
    try fs.makeDirIfNotExistZ(std.fs.cwd(), output_path[0..output_path_slash :0]);
    output_path[output_path_slash] = '/';

    const project_txt_file = try std.fs.cwd().openFileZ(project_txt_path, .{});
    defer project_txt_file.close();

    var project_txt_reader = std.io.bufferedReader(project_txt_file.reader());
    var project_txt_line_buf: [256]u8 = undefined;

    var cur_path = std.BoundedArray(u8, 4095){};
    try cur_path.appendSlice(project_txt_path);
    popPathFile(&cur_path);

    var index: Index = .{};
    defer index.deinit(allocator);

    // Globs start at 1, so 0 doesn't exist, so set the sizes to 0xffff_ffff.
    inline for (std.meta.fields(Directories)) |field| {
        // (except for DIRR, for some reason)
        if (!std.meta.eql(field.name, "rooms")) {
            try @field(index.directories, field.name).append(allocator, .{
                .room = 0,
                .offset = 0,
                .len = 0xffff_ffff,
            });
        }
    }

    try readIndexBlobs(allocator, &index, &cur_path);

    var cur_state: ?DiskState = null;

    while (true) {
        const project_line = project_txt_reader.reader()
            .readUntilDelimiter(&project_txt_line_buf, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (!std.mem.startsWith(u8, project_line, "room "))
            return error.BadData;
        var project_line_words = std.mem.splitScalar(u8, project_line[5..], ' ');
        const disk_number_str = project_line_words.next() orelse return error.BadData;
        const room_number_str = project_line_words.next() orelse return error.BadData;
        const room_name = project_line_words.next() orelse return error.BadData;
        if (project_line_words.next()) |_| return error.BadData;

        const disk_number = try std.fmt.parseInt(u8, disk_number_str, 10);
        if (disk_number < 1 or disk_number > 26) return error.BadData;

        const room_number = try std.fmt.parseInt(u8, room_number_str, 10);
        if (room_number < 1) return error.BadData;

        try growArrayList([]u8, &index.room_names, allocator, room_number + 1, &.{});
        index.room_names.items[room_number] = try allocator.dupe(u8, room_name);

        if (cur_state) |*state| if (state.disk_number != disk_number) {
            try finishDisk(state);
            cur_state = null;
        };

        if (cur_state == null) {
            cur_state = @as(DiskState, undefined); // TODO: is there a better way?
            try startDisk(allocator, disk_number, output_path, &cur_state.?);
        }

        const state = &cur_state.?;

        try cur_path.appendSlice(room_name);
        try cur_path.append('/');
        defer cur_path.len -= @intCast(room_name.len + 1);

        const room_file = room_file: {
            try cur_path.appendSlice("room.txt\x00");
            defer cur_path.len -= 9;

            const room_txt_path = cur_path.buffer[0 .. cur_path.len - 1 :0];
            break :room_file try std.fs.cwd().openFileZ(room_txt_path, .{});
        };
        defer room_file.close();

        var room_reader = std.io.bufferedReader(room_file.reader());
        var room_line_buf: [256]u8 = undefined;

        const lflf_fixup = try beginBlock(&state.writer, "LFLF");

        try growArrayList(u8, &index.lfl_disks, allocator, room_number + 1, 0);
        index.lfl_disks.items[room_number] = disk_number;

        try growArrayList(u32, &index.lfl_offsets, allocator, room_number + 1, 0);
        index.lfl_offsets.items[room_number] = @intCast(state.writer.bytes_written);

        while (true) {
            const room_line_str = room_reader.reader()
                .readUntilDelimiter(&room_line_buf, '\n') catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            switch (try parseRoomLine(room_line_str)) {
                .raw_glob => |raw_glob| try handleRawGlob(
                    allocator,
                    room_number,
                    raw_glob,
                    &cur_path,
                    state,
                    &index,
                ),
            }
        }

        try endBlock(&state.writer, &state.fixups, lflf_fixup);
    }

    if (cur_state) |*state|
        try finishDisk(state);

    try writeIndex(allocator, &index, output_path);
}

fn readIndexBlobs(
    allocator: std.mem.Allocator,
    index: *Index,
    cur_path: *std.BoundedArray(u8, 4095),
) !void {
    {
        try cur_path.appendSlice("maxs.bin\x00");
        defer cur_path.len -= 9;

        const path = cur_path.buffer[0 .. cur_path.len - 1 :0];
        index.maxs = try std.fs.cwd().readFileAlloc(allocator, path, 1 << 20);
    }

    {
        try cur_path.appendSlice("dobj.bin\x00");
        defer cur_path.len -= 9;

        const path = cur_path.buffer[0 .. cur_path.len - 1 :0];
        index.dobj = try std.fs.cwd().readFileAlloc(allocator, path, 1 << 20);
    }
}

fn startDisk(
    allocator: std.mem.Allocator,
    disk_number: u8,
    output_path: [:0]u8,
    state: *DiskState,
) !void {
    output_path[output_path.len - 3] = '(';
    output_path[output_path.len - 2] = 'a' - 1 + disk_number;
    output_path[output_path.len - 1] = ')';

    state.disk_number = disk_number;

    state.file = try std.fs.cwd().createFileZ(output_path, .{});
    errdefer state.file.close();

    state.xor_writer = io.xorWriter(state.file.writer(), xor_key);
    state.buf_writer = std.io.bufferedWriter(state.xor_writer.writer());
    state.writer = std.io.countingWriter(state.buf_writer.writer());

    state.fixups = std.ArrayList(Fixup).init(allocator);
    errdefer state.fixups.deinit();

    // Hardcode the fixup pos since it's always the same
    const lecf_start = try beginBlock(&state.writer, "LECF");
    std.debug.assert(lecf_start == 0);
}

fn finishDisk(state: *DiskState) !void {
    // End the LECF block
    try endBlock(&state.writer, &state.fixups, 0);

    try state.buf_writer.flush();

    try writeFixups(state.file, state.xor_writer.writer(), state.fixups.items);
    state.fixups.deinit();

    state.file.close();
}

fn writeFixups(file: std.fs.File, writer: anytype, fixups: []const Fixup) !void {
    for (fixups) |fixup| {
        try file.seekTo(fixup.offset);
        try writer.writeInt(u32, fixup.value, .big);
    }
}

const RoomLine = union(enum) {
    raw_glob: struct {
        block_id: BlockId,
        glob_number: u32,
        block_path: []const u8,
    },
};

fn parseRoomLine(line: []const u8) !RoomLine {
    if (!std.mem.startsWith(u8, line, "raw-glob "))
        return error.BadData;
    var words = std.mem.splitScalar(u8, line[9..], ' ');
    const block_id_str = words.next() orelse return error.BadData;
    const glob_number_str = words.next() orelse return error.BadData;
    const block_path = words.next() orelse return error.BadData;
    if (words.next()) |_| return error.BadData;

    const block_id = parseBlockId(block_id_str) orelse return error.BadData;

    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    return .{ .raw_glob = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .block_path = block_path,
    } };
}

fn handleRawGlob(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: std.meta.FieldType(RoomLine, .raw_glob),
    cur_path: *std.BoundedArray(u8, 4095),
    state: *DiskState,
    index: *Index,
) !void {
    try cur_path.appendSlice(line.block_path);
    try cur_path.append(0);
    defer cur_path.len -= @intCast(line.block_path.len + 1);

    const block_path = cur_path.buffer[0 .. cur_path.buffer.len - 1 :0];

    const block_file = try std.fs.cwd().openFileZ(block_path, .{});
    defer block_file.close();

    const block_fixup = try beginBlockImpl(&state.writer, line.block_id);

    try io.copy(block_file, state.writer.writer());

    try endBlock(&state.writer, &state.fixups, block_fixup);
    const block_len = state.fixups.getLast().value;

    const directory = directoryForBlockId(&index.directories, line.block_id) orelse
        return error.BadData;
    try growMultiArrayList(DirectoryEntry, directory, allocator, line.glob_number + 1, .{
        .room = 0,
        .offset = 0,
        .len = 0,
    });
    const offset = block_fixup - index.lfl_offsets.items[room_number];
    directory.set(line.glob_number, .{
        .room = room_number,
        .offset = @intCast(offset),
        .len = block_len,
    });
}

fn writeIndex(allocator: std.mem.Allocator, index: *Index, output_path: [:0]u8) !void {
    output_path[output_path.len - 3] = 'h';
    output_path[output_path.len - 2] = 'e';
    output_path[output_path.len - 1] = '0';

    const file = try std.fs.cwd().createFileZ(output_path, .{});
    errdefer file.close();

    const xor_writer = io.xorWriter(file.writer(), xor_key);
    var buf_writer = std.io.bufferedWriter(xor_writer.writer());
    var writer = std.io.countingWriter(buf_writer.writer());

    var fixups = std.ArrayList(Fixup).init(allocator);
    defer fixups.deinit();

    const maxs_fixup = try beginBlock(&writer, "MAXS");
    try writer.writer().writeAll(index.maxs);
    try endBlock(&writer, &fixups, maxs_fixup);

    // SCUMM outputs sequential room numbers for these whether or not the room
    // actually exists.
    for (0.., index.directories.room_images.items(.room)) |i, *room|
        room.* = @intCast(i);
    for (0.., index.directories.rooms.items(.room)) |i, *room|
        room.* = @intCast(i);

    const directory_info = .{
        .{ "DIRI", .room_images },
        .{ "DIRR", .rooms },
        .{ "DIRS", .scripts },
        .{ "DIRN", .sounds },
        .{ "DIRC", .costumes },
        .{ "DIRF", .charsets },
        .{ "DIRM", .images },
        .{ "DIRT", .talkies },
    };
    inline for (directory_info) |info| {
        const block_id, const field = info;
        try writeDirectory(
            &writer,
            block_id,
            &@field(index.directories, @tagName(field)),
            &fixups,
        );
    }

    const dlfl_fixup = try beginBlock(&writer, "DLFL");
    try writer.writer().writeInt(u16, @intCast(index.lfl_offsets.items.len), .little);
    try writer.writer().writeAll(std.mem.sliceAsBytes(index.lfl_offsets.items));
    std.debug.assert(builtin.cpu.arch.endian() == .little);
    try endBlock(&writer, &fixups, dlfl_fixup);

    const disk_fixup = try beginBlock(&writer, "DISK");
    try writer.writer().writeInt(u16, @intCast(index.lfl_disks.items.len), .little);
    try writer.writer().writeAll(index.lfl_disks.items);
    std.debug.assert(builtin.cpu.arch.endian() == .little);
    try endBlock(&writer, &fixups, disk_fixup);

    const rnam_fixup = try beginBlock(&writer, "RNAM");
    for (0.., index.room_names.items) |num, name| {
        if (name.len == 0)
            continue;
        try writer.writer().writeInt(u16, @intCast(num), .little);
        // TODO: could you writeAll with a null-terminated name to save a write
        // call here?
        try writer.writer().writeAll(name);
        try writer.writer().writeByte(0);
    }
    try writer.writer().writeInt(u16, 0, .little); // terminator
    try endBlock(&writer, &fixups, rnam_fixup);

    const dobj_fixup = try beginBlock(&writer, "DOBJ");
    try writer.writer().writeAll(index.dobj);
    try endBlock(&writer, &fixups, dobj_fixup);

    const aary_fixup = try beginBlock(&writer, "AARY");
    try writer.writer().writeInt(u16, 0, .little);
    try endBlock(&writer, &fixups, aary_fixup);

    const inib_fixup = try beginBlock(&writer, "INIB");
    const note_fixup = try beginBlock(&writer, "NOTE");
    try writer.writer().writeInt(u16, 0, .little);
    try endBlock(&writer, &fixups, note_fixup);
    try endBlock(&writer, &fixups, inib_fixup);

    try buf_writer.flush();

    try writeFixups(file, xor_writer.writer(), fixups.items);
}

fn writeDirectory(
    stream: anytype,
    comptime block_id: []const u8,
    directory: *const std.MultiArrayList(DirectoryEntry),
    fixups: *std.ArrayList(Fixup),
) !void {
    const id = comptime blockId(block_id);
    return writeDirectoryImpl(stream, id, directory, fixups);
}

fn writeDirectoryImpl(
    stream: anytype,
    block_id: BlockId,
    directory: *const std.MultiArrayList(DirectoryEntry),
    fixups: *std.ArrayList(Fixup),
) !void {
    const block_fixup = try beginBlockImpl(stream, block_id);

    const slice = directory.slice();
    try stream.writer().writeInt(u16, @intCast(slice.len), .little);
    try stream.writer().writeAll(slice.items(.room));
    try stream.writer().writeAll(std.mem.sliceAsBytes(slice.items(.offset)));
    try stream.writer().writeAll(std.mem.sliceAsBytes(slice.items(.len)));
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    try endBlock(stream, fixups, block_fixup);
}

const DiskState = struct {
    disk_number: u8,
    file: std.fs.File,
    xor_writer: io.XorWriter(std.fs.File.Writer),
    buf_writer: std.io.BufferedWriter(4096, io.XorWriter(std.fs.File.Writer).Writer),
    writer: std.io.CountingWriter(std.io.BufferedWriter(4096, io.XorWriter(std.fs.File.Writer).Writer).Writer),
    fixups: std.ArrayList(Fixup),
};

const Fixup = struct {
    offset: u32,
    value: u32,
};

const Index = struct {
    maxs: []u8 = &.{},
    directories: Directories = .{},
    lfl_offsets: std.ArrayListUnmanaged(u32) = .{},
    lfl_disks: std.ArrayListUnmanaged(u8) = .{},
    room_names: std.ArrayListUnmanaged([]u8) = .{},
    dobj: []u8 = &.{},

    fn deinit(self: *Index, allocator: std.mem.Allocator) void {
        allocator.free(self.dobj);

        var i = self.room_names.items.len;
        while (i > 0) {
            i -= 1;
            const room_name = self.room_names.items[i];
            allocator.free(room_name);
        }
        self.room_names.deinit(allocator);

        self.lfl_disks.deinit(allocator);
        self.lfl_offsets.deinit(allocator);
        self.directories.deinit(allocator);
        allocator.free(self.maxs);
    }
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

    fn deinit(self: *Directories, allocator: std.mem.Allocator) void {
        self.talkies.deinit(allocator);
        self.images.deinit(allocator);
        self.charsets.deinit(allocator);
        self.costumes.deinit(allocator);
        self.sounds.deinit(allocator);
        self.scripts.deinit(allocator);
        self.rooms.deinit(allocator);
        self.room_images.deinit(allocator);
    }
};

const DirectoryEntry = struct {
    room: u8,
    offset: u32,
    len: u32,
};

// TODO: this is duplicated
fn directoryForBlockId(
    directories: *Directories,
    block_id: BlockId,
) ?*std.MultiArrayList(DirectoryEntry) {
    return switch (block_id) {
        blockId("RMIM") => &directories.room_images,
        blockId("RMDA") => &directories.rooms,
        blockId("SCRP") => &directories.scripts,
        blockId("DIGI"), blockId("TALK") => &directories.sounds,
        blockId("AKOS") => &directories.costumes,
        blockId("CHAR") => &directories.charsets,
        blockId("AWIZ"), blockId("MULT") => &directories.images,
        blockId("TLKE") => &directories.talkies,
        else => null,
    };
}

fn beginBlock(stream: anytype, comptime block_id: []const u8) !u32 {
    const id = comptime blockId(block_id);
    return beginBlockImpl(stream, id);
}

fn beginBlockImpl(stream: anytype, id: BlockId) !u32 {
    const block_start: u32 = @intCast(stream.bytes_written);

    try stream.writer().writeInt(BlockId, id, .little);
    // Write the length as a placeholder to be filled in later
    try stream.writer().writeAll(&@as([4]u8, undefined));

    return block_start;
}

fn endBlock(stream: anytype, fixups: *std.ArrayList(Fixup), block_start: u32) !void {
    const stream_pos: u32 = @intCast(stream.bytes_written);
    try fixups.append(.{
        .offset = block_start + 4,
        .value = stream_pos - block_start,
    });
}

fn growArrayList(
    T: type,
    xs: *std.ArrayListUnmanaged(T),
    allocator: std.mem.Allocator,
    minimum_len: usize,
    fill: T,
) !void {
    if (xs.items.len >= minimum_len)
        return;

    try xs.ensureTotalCapacity(allocator, minimum_len);
    @memset(xs.allocatedSlice()[xs.items.len..minimum_len], fill);
    xs.items.len = minimum_len;
}

fn growMultiArrayList(
    T: type,
    xs: *std.MultiArrayList(T),
    allocator: std.mem.Allocator,
    minimum_len: usize,
    fill: T,
) !void {
    if (xs.len >= minimum_len)
        return;

    // XXX: This could be more efficient by setting each field array all at once.
    try xs.ensureTotalCapacity(allocator, minimum_len);
    while (xs.len < minimum_len)
        xs.appendAssumeCapacity(fill);
}

fn popPathFile(str: *std.BoundedArray(u8, 4095)) void {
    const slash = std.mem.lastIndexOfScalar(u8, str.slice(), '/');
    str.len = if (slash) |s| @intCast(s + 1) else 0;
}
