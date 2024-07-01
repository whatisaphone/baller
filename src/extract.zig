const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const blockReader = @import("block_reader.zig").blockReader;
const xor_key = @import("build.zig").xor_key;
const fs = @import("fs.zig");
const io = @import("io.zig");

pub fn runCli(allocator: std.mem.Allocator) !void {
    if (std.os.argv.len != 1 + 1 + 2)
        return error.CommandLine;

    const input_path = std.mem.sliceTo(std.os.argv[2], 0);
    const output_path = std.mem.sliceTo(std.os.argv[3], 0);

    try run(allocator, &.{
        .input_path = input_path,
        .output_path = output_path,
    });
}

const Extract = struct {
    input_path: [:0]const u8,
    output_path: [:0]const u8,
};

pub fn run(allocator: std.mem.Allocator, args: *const Extract) !void {
    var input_path_buf = std.BoundedArray(u8, 4095){};
    try input_path_buf.appendSlice(args.input_path);
    try input_path_buf.append(0);
    const input_path = input_path_buf.buffer[0 .. input_path_buf.len - 1 :0];

    const output_path = args.output_path;

    if (!std.mem.endsWith(u8, input_path, ".he0"))
        return error.CommandLine;

    try fs.makeDirIfNotExistZ(std.fs.cwd(), output_path);

    var index = try readIndex(allocator, input_path);
    defer index.deinit(allocator);

    var path_buf = std.BoundedArray(u8, 4095){};
    try path_buf.appendSlice(output_path);
    try path_buf.append('/');

    try dumpIndexBlobs(&index, &path_buf);

    const project_txt_path = try std.fmt.allocPrintZ(
        allocator,
        "{s}/project.txt",
        .{output_path},
    );
    defer allocator.free(project_txt_path);

    const project_txt_file = try std.fs.cwd().createFileZ(project_txt_path, .{});
    defer project_txt_file.close();

    var project_txt = std.io.bufferedWriter(project_txt_file.writer());

    // input_path will be modified to point to each disk file
    // e.g. "baseball 2001.(a)"
    input_path[input_path.len - 3] = '(';
    input_path[input_path.len - 1] = ')';

    for (1..2 + 1) |disk_number_usize| {
        const disk_number: u8 = @intCast(disk_number_usize);
        input_path[input_path.len - 2] = 'a' - 1 + disk_number;

        try extractDisk(
            allocator,
            input_path,
            output_path,
            disk_number,
            &project_txt,
            &index,
        );
    }

    try project_txt.flush();
}

const State = struct {
    cur_path: std.BoundedArray(u8, 4095) = .{},

    fn init(output_path: []const u8) !State {
        var result = State{
            .cur_path = .{},
        };
        try result.cur_path.appendSlice(output_path);
        try result.cur_path.append('/');
        return result;
    }
};

const Index = struct {
    maxs: *Maxs,
    directories: Directories,
    lfl_offsets: []u32,
    lfl_disks: []u8,
    room_name_buf: []u8,
    room_name_starts: []u16,
    room_name_lens: []u8,
    dobj: []u8,

    fn deinit(self: *Index, allocator: std.mem.Allocator) void {
        allocator.free(self.dobj);
        allocator.free(self.room_name_lens);
        allocator.free(self.room_name_starts);
        allocator.free(self.room_name_buf);
        allocator.free(self.lfl_disks);
        allocator.free(self.lfl_offsets);
        self.directories.deinit(allocator);
        allocator.destroy(self.maxs);
    }

    fn roomName(self: *const Index, room_number: u8) ![]const u8 {
        const room_index = room_number - 1;
        if (room_index >= self.room_name_starts.len)
            return error.NotFound;

        const len = self.room_name_lens[room_index];
        // Missing room name is indicated with len == 0
        if (len == 0)
            return error.NotFound;
        const start = self.room_name_starts[room_index];
        return self.room_name_buf[start .. start + len];
    }
};

const Directories = struct {
    room_images: std.MultiArrayList(DirectoryEntry),
    rooms: std.MultiArrayList(DirectoryEntry),
    scripts: std.MultiArrayList(DirectoryEntry),
    sounds: std.MultiArrayList(DirectoryEntry),
    costumes: std.MultiArrayList(DirectoryEntry),
    charsets: std.MultiArrayList(DirectoryEntry),
    images: std.MultiArrayList(DirectoryEntry),
    talkies: std.MultiArrayList(DirectoryEntry),

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

const Maxs = extern struct {
    variables: u16,
    unknown1: u16,
    room_variables: u16,
    objects_in_room: u16,
    arrays: u16,
    unknown2: u16,
    unknown3: u16,
    flobjects: u16,
    inventory_objects: u16,
    rooms: u16,
    scripts: u16,
    sounds: u16,
    charsets: u16,
    costumes: u16,
    objects: u16,
    images: u16,
    sprites: u16,
    local_scripts: u16,
    heap: u16,
    palettes: u16,
    unknown4: u16,
    talkies: u16,
};

const DirectoryEntry = struct {
    const packed_size = 1 + 4 + 4;

    room: u8,
    offset: u32,
    len: u32,
};

fn readIndex(allocator: std.mem.Allocator, path: [*:0]u8) !Index {
    const file = try std.fs.cwd().openFileZ(path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);
    const in = reader.reader();

    var blocks = blockReader(&reader);

    // MAXS

    const maxs_len = try blocks.expectBlock("MAXS");
    if (maxs_len != @sizeOf(Maxs))
        return error.BadData;
    const maxs = try allocator.create(Maxs);
    errdefer allocator.destroy(maxs);
    try in.readNoEof(std.mem.asBytes(maxs));
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    // DIR*

    const directory_info = .{
        .{ "DIRI", .rooms, .room_images },
        .{ "DIRR", .rooms, .rooms },
        .{ "DIRS", .scripts, .scripts },
        .{ "DIRN", .sounds, .sounds },
        .{ "DIRC", .costumes, .costumes },
        .{ "DIRF", .charsets, .charsets },
        .{ "DIRM", .images, .images },
        .{ "DIRT", .talkies, .talkies },
    };

    var directories: Directories = undefined;

    inline for (directory_info) |info| {
        const block_id, const maxs_field, const directories_field = info;
        @field(directories, @tagName(directories_field)) = try readDirectory(
            allocator,
            in,
            &blocks,
            comptime blockId(block_id),
            @field(maxs, @tagName(maxs_field)),
        );
    }

    // DLFL

    const dlfl_len = try blocks.expectBlock("DLFL");
    if (dlfl_len != 2 + maxs.rooms * 4)
        return error.BadData;
    const dlfl_count = try in.readInt(u16, .little);
    if (dlfl_count != maxs.rooms)
        return error.BadData;

    const dlfl = try allocator.alloc(u32, dlfl_count);
    errdefer allocator.free(dlfl);

    try in.readNoEof(std.mem.sliceAsBytes(dlfl));
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    // DISK

    const disk_len = try blocks.expectBlock("DISK");
    if (disk_len != 2 + maxs.rooms * 1)
        return error.BadData;
    const disk_count = try in.readInt(u16, .little);
    if (disk_count != maxs.rooms)
        return error.BadData;

    const disk = try allocator.alloc(u8, disk_count);
    errdefer allocator.free(disk);

    try in.readNoEof(disk);

    // RNAM

    const rnam_len = try blocks.expectBlock("RNAM");

    var room_name_buf = try allocator.alloc(u8, rnam_len);
    errdefer allocator.free(room_name_buf);

    var room_name_starts = try allocator.alloc(u16, maxs.rooms);
    errdefer allocator.free(room_name_starts);

    var room_name_lens = try allocator.alloc(u8, maxs.rooms);
    errdefer allocator.free(room_name_lens);

    var room_name_buf_pos: u16 = 0;
    // Missing room name is indicated with len == 0
    @memset(room_name_lens, 0);

    while (true) {
        const room_number = try in.readInt(u16, .little);
        if (room_number == 0) // terminator
            break;

        const room_index_u16 = try std.math.sub(u16, room_number, 1);
        const room_index = std.math.cast(u8, room_index_u16) orelse
            return error.Overflow;

        room_name_starts[room_index] = room_name_buf_pos;

        while (true) {
            const n = try in.readByte();
            if (n == 0) // null terminated
                break;
            if (room_name_buf_pos >= room_name_buf.len)
                return error.BadData;
            room_name_buf[room_name_buf_pos] = n;
            room_name_buf_pos += 1;
        }

        const name_len = room_name_buf_pos - room_name_starts[room_index];
        room_name_lens[room_index] = std.math.cast(u8, name_len) orelse
            return error.Overflow;
    }

    // DOBJ

    const dobj_len = try blocks.expectBlock("DOBJ");

    const dobj = try allocator.alloc(u8, dobj_len);
    errdefer allocator.free(dobj);

    try in.readNoEof(dobj);

    // AARY

    const aary_len = try blocks.expectBlock("AARY");
    if (aary_len != 2)
        return error.BadData;
    if (try in.readInt(u16, .little) != 0)
        return error.BadData;

    // INIB

    const inib_len = try blocks.expectBlock("INIB");
    if (inib_len < 8)
        return error.BadData;

    var inib_blocks = blockReader(&reader);

    const note_len = try inib_blocks.expectBlock("NOTE");
    if (note_len != 2)
        return error.BadData;
    if (try in.readInt(u16, .little) != 0)
        return error.BadData;

    try inib_blocks.checkSync();

    try blocks.checkSync();

    // Phew, we made it.

    try io.requireEof(reader.reader());

    return Index{
        .maxs = maxs,
        .directories = directories,
        .lfl_offsets = dlfl,
        .lfl_disks = disk,
        .room_name_buf = room_name_buf,
        .room_name_starts = room_name_starts,
        .room_name_lens = room_name_lens,
        .dobj = dobj,
    };
}

fn readDirectory(
    allocator: std.mem.Allocator,
    in: anytype,
    blocks: anytype,
    block_id: BlockId,
    expected_count: u16,
) !std.MultiArrayList(DirectoryEntry) {
    const len = try blocks.expect(block_id);
    if (len != 2 + expected_count * DirectoryEntry.packed_size)
        return error.BadData;
    const count = try in.readInt(u16, .little);
    if (count != expected_count)
        return error.BadData;

    var result = std.MultiArrayList(DirectoryEntry){};
    try result.setCapacity(allocator, count);
    result.len = count;

    const slice = result.slice();
    try in.readNoEof(slice.items(.room));
    try in.readNoEof(std.mem.sliceAsBytes(slice.items(.offset)));
    try in.readNoEof(std.mem.sliceAsBytes(slice.items(.len)));
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    return result;
}

// Let's see how long we can get away with this.
fn dumpIndexBlobs(index: *const Index, path_buf: *std.BoundedArray(u8, 4095)) !void {
    {
        try path_buf.appendSlice("maxs.bin\x00");
        defer path_buf.len -= 9;

        const path = path_buf.buffer[0 .. path_buf.len - 1 :0];
        const file = try std.fs.cwd().createFileZ(path, .{});
        defer file.close();

        try file.writeAll(std.mem.asBytes(index.maxs));
        std.debug.assert(builtin.cpu.arch.endian() == .little);
    }

    {
        try path_buf.appendSlice("dobj.bin\x00");
        defer path_buf.len -= 9;

        const path = path_buf.buffer[0 .. path_buf.len - 1 :0];
        const file = try std.fs.cwd().createFileZ(path, .{});
        defer file.close();

        try file.writeAll(index.dobj);
    }
}

fn extractDisk(
    allocator: std.mem.Allocator,
    input_path: [*:0]u8,
    output_path: []const u8,
    disk_number: u8,
    project_txt: anytype,
    index: *const Index,
) !void {
    const file = try std.fs.cwd().openFileZ(input_path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);

    var state = try State.init(output_path);

    var file_blocks = blockReader(&reader);

    const lecf_len = try file_blocks.expectBlock("LECF");
    const lecf_end = reader.bytes_read + lecf_len;

    var lecf_blocks = blockReader(&reader);

    while (reader.bytes_read < lecf_end) {
        const lflf_len = try lecf_blocks.expectBlock("LFLF");
        const lflf_end = reader.bytes_read + lflf_len;

        const room_number =
            for (0.., index.lfl_disks, index.lfl_offsets) |i, disk, offset|
        {
            if (disk == disk_number and offset == reader.bytes_read)
                break @as(u8, @intCast(i));
        } else return error.BadData;

        const room_name = index.roomName(room_number) catch
            return error.BadData;

        try project_txt.writer().print("room {} {} {s}\n", .{ disk_number, room_number, room_name });

        const before_room_path_len = state.cur_path.len;
        defer state.cur_path.len = before_room_path_len;

        try state.cur_path.appendSlice(room_name);
        try state.cur_path.append('\x00');
        const room_dir_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];
        try fs.makeDirIfNotExistZ(std.fs.cwd(), room_dir_path);
        state.cur_path.buffer[state.cur_path.len - 1] = '/';

        const room_txt_file = room_txt_file: {
            try state.cur_path.appendSlice("room.txt\x00");
            defer state.cur_path.len -= 9;

            const room_txt_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];
            break :room_txt_file try std.fs.cwd().createFileZ(room_txt_path, .{});
        };
        defer room_txt_file.close();

        var room_txt = std.io.bufferedWriter(room_txt_file.writer());

        var lflf_blocks = blockReader(&reader);

        const rmim_offset, const rmim_data =
            try readGlob(allocator, &lflf_blocks, comptime blockId("RMIM"), &reader);
        defer allocator.free(rmim_data);

        const rmda_offset, const rmda_data =
            try readGlob(allocator, &lflf_blocks, comptime blockId("RMDA"), &reader);
        defer allocator.free(rmda_data);

        try writeGlob(
            disk_number,
            comptime blockId("RMIM"),
            rmim_offset,
            rmim_data,
            index,
            &state,
            &room_txt,
        );

        try writeGlob(
            disk_number,
            comptime blockId("RMDA"),
            rmda_offset,
            rmda_data,
            index,
            &state,
            &room_txt,
        );

        while (reader.bytes_read < lflf_end) {
            const id, const len = try lflf_blocks.next();
            try extractGlob(disk_number, id, len, &reader, index, &state, &room_txt);
        }

        try lflf_blocks.checkSync();

        try room_txt.flush();
    }

    try lecf_blocks.checkSync();

    try file_blocks.checkSync();

    try io.requireEof(reader.reader());
}

fn readGlob(
    allocator: std.mem.Allocator,
    blocks: anytype,
    block_id: BlockId,
    reader: anytype,
) !struct { u32, []u8 } {
    const offset = reader.bytes_read;
    const block_len = try blocks.expect(block_id);
    const data = try allocator.alloc(u8, block_len);
    errdefer allocator.free(data);
    try reader.reader().readNoEof(data);
    return .{ @intCast(offset), data };
}

fn extractGlob(
    disk_number: u8,
    id: BlockId,
    len: u32,
    reader: anytype,
    index: *const Index,
    state: *State,
    room_txt: anytype,
) !void {
    const glob_number = try findGlobNumber(
        index,
        id,
        disk_number,
        @intCast(reader.bytes_read - 8),
    ) orelse return error.BadData;

    const before_child_path_len = state.cur_path.len;
    defer state.cur_path.len = before_child_path_len;

    try state.cur_path.writer().print(
        "{s}_{:0>4}.bin\x00",
        .{ blockIdToStr(&id), glob_number },
    );
    const cur_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];

    try room_txt.writer().print(
        "raw-glob {s} {} {s}\n",
        .{ blockIdToStr(&id), glob_number, cur_path[before_child_path_len..] },
    );

    const output_file = try std.fs.cwd().createFileZ(cur_path, .{});
    defer output_file.close();

    try io.copy(std.io.limitedReader(reader.reader(), len), output_file);
}

// TODO: this is mostly a copy/paste
fn writeGlob(
    disk_number: u8,
    block_id: BlockId,
    block_offset: u32,
    data: []const u8,
    index: *const Index,
    state: *State,
    room_txt: anytype,
) !void {
    const glob_number = try findGlobNumber(
        index,
        block_id,
        disk_number,
        block_offset,
    ) orelse return error.BadData;

    const before_child_path_len = state.cur_path.len;
    defer state.cur_path.len = before_child_path_len;

    try state.cur_path.writer().print(
        "{s}_{:0>4}.bin\x00",
        .{ blockIdToStr(&block_id), glob_number },
    );
    const cur_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];

    try room_txt.writer().print(
        "raw-glob {s} {} {s}\n",
        .{ blockIdToStr(&block_id), glob_number, cur_path[before_child_path_len..] },
    );

    const output_file = try std.fs.cwd().createFileZ(cur_path, .{});
    defer output_file.close();

    try output_file.writeAll(data);
}

fn findGlobNumber(
    index: *const Index,
    block_id: BlockId,
    disk_number: u8,
    offset: u32,
) !?u32 {
    const directory = directoryForBlockId(&index.directories, block_id) orelse
        return null;
    const slice = directory.slice();
    for (0..slice.len) |i| {
        const room = slice.items(.room)[i];
        const d = index.lfl_disks[room];
        const o = index.lfl_offsets[room] + slice.items(.offset)[i];
        if (d == disk_number and o == offset)
            return @intCast(i);
    }
    return error.BadData;
}

fn directoryForBlockId(
    directories: *const Directories,
    block_id: BlockId,
) ?*const std.MultiArrayList(DirectoryEntry) {
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
