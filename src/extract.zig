const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const blockReader = @import("block_reader.zig").blockReader;
const xor_key = @import("build.zig").xor_key;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const report = @import("report.zig");
const rmim = @import("rmim.zig");

pub fn runCli(allocator: std.mem.Allocator) !void {
    if (std.os.argv.len != 1 + 1 + 2)
        return error.CommandLine;

    const input_path = std.mem.sliceTo(std.os.argv[2], 0);
    const output_path = std.mem.sliceTo(std.os.argv[3], 0);

    try run(allocator, &.{
        .input_path = input_path,
        .output_path = output_path,
        .raw = false,
    });
}

const Extract = struct {
    input_path: [:0]const u8,
    output_path: [:0]const u8,
    raw: bool,
};

pub fn run(allocator: std.mem.Allocator, args: *const Extract) !void {
    var input_path_buf = std.BoundedArray(u8, 4095){};
    try input_path_buf.appendSlice(args.input_path);
    try input_path_buf.append(0);
    const input_path = input_path_buf.buffer[0 .. input_path_buf.len - 1 :0];

    const output_path = args.output_path;

    const game = try games.detectGameOrFatal(input_path);

    try fs.makeDirIfNotExistZ(std.fs.cwd(), output_path);

    var index = try readIndex(allocator, game, input_path);
    defer index.deinit(allocator);

    var path_buf = std.BoundedArray(u8, 4095){};
    try path_buf.appendSlice(output_path);
    try path_buf.append('/');

    try dumpIndexBlobs(game, &index, &path_buf);

    const project_txt_path = try std.fmt.allocPrintZ(
        allocator,
        "{s}/project.txt",
        .{output_path},
    );
    defer allocator.free(project_txt_path);

    const project_txt_file = try std.fs.cwd().createFileZ(project_txt_path, .{});
    defer project_txt_file.close();

    var project_txt = std.io.bufferedWriter(project_txt_file.writer());

    for (1..1 + games.numberOfDisks(game)) |disk_number_usize| {
        const disk_number: u8 = @intCast(disk_number_usize);

        games.pointPathToDisk(game, input_path, disk_number);

        try extractDisk(
            allocator,
            input_path,
            output_path,
            game,
            disk_number,
            &project_txt,
            &index,
            args.raw,
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
    lfl_disks: ?[]u8,
    room_name_buf: []u8,
    room_name_starts: []u16,
    room_name_lens: []u8,
    dobj: []u8,
    aary: []u8,

    fn deinit(self: *Index, allocator: std.mem.Allocator) void {
        allocator.free(self.aary);
        allocator.free(self.dobj);
        allocator.free(self.room_name_lens);
        allocator.free(self.room_name_starts);
        allocator.free(self.room_name_buf);
        if (self.lfl_disks) |d|
            allocator.free(d);
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

fn readIndex(allocator: std.mem.Allocator, game: games.Game, path: [*:0]u8) !Index {
    const file = try std.fs.cwd().openFileZ(path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);
    const in = reader.reader();

    var blocks = blockReader(&reader);

    // MAXS

    const maxs_len = try blocks.expectBlock("MAXS");
    if (maxs_len != games.maxsLen(game))
        return error.BadData;
    const maxs = try allocator.create(Maxs);
    errdefer allocator.destroy(maxs);

    // TODO: Figure out the layout of MAXS fields in other games. This is a huge
    // hack, I'm not sure the fields are a strict prefix.
    try in.readNoEof(std.mem.asBytes(maxs)[0..maxs_len]);
    @memset(std.mem.asBytes(maxs)[maxs_len..@sizeOf(Maxs)], 0);

    std.debug.assert(builtin.cpu.arch.endian() == .little);

    // DIR*

    const read_dir = struct {
        allocator: std.mem.Allocator,
        in: @TypeOf(in),
        blocks: *@TypeOf(blocks),

        fn call(
            self: *const @This(),
            comptime block_id: []const u8,
            expected_count: u16,
        ) !std.MultiArrayList(DirectoryEntry) {
            return readDirectory(
                self.allocator,
                self.in,
                self.blocks,
                comptime blockId(block_id),
                expected_count,
            );
        }
    }{ .allocator = allocator, .in = in, .blocks = &blocks };

    var directories: Directories = .{};
    errdefer directories.deinit(allocator);

    directories.room_images = try read_dir.call("DIRI", maxs.rooms);
    directories.rooms = try read_dir.call("DIRR", maxs.rooms);
    directories.scripts = try read_dir.call("DIRS", maxs.scripts);
    directories.sounds = try read_dir.call("DIRN", maxs.sounds);
    directories.costumes = try read_dir.call("DIRC", maxs.costumes);
    directories.charsets = try read_dir.call("DIRF", maxs.charsets);
    directories.images = try read_dir.call("DIRM", maxs.images);
    if (games.hasTalkies(game))
        directories.talkies = try read_dir.call("DIRT", maxs.talkies);

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

    const disk = if (games.hasDisk(game)) disk: {
        const disk_len = try blocks.expectBlock("DISK");
        if (disk_len != 2 + maxs.rooms * 1)
            return error.BadData;
        const disk_count = try in.readInt(u16, .little);
        if (disk_count != maxs.rooms)
            return error.BadData;

        const disk = try allocator.alloc(u8, disk_count);
        errdefer allocator.free(disk);

        try in.readNoEof(disk);
        break :disk disk;
    } else null;
    errdefer if (disk) |d|
        allocator.free(d);

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

    const aary = try allocator.alloc(u8, aary_len);
    errdefer allocator.free(aary);

    try in.readNoEof(aary);

    // INIB

    if (games.hasIndexInib(game)) {
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
    }

    // Phew, we made it.

    try blocks.checkSync();
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
        .aary = aary,
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
fn dumpIndexBlobs(
    game: games.Game,
    index: *const Index,
    path_buf: *std.BoundedArray(u8, 4095),
) !void {
    {
        try path_buf.appendSlice("maxs.bin\x00");
        defer path_buf.len -= 9;

        const path = path_buf.buffer[0 .. path_buf.len - 1 :0];
        const file = try std.fs.cwd().createFileZ(path, .{});
        defer file.close();

        try file.writeAll(std.mem.asBytes(index.maxs)[0..games.maxsLen(game)]);
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

    {
        try path_buf.appendSlice("aary.bin\x00");
        defer path_buf.len -= 9;

        const path = path_buf.buffer[0 .. path_buf.len - 1 :0];
        const file = try std.fs.cwd().createFileZ(path, .{});
        defer file.close();

        try file.writeAll(index.aary);
    }
}

fn extractDisk(
    allocator: std.mem.Allocator,
    input_path: [*:0]u8,
    output_path: []const u8,
    game: games.Game,
    disk_number: u8,
    project_txt: anytype,
    index: *const Index,
    raw: bool,
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

        const room_number = try findLflfRoomNumber(
            game,
            index,
            disk_number,
            @intCast(reader.bytes_read),
        );

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

        const rmim_decoded = rmim_decoded: {
            if (raw)
                break :rmim_decoded false;
            decodeRmim(allocator, rmim_data, rmda_data, &state, &room_txt) catch |err| {
                if (err != error.DecompressBmap)
                    return err;
                report.warn("could not decode {s} RMIM", .{room_name});
                break :rmim_decoded false;
            };
            break :rmim_decoded true;
        };
        if (!rmim_decoded)
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

fn findLflfRoomNumber(
    game: games.Game,
    index: *const Index,
    disk_number: u8,
    lflf_data_offset: u32,
) !u8 {
    if (!games.hasDisk(game)) {
        std.debug.assert(disk_number == 1);
        for (0.., index.lfl_offsets) |i, offset| {
            if (offset == lflf_data_offset)
                return @intCast(i);
        }
    } else {
        for (0.., index.lfl_disks.?, index.lfl_offsets) |i, disk, offset| {
            if (disk == disk_number and offset == lflf_data_offset)
                return @intCast(i);
        }
    }
    return error.BadData;
}

fn decodeRmim(
    allocator: std.mem.Allocator,
    rmim_raw: []const u8,
    rmda_raw: []const u8,
    state: *State,
    room_txt: anytype,
) !void {
    var bmp = try rmim.decode(allocator, rmim_raw, rmda_raw);
    defer bmp.deinit(allocator);

    const before_child_path_len = state.cur_path.len;
    defer state.cur_path.len = before_child_path_len;

    try state.cur_path.appendSlice("RMIM.bmp\x00");
    const cur_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];

    const output_file = try std.fs.cwd().createFileZ(cur_path, .{});
    defer output_file.close();

    try output_file.writeAll(bmp.items);

    try room_txt.writer().print(
        "room-image {s}\n",
        .{cur_path[before_child_path_len..]},
    );
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
    // Loop through the directory looking for an entry at the given offset.
    const directory = directoryForBlockId(&index.directories, block_id) orelse
        return null;
    const slice = directory.slice();
    for (0..slice.len) |i| {
        const room = slice.items(.room)[i];

        // If the game uses disks, require the disk number to match.
        if (index.lfl_disks) |lfl_disks| {
            if (lfl_disks[room] != disk_number)
                continue;
        }

        // Require the offset to match.
        if (index.lfl_offsets[room] + slice.items(.offset)[i] != offset)
            continue;

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
        blockId("DIGI"), blockId("SOUN"), blockId("TALK") => &directories.sounds,
        blockId("AKOS") => &directories.costumes,
        blockId("CHAR") => &directories.charsets,
        blockId("AWIZ"), blockId("MULT") => &directories.images,
        blockId("TLKE") => &directories.talkies,
        else => null,
    };
}
