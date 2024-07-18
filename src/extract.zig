const builtin = @import("builtin");
const std = @import("std");

const audio = @import("audio.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const blockReader = @import("block_reader.zig").blockReader;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const xor_key = @import("build.zig").xor_key;
const disasm = @import("disasm.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const report = @import("report.zig");
const pathf = @import("pathf.zig");
const rmim = @import("rmim.zig");

pub fn runCli(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len != 2)
        return error.CommandLine;

    const input_path = args[0];
    const output_path = args[1];

    try run(allocator, &.{
        .input_path = input_path,
        .output_path = output_path,
        .rmim_decode = true,
        .script_modes = &.{ .decode, .raw },
        .sound_modes = &.{ .decode, .raw },
        .awiz_modes = &.{ .decode, .raw },
        .mult_modes = &.{ .decode, .raw },
    });
}

const ResourceMode = enum {
    raw,
    decode,
};

const Extract = struct {
    input_path: [:0]const u8,
    output_path: [:0]const u8,
    rmim_decode: bool,
    script_modes: []const ResourceMode,
    sound_modes: []const ResourceMode,
    awiz_modes: []const ResourceMode,
    mult_modes: []const ResourceMode,
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
            args.rmim_decode,
            args.script_modes,
            args.sound_modes,
            args.awiz_modes,
            args.mult_modes,
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

const RoomState = struct {
    path: *std.BoundedArray(u8, 4095),
    path_start: u32,
    room_txt: std.io.BufferedWriter(4096, std.fs.File.Writer).Writer,

    fn curPathRelative(self: *const RoomState) [:0]const u8 {
        return self.path.buffer[self.path_start..self.path.len :0];
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
        const inib_end: u32 = @intCast(reader.bytes_read + inib_len);
        if (inib_len < 8)
            return error.BadData;
        var inib_blocks = blockReader(&reader);

        const note_len = try inib_blocks.expectBlock("NOTE");
        if (note_len != 2)
            return error.BadData;
        if (try in.readInt(u16, .little) != 0)
            return error.BadData;

        try inib_blocks.finish(inib_end);
    }

    // Phew, we made it.

    try blocks.finishEof();

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
    rmim_decode: bool,
    script_modes: []const ResourceMode,
    sound_modes: []const ResourceMode,
    awiz_modes: []const ResourceMode,
    mult_modes: []const ResourceMode,
) !void {
    const file = try std.fs.cwd().openFileZ(input_path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);

    var state = try State.init(output_path);

    var file_blocks = blockReader(&reader);

    const lecf_len = try file_blocks.expectBlock("LECF");
    const lecf_end: u32 = @intCast(reader.bytes_read + lecf_len);

    var lecf_blocks = blockReader(&reader);

    while (reader.bytes_read < lecf_end) {
        const lflf_len = try lecf_blocks.expectBlock("LFLF");
        const lflf_end: u32 = @intCast(reader.bytes_read + lflf_len);

        const room_number = try findLflfRoomNumber(
            game,
            index,
            disk_number,
            @intCast(reader.bytes_read),
        );

        const room_name = index.roomName(room_number) catch
            return error.BadData;

        try project_txt.writer().print("room {} {} {s}\n", .{ disk_number, room_number, room_name });

        const room_path = try pathf.print(&state.cur_path, "{s}/", .{room_name});
        defer room_path.restore();

        try fs.makeDirIfNotExistZ(std.fs.cwd(), room_path.full());

        const room_txt_file = room_txt_file: {
            try state.cur_path.appendSlice("room.txt\x00");
            defer state.cur_path.len -= 9;

            const room_txt_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];
            break :room_txt_file try std.fs.cwd().createFileZ(room_txt_path, .{});
        };
        defer room_txt_file.close();

        var room_txt = std.io.bufferedWriter(room_txt_file.writer());

        const room_state = RoomState{
            .path = &state.cur_path,
            .path_start = state.cur_path.len,
            .room_txt = room_txt.writer(),
        };

        var lflf_blocks = blockReader(&reader);

        const rmim_data =
            try readGlob(allocator, &lflf_blocks, comptime blockId("RMIM"), &reader);
        defer allocator.free(rmim_data);

        const rmda_data =
            try readGlob(allocator, &lflf_blocks, comptime blockId("RMDA"), &reader);
        defer allocator.free(rmda_data);

        const rmim_decoded = rmim_decoded: {
            if (!rmim_decode)
                break :rmim_decoded false;
            decodeRmim(allocator, rmim_data, rmda_data, &state, &room_state) catch |err| {
                if (err != error.DecompressBmap)
                    return err;
                break :rmim_decoded false;
            };
            break :rmim_decoded true;
        };
        if (!rmim_decoded)
            try writeGlob(
                comptime blockId("RMIM"),
                room_number,
                rmim_data,
                &state,
                &room_state,
            );

        try extractRmda(allocator, rmda_data, script_modes, &state, &room_state);

        while (reader.bytes_read < lflf_end) {
            const offset: u32 = @intCast(reader.bytes_read);
            const id, const len = try lflf_blocks.next();

            const glob_number = try findGlobNumber(index, id, disk_number, offset);

            const data = try allocator.alloc(u8, len);
            defer allocator.free(data);
            try reader.reader().readNoEof(data);

            const modes = switch (id) {
                blockId("SCRP") => script_modes,
                blockId("DIGI"), blockId("TALK") => sound_modes,
                blockId("AWIZ") => awiz_modes,
                blockId("MULT") => mult_modes,
                else => &.{ResourceMode.raw},
            };
            try extractGlob(
                allocator,
                id,
                glob_number,
                data,
                modes,
                rmda_data,
                &state,
                &room_state,
            );
        }

        try lflf_blocks.finish(lflf_end);

        try room_txt.flush();
    }

    try lecf_blocks.finish(lecf_end);

    try file_blocks.finishEof();
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

fn extractRmda(
    allocator: std.mem.Allocator,
    rmda_raw: []const u8,
    script_modes: []const ResourceMode,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try pathf.print(&state.cur_path, "RMDA/", .{});
    defer path.restore();

    try fs.makeDirIfNotExistZ(std.fs.cwd(), path.full());

    var reader = std.io.fixedBufferStream(rmda_raw);
    var blocks = fixedBlockReader(&reader);

    var block_numbers = std.AutoArrayHashMapUnmanaged(BlockId, u16){};
    defer block_numbers.deinit(allocator);

    try room_state.room_txt.writeAll("rmda\n");

    while (reader.pos < rmda_raw.len) {
        const block_id, const block_len = try blocks.next();
        const block_raw = try io.readInPlace(&reader, block_len);

        const block_number_entry = try block_numbers.getOrPutValue(allocator, block_id, 0);
        block_number_entry.value_ptr.* += 1;
        const block_number = block_number_entry.value_ptr.*;

        const modes = switch (block_id) {
            blockId("LSCR"), blockId("LSC2") => script_modes,
            else => &.{ResourceMode.raw},
        };
        try extractRmdaBlock(
            allocator,
            block_id,
            block_number,
            block_raw,
            modes,
            state,
            room_state,
        );
    }

    try blocks.finishEof();

    try room_state.room_txt.writeAll("end-rmda\n");
}

fn extractRmdaBlock(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_seq: u32,
    data: []const u8,
    modes: []const ResourceMode,
    state: *State,
    room_state: *const RoomState,
) !void {
    var wrote_line = false;
    for (modes) |mode| switch (mode) {
        .decode => switch (block_id) {
            blockId("LSCR"), blockId("LSC2") => {
                const lsc_number = decodeLsc(
                    allocator,
                    block_id,
                    block_seq,
                    data,
                    state,
                ) catch |err| {
                    if (err == error.BadData)
                        continue;
                    return err;
                };

                if (!wrote_line) {
                    try writeLscAsmLine(block_id, block_seq, lsc_number, state, room_state);
                    wrote_line = true;
                }
            },
            else => unreachable,
        },
        .raw => {
            try writeRawBlockFile(block_id, block_seq, data, state);

            if (!wrote_line) {
                try writeRawBlockLine(block_id, block_seq, state, room_state);
                wrote_line = true;
            }
        },
    };

    // This should be unreachable as long as `raw` is in the mode list
    if (!wrote_line)
        return error.BadData;
}

fn extractGlob(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number_opt: ?u32,
    data: []const u8,
    modes: []const ResourceMode,
    rmda_data: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    // HACK: this only works for one block per block id per room. it's needed
    // for a strange CHAR block in backyard soccer.
    const block_number = glob_number_opt orelse 0;

    var wrote_line = false;
    for (modes) |mode| switch (mode) {
        .decode => switch (block_id) {
            blockId("SCRP") => {
                decodeScrp(allocator, block_number, data, state) catch |err| {
                    if (err == error.BadData)
                        continue;
                    return err;
                };

                if (!wrote_line) {
                    try writeScrpAsmLine(block_number, state, room_state);
                    wrote_line = true;
                }
            },
            blockId("DIGI"), blockId("TALK") => {
                decodeAudio(block_id, block_number, data, state) catch |err| {
                    if (err == error.BadData)
                        continue;
                    return err;
                };

                if (!wrote_line) {
                    try writeDigiLine(block_id, block_number, state, room_state);
                    wrote_line = true;
                }
            },
            blockId("AWIZ") => {
                var wiz = decodeAwiz(allocator, block_number, rmda_data, data, state) catch |err| {
                    if (err == error.DecodeAwiz)
                        continue;
                    return err;
                };
                defer wiz.deinit(allocator);

                if (!wrote_line) {
                    try writeAwizLines(block_number, &wiz, state, room_state);
                    wrote_line = true;
                }
            },
            blockId("MULT") => {
                var mult = decodeMult(allocator, block_number, rmda_data, data, state) catch |err| {
                    if (err == error.DecodeAwiz)
                        continue;
                    return err;
                };
                defer mult.deinit(allocator);

                if (!wrote_line) {
                    try writeMultLines(&mult, room_state);
                    wrote_line = true;
                }
            },
            else => unreachable,
        },
        .raw => {
            if (glob_number_opt) |glob_number| {
                try writeRawGlobFile(block_id, glob_number, data, state);

                if (!wrote_line) {
                    try writeRawGlobLine(block_id, glob_number, state, room_state);
                    wrote_line = true;
                }
            } else {
                try writeRawBlockFile(block_id, block_number, data, state);

                if (!wrote_line) {
                    try writeRawBlockLine(block_id, block_number, state, room_state);
                    wrote_line = true;
                }
            }
        },
    };

    // This should be unreachable as long as `raw` is in the mode list
    if (!wrote_line)
        return error.BadData;
}

fn decodeRmim(
    allocator: std.mem.Allocator,
    rmim_raw: []const u8,
    rmda_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    var bmp = try rmim.decode(allocator, rmim_raw, rmda_raw);
    defer bmp.deinit(allocator);

    const path = try pathf.print(&state.cur_path, "RMIM.bmp", .{});
    defer path.restore();

    const output_file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer output_file.close();

    try output_file.writeAll(bmp.items);

    try room_state.room_txt.print("room-image {s}\n", .{path.relative()});
}

fn decodeScrp(
    allocator: std.mem.Allocator,
    glob_number: u32,
    data: []const u8,
    state: *State,
) !void {
    var disassembly = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 1024);
    defer disassembly.deinit(allocator);

    try disasm.disassemble(allocator, data, disassembly.writer(allocator));

    const path = try appendGlobPath(state, comptime blockId("SCRP"), glob_number, "s");
    defer path.restore();

    const output_file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer output_file.close();

    try output_file.writeAll(disassembly.items);
}

fn writeScrpAsmLine(
    glob_number: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try appendGlobPath(state, comptime blockId("SCRP"), glob_number, "s");
    defer path.restore();

    try room_state.room_txt.print(
        "scrp-asm {} {s}\n",
        .{ glob_number, path.relative() },
    );
}

fn decodeLsc(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_seq: u32,
    data: []const u8,
    state: *State,
) !u32 {
    var disassembly = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 1024);
    defer disassembly.deinit(allocator);

    const lsc_number, const bytecode = switch (block_id) {
        blockId("LSCR") => blk: {
            if (data.len < 1)
                return error.BadData;
            break :blk .{ data[0], data[1..] };
        },
        blockId("LSC2") => blk: {
            if (data.len < 4)
                return error.BadData;
            const number = std.mem.readInt(u32, data[0..4], .little);
            break :blk .{ number, data[4..] };
        },
        else => unreachable,
    };
    try disasm.disassemble(allocator, bytecode, disassembly.writer(allocator));

    const path = try appendGlobPath(state, block_id, block_seq, "s");
    defer path.restore();

    const file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer file.close();
    try file.writeAll(disassembly.items);

    return lsc_number;
}

fn writeLscAsmLine(
    block_id: BlockId,
    block_seq: u32,
    lsc_number: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    // TODO: use lsc_number as the filename too. i'm not doing this yet because
    // it would be inconsistent with the filenames when decoded as raw.

    const path = try appendGlobPath(state, block_id, block_seq, "s");
    defer path.restore();

    try room_state.room_txt.print(
        "    lsc-asm {s} {} {s}\n",
        .{ blockIdToStr(&block_id), lsc_number, room_state.curPathRelative() },
    );
}

fn decodeAudio(
    block_id: BlockId,
    glob_number: u32,
    data: []const u8,
    state: *State,
) !void {
    const path = try appendGlobPath(state, block_id, glob_number, "wav");
    defer path.restore();

    const output_file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer output_file.close();
    var output_writer = std.io.bufferedWriter(output_file.writer());

    try audio.decode(data, output_writer.writer());

    try output_writer.flush();
}

fn writeDigiLine(
    block_id: BlockId,
    glob_number: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try appendGlobPath(state, block_id, glob_number, "wav");
    defer path.restore();

    try room_state.room_txt.print(
        "audio {s} {} {s}\n",
        .{ blockIdToStr(&block_id), glob_number, path.relative() },
    );
}

fn decodeAwiz(
    allocator: std.mem.Allocator,
    glob_number: u32,
    rmda_raw: []const u8,
    awiz_raw: []const u8,
    state: *State,
) !awiz.Awiz {
    const path = try appendGlobPath(state, comptime blockId("AWIZ"), glob_number, "bmp");
    defer path.restore();

    return decodeAwizIntoPath(allocator, rmda_raw, awiz_raw, path.full());
}

fn decodeAwizIntoPath(
    allocator: std.mem.Allocator,
    rmda_raw: []const u8,
    awiz_raw: []const u8,
    path: [*:0]const u8,
) !awiz.Awiz {
    var wiz = try awiz.decode(allocator, awiz_raw, rmda_raw);
    errdefer wiz.deinit(allocator);

    for (wiz.blocks.slice()) |block| switch (block) {
        .two_ints, .wizh => {},
        .wizd => |bmp_data| {
            const output_file = try std.fs.cwd().createFileZ(path, .{});
            defer output_file.close();

            try output_file.writeAll(bmp_data.items);
        },
    };

    return wiz;
}

fn writeAwizLines(
    glob_number: u32,
    wiz: *const awiz.Awiz,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path =
        try appendGlobPath(state, comptime blockId("AWIZ"), glob_number, "bmp");
    defer path.restore();

    try room_state.room_txt.print("awiz {}\n", .{glob_number});
    try writeAwizChildrenGivenBmpPath(wiz, path.relative(), 1, room_state.room_txt);
    try room_state.room_txt.writeAll("end-awiz\n");
}

fn writeAwizChildrenGivenBmpPath(
    wiz: *const awiz.Awiz,
    bmp_relative_path: []const u8,
    indent: u8,
    out: anytype,
) !void {
    for (wiz.blocks.slice()) |block| {
        for (0..indent * 4) |_|
            try out.writeByte(' ');
        switch (block) {
            .two_ints => |b| {
                try out.print(
                    "{s} {} {}\n",
                    .{ blockIdToStr(&b.id), b.ints[0], b.ints[1] },
                );
            },
            .wizh => {
                try out.writeAll("WIZH\n");
            },
            .wizd => {
                try out.print("WIZD {s}\n", .{bmp_relative_path});
            },
        }
    }
}

const Mult = struct {
    wizs: std.ArrayListUnmanaged(awiz.Awiz) = .{},
    room_lines: std.ArrayListUnmanaged(u8) = .{},

    fn deinit(self: *Mult, allocator: std.mem.Allocator) void {
        self.room_lines.deinit(allocator);

        var i: usize = self.wizs.items.len;
        while (i > 0) {
            i -= 1;
            self.wizs.items[i].deinit(allocator);
        }
        self.wizs.deinit(allocator);
    }
};

fn decodeMult(
    allocator: std.mem.Allocator,
    glob_number: u32,
    rmda_raw: []const u8,
    mult_raw: []const u8,
    state: *State,
) !Mult {
    var mult = Mult{};
    errdefer mult.deinit(allocator);

    const path = try pathf.print(
        &state.cur_path,
        "{s}_{:0>4}_",
        .{ blockIdToStr(&comptime blockId("MULT")), glob_number },
    );
    defer path.restore();

    try mult.room_lines.ensureTotalCapacity(allocator, 256);
    try mult.room_lines.writer(allocator).print("mult {}\n", .{glob_number});

    var stream = std.io.fixedBufferStream(mult_raw);
    var mult_blocks = fixedBlockReader(&stream);

    while (try mult_blocks.peek() != comptime blockId("WRAP")) {
        const id, const len = try mult_blocks.next();
        switch (id) {
            blockId("DEFA") => {
                const defa_raw = try io.readInPlace(&stream, len);

                const path_end = try pathf.print(&state.cur_path, "{s}.bin", .{blockIdToStr(&id)});
                defer path_end.restore();

                const file = try std.fs.cwd().createFileZ(path.full(), .{});
                defer file.close();
                try file.writeAll(defa_raw);

                try mult.room_lines.writer(allocator).print(
                    "    raw-block {s} {s}\n",
                    .{ blockIdToStr(&id), path.relative() },
                );
            },
            else => return error.BadData,
        }
    }

    const wrap_len = try mult_blocks.assumeBlock("WRAP");
    const wrap_end: u32 = @intCast(stream.pos + wrap_len);
    var wrap_blocks = fixedBlockReader(&stream);

    const offs_start: u32 = @intCast(stream.pos);
    const offs_len = try wrap_blocks.expectBlock("OFFS");
    const offs_count = std.math.divExact(u32, offs_len, 4) catch return error.BadData;
    const offs_raw = try io.readInPlace(&stream, offs_len);
    const offs = std.mem.bytesAsSlice(u32, offs_raw);
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    var wiz_offsets = std.ArrayListUnmanaged(u32){};
    defer wiz_offsets.deinit(allocator);
    try wiz_offsets.ensureTotalCapacityPrecise(allocator, offs_count);

    try mult.wizs.ensureTotalCapacityPrecise(allocator, offs_count);

    while (stream.pos < wrap_end) {
        const wiz_offset: u32 = @intCast(stream.pos - offs_start);
        try wiz_offsets.append(allocator, wiz_offset);

        const awiz_len = try wrap_blocks.expectBlock("AWIZ");
        const awiz_raw = try io.readInPlace(&stream, awiz_len);

        const path2 = try appendGlobPath(
            state,
            comptime blockId("AWIZ"),
            @intCast(wiz_offsets.items.len - 1),
            "bmp",
        );
        defer path2.restore();

        var wiz = try decodeAwizIntoPath(allocator, rmda_raw, awiz_raw, path.full());
        mult.wizs.appendAssumeCapacity(wiz);

        try mult.room_lines.appendSlice(allocator, "    awiz\n");
        try writeAwizChildrenGivenBmpPath(
            &wiz,
            path.relative(),
            2,
            &mult.room_lines.writer(allocator),
        );
        try mult.room_lines.appendSlice(allocator, "    end-awiz\n");
    }

    try mult.room_lines.appendSlice(allocator, "    indices");
    for (offs) |off| {
        const index = std.sort.binarySearch(u32, off, wiz_offsets.items, {}, u32Order) orelse
            return error.BadData;
        try mult.room_lines.writer(allocator).print(" {}", .{index});
    }
    try mult.room_lines.append(allocator, '\n');

    try wrap_blocks.finish(wrap_end);

    try mult_blocks.finishEof();

    try mult.room_lines.appendSlice(allocator, "end-mult\n");

    return mult;
}

fn u32Order(_: void, lhs: u32, rhs: u32) std.math.Order {
    return std.math.order(lhs, rhs);
}

fn writeMultLines(mult: *const Mult, room_state: *const RoomState) !void {
    try room_state.room_txt.writeAll(mult.room_lines.items);
}

fn readGlob(
    allocator: std.mem.Allocator,
    blocks: anytype,
    block_id: BlockId,
    reader: anytype,
) ![]u8 {
    const block_len = try blocks.expect(block_id);
    const data = try allocator.alloc(u8, block_len);
    errdefer allocator.free(data);
    try reader.reader().readNoEof(data);
    return data;
}

// TODO: this is mostly a copy/paste
fn writeGlob(
    block_id: BlockId,
    glob_number: u32,
    data: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try appendGlobPath(state, block_id, glob_number, "bin");
    defer path.restore();

    try room_state.room_txt.print(
        "raw-glob {s} {} {s}\n",
        .{ blockIdToStr(&block_id), glob_number, path.relative() },
    );

    const output_file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer output_file.close();

    try output_file.writeAll(data);
}

fn writeRawGlobFile(
    block_id: BlockId,
    glob_number: u32,
    data: []const u8,
    state: *State,
) !void {
    const path = try appendGlobPath(state, block_id, glob_number, "bin");
    defer path.restore();

    const output_file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer output_file.close();

    try output_file.writeAll(data);
}

fn writeRawGlobLine(
    block_id: BlockId,
    glob_number: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try appendGlobPath(state, block_id, glob_number, "bin");
    defer path.restore();

    try room_state.room_txt.print(
        "raw-glob {s} {} {s}\n",
        .{ blockIdToStr(&block_id), glob_number, room_state.curPathRelative() },
    );
}

const writeRawBlockFile = writeRawGlobFile;

fn writeRawBlockLine(
    block_id: BlockId,
    block_seq: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try appendGlobPath(state, block_id, block_seq, "bin");
    defer path.restore();

    try room_state.room_txt.print(
        "    raw-block {s} {s}\n",
        .{ blockIdToStr(&block_id), room_state.curPathRelative() },
    );
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
    return null;
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

fn appendGlobPath(
    state: *State,
    block_id: BlockId,
    number: u32,
    ext: []const u8,
) !pathf.PrintedPath {
    return pathf.print(
        &state.cur_path,
        "{s}_{:0>4}.{s}",
        .{ blockIdToStr(&block_id), number, ext },
    );
}
