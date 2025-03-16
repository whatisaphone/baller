const std = @import("std");

const Symbols = @import("Symbols.zig");
const akos = @import("akos.zig");
const audio = @import("audio.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const blockReader = @import("block_reader.zig").blockReader;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const xor_key = @import("build.zig").xor_key;
const cliargs = @import("cliargs.zig");
const disasm = @import("disasm.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const lang = @import("lang.zig");
const report = @import("report.zig");
const pathf = @import("pathf.zig");
const rmim = @import("rmim.zig");

pub fn runCli(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    var input_path_opt: ?[:0]const u8 = null;
    var output_path_opt: ?[:0]const u8 = null;
    var symbols_path: ?[:0]const u8 = null;
    var akos_modes: []const ResourceMode = &.{ .decode, .raw };

    var it = cliargs.Iterator.init(args);
    while (it.next()) |arg| switch (arg) {
        .long_option => |opt| {
            if (std.mem.eql(u8, opt.flag, "symbols")) {
                if (symbols_path == null)
                    symbols_path = opt.value
                else
                    return arg.reportUnexpected();
            } else if (std.mem.eql(u8, opt.flag, "akos")) {
                if (std.mem.eql(u8, opt.value, "raw"))
                    akos_modes = &.{.raw}
                else
                    return arg.reportInvalidValue();
            } else {
                return arg.reportUnexpected();
            }
        },
        .positional => |str| {
            if (input_path_opt == null)
                input_path_opt = str
            else if (output_path_opt == null)
                output_path_opt = str
            else
                return arg.reportUnexpected();
        },
        else => return arg.reportUnexpected(),
    };

    const input_path = input_path_opt orelse return cliargs.reportMissing("index");
    const output_path = output_path_opt orelse return cliargs.reportMissing("output");

    var symbols_text: []const u8 = &.{};
    defer allocator.free(symbols_text);

    if (symbols_path) |path|
        symbols_text = try fs.readFileZ(allocator, std.fs.cwd(), path);

    var result = try run(allocator, &.{
        .input_path = input_path,
        .output_path = output_path,
        .rmim_decode = true,
        .script_modes = &.{ .decode, .raw },
        .sound_modes = &.{ .decode, .raw },
        .awiz_modes = &.{ .decode, .raw },
        .mult_modes = &.{ .decode, .raw },
        .akos_modes = akos_modes,
        .akcd_modes = &.{ .decode, .raw },
        .symbols_text = symbols_text,
    });
    defer result.deinit(allocator);
}

pub const ResourceMode = enum {
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
    akos_modes: []const ResourceMode,
    akcd_modes: []const ResourceMode,
    symbols_text: []const u8,
    dump_index: bool = false,
};

pub const Result = struct {
    block_stats: std.AutoArrayHashMapUnmanaged(BlockId, BlockStat),
    scripts_with_unknown_byte: u32,

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        self.block_stats.deinit(allocator);
    }
};

pub fn run(allocator: std.mem.Allocator, args: *const Extract) !Result {
    var input_path_buf = pathf.Path{};
    try input_path_buf.appendSlice(args.input_path);
    try input_path_buf.append(0);
    const input_path = input_path_buf.buffer[0 .. input_path_buf.len - 1 :0];

    const output_path = args.output_path;

    const game = try games.detectGameOrFatal(input_path);

    try fs.makeDirIfNotExistZ(std.fs.cwd(), output_path);

    var state: State = .{
        .options = args,
        .symbols = .{ .game = game },
        .hack_skip_awiz_uncompressed = game == .basketball,
    };
    errdefer state.deinit(allocator);

    try state.cur_path.appendSlice(output_path);
    try state.cur_path.append('/');

    state.symbols = try Symbols.parse(allocator, game, args.symbols_text);

    try state.block_seqs.ensureTotalCapacity(allocator, 16);

    var index = try readIndex(allocator, game, input_path);
    defer index.deinit(allocator);

    if (args.dump_index) {
        var buf = std.io.bufferedWriter(std.io.getStdOut().writer());
        try dumpIndex(&index, buf.writer());
        try buf.flush();
    }

    try writeIndexBlobs(game, &index, &state.cur_path);

    const project_txt_file = project_txt_file: {
        const path = try pathf.append(&state.cur_path, "project.txt");
        defer path.restore();

        break :project_txt_file try std.fs.cwd().createFileZ(path.full(), .{});
    };
    defer project_txt_file.close();

    var project_txt = std.io.bufferedWriter(project_txt_file.writer());

    if (args.symbols_text.len != 0) {
        const path = try pathf.append(&state.cur_path, "symbols.ini");
        defer path.restore();

        try fs.writeFileZ(std.fs.cwd(), path.full(), args.symbols_text);

        try project_txt.writer().writeAll("symbols symbols.ini\n");
    }

    if (std.mem.indexOfScalar(ResourceMode, args.script_modes, .decode)) |_|
        state.language = lang.buildLanguage(game);

    for (1..1 + games.numberOfDisks(game)) |disk_number_usize| {
        const disk_number: u8 = @intCast(disk_number_usize);

        games.pointPathToDisk(game, input_path, disk_number);

        try extractDisk(
            allocator,
            &state,
            input_path,
            game,
            disk_number,
            &project_txt,
            &index,
        );
    }

    try project_txt.flush();

    // partial deinit/move of state
    state.block_seqs.deinit(allocator);
    state.symbols.deinit(allocator);

    return .{
        .block_stats = state.block_stats,
        .scripts_with_unknown_byte = state.scripts_with_unknown_byte,
    };
}

const State = struct {
    options: *const Extract,
    cur_path: pathf.Path = .{},
    symbols: Symbols,
    /// workaround for basketball bug
    hack_skip_awiz_uncompressed: bool,
    block_seqs: std.AutoArrayHashMapUnmanaged(BlockId, u16) = .{},
    block_stats: std.AutoArrayHashMapUnmanaged(BlockId, BlockStat) = .{},
    language: ?lang.Language = null,
    scripts_with_unknown_byte: u32 = 0,

    fn deinit(self: *State, allocator: std.mem.Allocator) void {
        self.block_stats.deinit(allocator);
        self.block_seqs.deinit(allocator);
        self.symbols.deinit(allocator);
    }

    fn incBlockSeq(self: *State, allocator: std.mem.Allocator, block_id: BlockId) !u16 {
        const entry = try self.block_seqs.getOrPutValue(allocator, block_id, 0);
        entry.value_ptr.* += 1;
        return entry.value_ptr.*;
    }

    pub fn incrBlockStat(
        self: *State,
        allocator: std.mem.Allocator,
        block_id: BlockId,
        stat: std.meta.FieldEnum(BlockStat),
    ) !void {
        const entry = try self.block_stats.getOrPutValue(allocator, block_id, .{});
        const stat_ptr = switch (stat) {
            inline else => |s| &@field(entry.value_ptr, @tagName(s)),
        };
        stat_ptr.* += 1;
    }

    pub fn warnScriptUnknownByte(self: *State) void {
        self.scripts_with_unknown_byte += 1;
    }
};

pub const BlockStat = struct {
    raw: u32 = 0,
    decoded: u32 = 0,
    total: u32 = 0,
};

const RoomState = struct {
    room_number: u8,
    path: *pathf.Path,
    path_start: pathf.PathLen,
    room_txt: std.io.BufferedWriter(4096, std.fs.File.Writer).Writer,
    rmda: union { pending: void, raw: []const u8 },

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
    sver: ?[]u8,

    fn deinit(self: *Index, allocator: std.mem.Allocator) void {
        if (self.sver) |sver|
            allocator.free(sver);
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
        return self.room_name_buf[start..][0..len];
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

    const sver = if (games.hasIndexSver(game)) blk: {
        const sver_len = try blocks.expectBlock("SVER");
        const sver = try allocator.alloc(u8, sver_len);
        errdefer allocator.free(sver);
        try in.readNoEof(sver);
        break :blk sver;
    } else null;
    errdefer if (sver) |x|
        allocator.free(x);

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

        const note_block_len = try inib_blocks.expectBlock("NOTE");
        if (note_block_len < 2)
            return error.BadData;
        const note_str_len = note_block_len - 2;
        if (try in.readInt(u16, .little) != note_str_len)
            return error.BadData;
        try in.skipBytes(note_str_len, .{});

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
        .sver = sver,
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

    return result;
}

// Let's see how long we can get away with this.
fn writeIndexBlobs(
    game: games.Game,
    index: *const Index,
    path_buf: *pathf.Path,
) !void {
    const maxs_data = std.mem.asBytes(index.maxs)[0..games.maxsLen(game)];
    try writeIndexBlob(path_buf, "maxs.bin", maxs_data);

    try writeIndexBlob(path_buf, "dobj.bin", index.dobj);
    try writeIndexBlob(path_buf, "aary.bin", index.aary);
    if (index.sver) |sver|
        try writeIndexBlob(path_buf, "sver.bin", sver);
}

fn writeIndexBlob(
    path_buf: *pathf.Path,
    filename: []const u8,
    bytes: []const u8,
) !void {
    const path = try pathf.append(path_buf, filename);
    defer path.restore();
    try fs.writeFileZ(std.fs.cwd(), path.full(), bytes);
}

fn dumpIndex(index: *const Index, out: anytype) !void {
    inline for (comptime std.meta.fieldNames(Directories)) |name|
        try dumpDirectory(name, &@field(index.directories, name), out);
    for (0.., index.lfl_offsets) |i, off|
        try out.print("{s}.{},0x{x}\n", .{ "lfl_offset", i, off });
    if (index.lfl_disks) |disks|
        for (0.., disks) |i, disk|
            try out.print("{s}.{},{}\n", .{ "lfl_disk", i, disk });
}

fn dumpDirectory(
    name: []const u8,
    directory: *const std.MultiArrayList(DirectoryEntry),
    out: anytype,
) !void {
    const slice = directory.slice();
    for (slice.items(.room), 0..) |value, i|
        try out.print("{s}.{s}.{},{}\n", .{ name, "room", i, value });
    for (slice.items(.offset), 0..) |value, i|
        try out.print("{s}.{s}.{},0x{x}\n", .{ name, "offset", i, value });
    for (slice.items(.len), 0..) |value, i|
        try out.print("{s}.{s}.{},0x{x}\n", .{ name, "len", i, value });
}

fn extractDisk(
    allocator: std.mem.Allocator,
    state: *State,
    input_path: [*:0]u8,
    game: games.Game,
    disk_number: u8,
    project_txt: anytype,
    index: *const Index,
) !void {
    const file = try std.fs.cwd().openFileZ(input_path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);

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
            const path = try pathf.append(&state.cur_path, "room.txt");
            defer path.restore();
            break :room_txt_file try std.fs.cwd().createFileZ(path.full(), .{});
        };
        defer room_txt_file.close();

        var room_txt = std.io.bufferedWriter(room_txt_file.writer());

        var room_state = RoomState{
            .room_number = room_number,
            .path = &state.cur_path,
            .path_start = @intCast(state.cur_path.len),
            .room_txt = room_txt.writer(),
            .rmda = .{ .pending = {} },
        };

        // Block seqs start over for each room
        state.block_seqs.clearRetainingCapacity();

        var lflf_blocks = blockReader(&reader);

        const rmim_data =
            try readGlob(allocator, &lflf_blocks, comptime blockId("RMIM"), &reader);
        defer allocator.free(rmim_data);

        const rmda_data =
            try readGlob(allocator, &lflf_blocks, comptime blockId("RMDA"), &reader);
        room_state.rmda = .{ .raw = rmda_data };
        defer allocator.free(room_state.rmda.raw);

        try state.incrBlockStat(allocator, comptime blockId("RMIM"), .total);

        const rmim_decoded = rmim_decoded: {
            if (!state.options.rmim_decode)
                break :rmim_decoded false;
            decodeRmim(allocator, rmim_data, state, &room_state) catch |err| {
                if (err != error.DecompressBmap)
                    return err;
                break :rmim_decoded false;
            };
            try state.incrBlockStat(allocator, comptime blockId("RMIM"), .decoded);
            break :rmim_decoded true;
        };
        if (!rmim_decoded) {
            try writeGlob(
                comptime blockId("RMIM"),
                room_number,
                rmim_data,
                state,
                &room_state,
            );
            try state.incrBlockStat(allocator, comptime blockId("RMIM"), .raw);
        }

        try extractRmda(allocator, state, &room_state);

        while (reader.bytes_read < lflf_end) {
            const offset: u32 = @intCast(reader.bytes_read);
            const id, const len = try lflf_blocks.next();

            const glob_number = try findGlobNumber(index, id, disk_number, offset);

            const data = try allocator.alloc(u8, len);
            defer allocator.free(data);
            try reader.reader().readNoEof(data);

            try extractGlob(
                allocator,
                id,
                glob_number,
                data,
                glob_decoders,
                state,
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
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try pathf.print(&state.cur_path, "RMDA/", .{});
    defer path.restore();

    try fs.makeDirIfNotExistZ(std.fs.cwd(), path.full());

    var reader = std.io.fixedBufferStream(room_state.rmda.raw);
    var blocks = fixedBlockReader(&reader);

    try room_state.room_txt.writeAll("rmda\n");

    while (reader.pos < room_state.rmda.raw.len) {
        const block_id, const block_len = try blocks.next();
        const block_raw = try io.readInPlace(&reader, block_len);

        try extractRmdaBlock(allocator, block_id, block_raw, state, room_state);
    }

    try blocks.finishEof();

    try room_state.room_txt.writeAll("end-rmda\n");
}

fn extractRmdaBlock(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    data: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    try state.incrBlockStat(allocator, block_id, .total);

    const block_number = switch (block_id) {
        blockId("LSCR"), blockId("LSC2") => try getLscBlockNumber(block_id, data),
        blockId("OBIM"), blockId("OBCD") => try state.incBlockSeq(allocator, block_id),
        else => null,
    };

    for (getBlockModes(block_id, state)) |mode| switch (mode) {
        .decode => {
            const decode_result = switch (block_id) {
                blockId("ENCD"), blockId("EXCD") => decodeEnterExit(
                    allocator,
                    block_id,
                    data,
                    state,
                    room_state,
                ),
                blockId("LSCR"), blockId("LSC2") => decodeLsc(
                    allocator,
                    block_id,
                    block_number.?,
                    data,
                    state,
                    room_state,
                ),
                else => continue,
            };
            decode_result catch |err| {
                if (err == error.BadData)
                    continue;
                return err;
            };
            try state.incrBlockStat(allocator, block_id, .decoded);
            return;
        },
        .raw => {
            const path = if (block_number) |num|
                try pathf.appendBlockPath(&state.cur_path, block_id, num, "bin")
            else
                try pathf.print(&state.cur_path, "{s}.bin", .{blockIdToStr(&block_id)});
            defer path.restore();

            try fs.writeFileZ(std.fs.cwd(), path.full(), data);

            try room_state.room_txt.print(
                "    raw-block {s} {s}\n",
                .{ blockIdToStr(&block_id), room_state.curPathRelative() },
            );

            try state.incrBlockStat(allocator, block_id, .raw);
            return;
        },
    };

    // This should be unreachable as long as `raw` is in the mode list
    return error.BadData;
}

fn BlockDecoder(Cx: type) type {
    return *const fn (
        allocator: std.mem.Allocator,
        block_id: BlockId,
        block_number: u32,
        block_raw: []const u8,
        state: *State,
        room_state: *const RoomState,
        cx: Cx,
    ) anyerror!void;
}

fn BlockDecoderPair(Cx: type) type {
    return struct {
        id: BlockId,
        decoder: BlockDecoder(Cx),
    };
}

const glob_decoders: []const BlockDecoderPair(void) = &.{
    .{ .id = blockId("SCRP"), .decoder = decodeScrp },
    .{ .id = blockId("DIGI"), .decoder = decodeAudio },
    .{ .id = blockId("TALK"), .decoder = decodeAudio },
    .{ .id = blockId("AWIZ"), .decoder = decodeAwiz },
    .{ .id = blockId("MULT"), .decoder = decodeMult },
    .{ .id = blockId("WSOU"), .decoder = decodeWsou },
    .{ .id = blockId("AKOS"), .decoder = decodeAkos },
};

fn extractGlob(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number_opt: ?u32,
    data: []const u8,
    decoders: []const BlockDecoderPair(void),
    state: *State,
    room_state: *const RoomState,
) !void {
    try state.incrBlockStat(allocator, block_id, .total);

    // HACK: this only works for one block per block id per room. it's needed
    // for a strange CHAR block in backyard soccer.
    const block_number = glob_number_opt orelse 0;

    for (getBlockModes(block_id, state)) |mode| switch (mode) {
        .decode => {
            const decoder = for (decoders) |p| {
                if (p.id == block_id)
                    break p.decoder;
            } else continue;

            const glob_number = glob_number_opt orelse return error.BadData;

            decoder(
                allocator,
                block_id,
                glob_number,
                data,
                state,
                room_state,
                {},
            ) catch |err| {
                if (err == error.BadData or err == error.BlockFallbackToRaw)
                    continue;
                return err;
            };
            try state.incrBlockStat(allocator, block_id, .decoded);
            return;
        },
        .raw => {
            if (glob_number_opt) |glob_number| {
                try writeRawGlobFile(block_id, glob_number, data, state);
                try writeRawGlobLine(block_id, glob_number, state, room_state);
            } else {
                try writeRawBlockFile(block_id, block_number, data, state);
                try writeRawBlockLine(block_id, block_number, state, room_state);
            }
            try state.incrBlockStat(allocator, block_id, .raw);
            return;
        },
    };

    // This should be unreachable as long as `raw` is in the mode list
    return error.BadData;
}

fn getBlockModes(block_id: BlockId, state: *const State) []const ResourceMode {
    return switch (block_id) {
        blockId("SCRP"),
        blockId("ENCD"),
        blockId("EXCD"),
        blockId("LSCR"),
        blockId("LSC2"),
        => state.options.script_modes,
        blockId("DIGI"), blockId("TALK"), blockId("WSOU") => state.options.sound_modes,
        blockId("AWIZ") => state.options.awiz_modes,
        blockId("MULT") => state.options.mult_modes,
        blockId("AKOS") => state.options.akos_modes,
        else => &.{ResourceMode.raw},
    };
}

fn decodeRmim(
    allocator: std.mem.Allocator,
    rmim_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    var decoded = try rmim.decode(allocator, rmim_raw, room_state.rmda.raw);
    defer decoded.deinit(allocator);

    const path = try pathf.print(&state.cur_path, "RMIM.bmp", .{});
    defer path.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), decoded.bmp.items);

    try room_state.room_txt.print(
        "room-image {} {s}\n",
        .{ decoded.compression, path.relative() },
    );
}

fn decodeScrp(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    _: void,
) !void {
    std.debug.assert(block_id == comptime blockId("SCRP"));
    try decodeScrpData(allocator, glob_number, block_raw, state);
    try writeScrpAsmLine(glob_number, state, room_state);
}

fn decodeScrpData(
    allocator: std.mem.Allocator,
    glob_number: u32,
    data: []const u8,
    state: *State,
) !void {
    var disassembly = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 1024);
    defer disassembly.deinit(allocator);

    try disasm.disassemble(
        allocator,
        &state.language.?,
        .{ .global = glob_number },
        data,
        &state.symbols,
        disassembly.writer(allocator),
        state,
    );

    const path = try appendGlobPath(state, comptime blockId("SCRP"), glob_number, "s");
    defer path.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), disassembly.items);
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

fn decodeEnterExit(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    data: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    var disassembly = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 1024);
    defer disassembly.deinit(allocator);

    const script_id: Symbols.ScriptId = switch (block_id) {
        blockId("ENCD") => .{ .enter = .{ .room = room_state.room_number } },
        blockId("EXCD") => .{ .exit = .{ .room = room_state.room_number } },
        else => unreachable,
    };

    try disasm.disassemble(
        allocator,
        &state.language.?,
        script_id,
        data,
        &state.symbols,
        disassembly.writer(allocator),
        state,
    );

    const path = try pathf.print(&state.cur_path, "{s}.s", .{blockIdToStr(&block_id)});
    defer path.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), disassembly.items);

    const keyword = switch (block_id) {
        blockId("ENCD") => "encd-asm",
        blockId("EXCD") => "excd-asm",
        else => unreachable,
    };
    try room_state.room_txt.print(
        "    {s} {s}\n",
        .{ keyword, room_state.curPathRelative() },
    );
}

fn getLscBlockNumber(block_id: BlockId, data: []const u8) !u32 {
    return switch (block_id) {
        blockId("LSCR") => {
            if (data.len < 1)
                return error.BadData;
            return data[0];
        },
        blockId("LSC2") => {
            if (data.len < 4)
                return error.BadData;
            return std.mem.readInt(u32, data[0..4], .little);
        },
        else => unreachable,
    };
}

fn decodeLsc(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_number: u32,
    data: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    try decodeLscData(allocator, block_id, block_number, data, state, room_state);
    try writeLscAsmLine(block_id, block_number, state, room_state);
}

fn decodeLscData(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_number: u32,
    data: []const u8,
    state: *State,
    room_state: *const RoomState,
) !void {
    var disassembly = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 1024);
    defer disassembly.deinit(allocator);

    const bytecode_offset: u8 = switch (block_id) {
        blockId("LSCR") => 1,
        blockId("LSC2") => 4,
        else => unreachable,
    };
    const bytecode = data[bytecode_offset..];

    try disasm.disassemble(
        allocator,
        &state.language.?,
        .{ .local = .{ .room = room_state.room_number, .number = block_number } },
        bytecode,
        &state.symbols,
        disassembly.writer(allocator),
        state,
    );

    const path = try appendGlobPath(state, block_id, block_number, "s");
    defer path.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), disassembly.items);
}

fn writeLscAsmLine(
    block_id: BlockId,
    block_number: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    const path = try appendGlobPath(state, block_id, block_number, "s");
    defer path.restore();

    try room_state.room_txt.print(
        "    lsc-asm {s} {} {s}\n",
        .{ blockIdToStr(&block_id), block_number, room_state.curPathRelative() },
    );
}

fn decodeAudio(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    _: void,
) !void {
    _ = allocator;
    try decodeAudioData(block_id, glob_number, block_raw, state);
    try writeAudioLine(block_id, glob_number, state, room_state);
}

fn decodeAudioData(
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

fn writeAudioLine(
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

fn decodeWsou(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    _: void,
) !void {
    _ = allocator;
    std.debug.assert(block_id == comptime blockId("WSOU"));
    try decodeWsouData(block_id, glob_number, block_raw, state);
    try writeWsouLine(block_id, glob_number, state, room_state);
}

fn decodeWsouData(
    block_id: BlockId,
    glob_number: u32,
    data: []const u8,
    state: *State,
) !void {
    std.debug.assert(block_id == comptime blockId("WSOU"));
    const path = try appendGlobPath(state, block_id, glob_number, "wav");
    defer path.restore();
    try fs.writeFileZ(std.fs.cwd(), path.full(), data);
}

fn writeWsouLine(
    block_id: BlockId,
    glob_number: u32,
    state: *State,
    room_state: *const RoomState,
) !void {
    std.debug.assert(block_id == comptime blockId("WSOU"));
    const path = try appendGlobPath(state, block_id, glob_number, "wav");
    defer path.restore();
    try room_state.room_txt.print(
        "wsou {} {s}\n",
        .{ glob_number, room_state.curPathRelative() },
    );
}

fn decodeAwiz(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    _: void,
) !void {
    std.debug.assert(block_id == comptime blockId("AWIZ"));

    var wiz = try decodeAwizData(
        allocator,
        glob_number,
        room_state.rmda.raw,
        block_raw,
        state,
    );
    defer wiz.deinit(allocator);

    try writeAwizLines(glob_number, &wiz, state, room_state);
}

fn decodeAwizData(
    allocator: std.mem.Allocator,
    glob_number: u32,
    rmda_raw: []const u8,
    awiz_raw: []const u8,
    state: *State,
) !awiz.Awiz {
    const path = try appendGlobPath(state, comptime blockId("AWIZ"), glob_number, "bmp");
    defer path.restore();

    return decodeAwizIntoPath(allocator, rmda_raw, null, awiz_raw, path.full(), state);
}

fn decodeAwizIntoPath(
    allocator: std.mem.Allocator,
    // TODO: merge next two params
    rmda_raw: []const u8,
    defa_rgbs: ?*const [0x300]u8,
    awiz_raw: []const u8,
    path: [*:0]const u8,
    state: *State,
) !awiz.Awiz {
    var wiz = awiz.decode(allocator, awiz_raw, rmda_raw, defa_rgbs, .{
        .hack_skip_uncompressed = state.hack_skip_awiz_uncompressed,
    }) catch |err| {
        if (err == error.BadData)
            return error.BlockFallbackToRaw;
        return err;
    };
    errdefer wiz.deinit(allocator);

    for (wiz.blocks.slice()) |block| switch (block) {
        .rgbs, .two_ints, .wizh, .trns => {},
        .wizd => |wizd| {
            try fs.writeFileZ(std.fs.cwd(), path, wizd.bmp.items);
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
            .rgbs => {
                try out.writeAll("RGBS\n");
            },
            .two_ints => |b| {
                try out.print(
                    "{s} {} {}\n",
                    .{ blockIdToStr(&b.id), b.ints[0], b.ints[1] },
                );
            },
            .wizh => {
                try out.writeAll("WIZH\n");
            },
            .trns => |trns| {
                try out.print("TRNS {}\n", .{trns});
            },
            .wizd => |wizd| switch (wizd.compression) {
                .none => try out.print("WIZD-uncompressed {s}\n", .{bmp_relative_path}),
                .rle => try out.print("WIZD {s}\n", .{bmp_relative_path}),
            },
        }
    }
}

fn decodeMult(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    _: void,
) !void {
    std.debug.assert(block_id == comptime blockId("MULT"));

    var mult = try decodeMultData(
        allocator,
        glob_number,
        block_raw,
        state,
        room_state,
    );
    defer mult.deinit(allocator);

    try writeMultLines(&mult, room_state);
}

const Mult = struct {
    defa_rgbs: ?*const [0x300]u8 = null,
    room_lines: std.ArrayListUnmanaged(u8) = .{},

    fn deinit(self: *Mult, allocator: std.mem.Allocator) void {
        self.room_lines.deinit(allocator);
    }
};

fn decodeMultData(
    allocator: std.mem.Allocator,
    glob_number: u32,
    mult_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
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

    if (try mult_blocks.peek() == comptime blockId("DEFA")) {
        const defa_len = try mult_blocks.assumeBlock("DEFA");
        const defa_raw = try io.readInPlace(&stream, defa_len);

        try extractMultDefa(allocator, defa_raw, &mult, path);
        try scanMultDefa(defa_raw, &mult);
    }

    const wrap_len = try mult_blocks.expectBlock("WRAP");
    const wrap_end: u32 = @intCast(stream.pos + wrap_len);
    var wrap_blocks = fixedBlockReader(&stream);

    const offs_start: u32 = @intCast(stream.pos);
    const offs_len = try wrap_blocks.expectBlock("OFFS");
    const offs_count = std.math.divExact(u32, offs_len, 4) catch return error.BadData;
    const offs_raw = try io.readInPlace(&stream, offs_len);
    const offs = std.mem.bytesAsSlice(u32, offs_raw);

    var wiz_offsets = std.ArrayListUnmanaged(u32){};
    defer wiz_offsets.deinit(allocator);
    try wiz_offsets.ensureTotalCapacityPrecise(allocator, offs_count);

    while (stream.pos < wrap_end) {
        const wiz_offset: u32 = @intCast(stream.pos - offs_start);
        try wiz_offsets.append(allocator, wiz_offset);

        const awiz_index: u32 = @intCast(wiz_offsets.items.len - 1);

        const awiz_id = comptime blockId("AWIZ");
        const awiz_len = try wrap_blocks.expect(awiz_id);
        const awiz_raw = try io.readInPlace(&stream, awiz_len);

        try extractMultChild(
            allocator,
            awiz_id,
            awiz_index,
            awiz_raw,
            mult_decoders,
            state,
            room_state,
            &mult,
        );
    }

    try wrap_blocks.finish(wrap_end);

    try mult_blocks.finishEof();

    try mult.room_lines.appendSlice(allocator, "    indices");
    for (offs) |off| {
        const index = std.sort.binarySearch(u32, wiz_offsets.items, off, orderU32) orelse
            return error.BadData;
        try mult.room_lines.writer(allocator).print(" {}", .{index});
    }
    try mult.room_lines.appendSlice(allocator, "\nend-mult\n");

    return mult;
}

const mult_decoders: []const BlockDecoderPair(*Mult) = &.{
    .{ .id = blockId("AWIZ"), .decoder = decodeMultAwiz },
};

fn scanMultDefa(defa_raw: []const u8, mult: *Mult) !void {
    var stream = std.io.fixedBufferStream(defa_raw);
    var blocks = fixedBlockReader(&stream);

    while (stream.pos != defa_raw.len) {
        const id, const len = try blocks.next();
        switch (id) {
            blockId("RGBS") => {
                if (len != 0x300) return error.BadData;
                if (mult.defa_rgbs != null) return error.BadData;
                mult.defa_rgbs = try io.readInPlaceAsValue(&stream, [0x300]u8);
            },
            else => {
                _ = try io.readInPlace(&stream, len);
            },
        }
    }

    try blocks.finishEof();
}

fn extractMultDefa(
    allocator: std.mem.Allocator,
    defa_raw: []const u8,
    mult: *Mult,
    path: pathf.PrintedPath,
) !void {
    const path_end = try pathf.print(path.buf, "{s}.bin", .{"DEFA"});
    defer path_end.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), defa_raw);

    try mult.room_lines.writer(allocator).print(
        "    raw-block {s} {s}\n",
        .{ "DEFA", path.relative() },
    );
}

fn extractMultChild(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_number: u32,
    data: []const u8,
    decoders: []const BlockDecoderPair(*Mult),
    state: *State,
    room_state: *const RoomState,
    cx: *Mult,
) !void {
    try state.incrBlockStat(allocator, block_id, .total);

    for (getBlockModes(block_id, state)) |mode| switch (mode) {
        .decode => {
            const decoder = for (decoders) |p| {
                if (p.id == block_id)
                    break p.decoder;
            } else continue;

            decoder(
                allocator,
                block_id,
                block_number,
                data,
                state,
                room_state,
                cx,
            ) catch |err| {
                if (err == error.BadData)
                    continue;
                return err;
            };
            try state.incrBlockStat(allocator, block_id, .decoded);
            return;
        },
        .raw => {
            const path = try appendGlobPath(state, block_id, block_number, "bin");
            defer path.restore();

            try fs.writeFileZ(std.fs.cwd(), path.full(), data);

            try cx.room_lines.writer(allocator).print(
                "    raw-block {s} {s}\n",
                .{ blockIdToStr(&block_id), room_state.curPathRelative() },
            );
            try state.incrBlockStat(allocator, block_id, .raw);
            return;
        },
    };

    // This should be unreachable as long as `raw` is in the mode list
    return error.BadData;
}

fn decodeMultAwiz(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    cx: *Mult,
) !void {
    std.debug.assert(block_id == comptime blockId("AWIZ"));

    const path = try appendGlobPath(state, block_id, block_number, "bmp");
    defer path.restore();

    var wiz = try decodeAwizIntoPath(
        allocator,
        room_state.rmda.raw,
        cx.defa_rgbs,
        block_raw,
        path.full(),
        state,
    );
    defer wiz.deinit(allocator);

    try cx.room_lines.appendSlice(allocator, "    awiz\n");
    try writeAwizChildrenGivenBmpPath(
        &wiz,
        room_state.curPathRelative(),
        2,
        cx.room_lines.writer(allocator),
    );
    try cx.room_lines.appendSlice(allocator, "    end-awiz\n");
}

fn orderU32(a: u32, b: u32) std.math.Order {
    return std.math.order(a, b);
}

fn writeMultLines(mult: *const Mult, room_state: *const RoomState) !void {
    try room_state.room_txt.writeAll(mult.room_lines.items);
}

fn decodeAkos(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    glob_number: u32,
    block_raw: []const u8,
    state: *State,
    room_state: *const RoomState,
    _: void,
) !void {
    std.debug.assert(block_id == comptime blockId("AKOS"));

    const akos_path = try pathf.print(&state.cur_path, "AKOS_{:0>4}/", .{glob_number});
    defer akos_path.restore();

    try fs.makeDirIfNotExistZ(std.fs.cwd(), akos_path.full());

    var manifest_buf = std.ArrayListUnmanaged(u8){};
    defer manifest_buf.deinit(allocator);

    try manifest_buf.writer(allocator).print("akos {}\n", .{glob_number});

    try akos.decode(
        allocator,
        block_raw,
        state.options.akcd_modes,
        akos_path,
        &manifest_buf,
        state,
    );

    try manifest_buf.appendSlice(allocator, "end-akos\n");

    try room_state.room_txt.writeAll(manifest_buf.items);
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

    try fs.writeFileZ(std.fs.cwd(), path.full(), data);
}

fn writeRawGlobFile(
    block_id: BlockId,
    glob_number: u32,
    data: []const u8,
    state: *State,
) !void {
    const path = try appendGlobPath(state, block_id, glob_number, "bin");
    defer path.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), data);
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
        blockId("DIGI"),
        blockId("SOUN"),
        blockId("TALK"),
        blockId("WSOU"),
        => &directories.sounds,
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
    return pathf.appendBlockPath(&state.cur_path, block_id, number, ext);
}
