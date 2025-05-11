const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const Block = @import("block_reader.zig").Block;
const fixedBlockReader2 = @import("block_reader.zig").fixedBlockReader2;
const streamingBlockReader = @import("block_reader.zig").streamingBlockReader;
const cliargs = @import("cliargs.zig");
const decompile = @import("decompile.zig");
const disasm = @import("disasm.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const lang = @import("lang.zig");
const mult = @import("mult.zig");
const pathf = @import("pathf.zig");
const rmim = @import("rmim.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub const xor_key = 0x69;

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var index_path_opt: ?[:0]const u8 = null;
    var output_path_opt: ?[:0]const u8 = null;
    var symbols_path: ?[:0]const u8 = null;
    var script: ?ScriptMode = null;
    var rmim_option: ?RawOrDecode = null;
    var scrp_option: ?RawOrDecode = null;
    var encd_option: ?RawOrDecode = null;
    var excd_option: ?RawOrDecode = null;
    var lsc2_option: ?RawOrDecode = null;
    var awiz_option: ?RawOrDecode = null;
    var mult_option: ?RawOrDecode = null;
    var akos_option: ?RawOrDecode = null;

    var it: cliargs.Iterator = .init(args);
    while (it.next()) |arg| switch (arg) {
        .positional => |str| {
            if (index_path_opt == null)
                index_path_opt = str
            else if (output_path_opt == null)
                output_path_opt = str
            else
                return arg.reportUnexpected();
        },
        .long_option => |opt| {
            if (std.mem.eql(u8, opt.flag, "symbols")) {
                if (symbols_path != null) return arg.reportDuplicate();
                symbols_path = opt.value;
            } else if (std.mem.eql(u8, opt.flag, "script")) {
                if (script != null) return arg.reportDuplicate();
                script = std.meta.stringToEnum(ScriptMode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "rmim")) {
                if (rmim_option != null) return arg.reportDuplicate();
                rmim_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "scrp")) {
                if (scrp_option != null) return arg.reportDuplicate();
                scrp_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "encd")) {
                if (encd_option != null) return arg.reportDuplicate();
                encd_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "excd")) {
                if (excd_option != null) return arg.reportDuplicate();
                excd_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "lsc2")) {
                if (lsc2_option != null) return arg.reportDuplicate();
                lsc2_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "awiz")) {
                if (awiz_option != null) return arg.reportDuplicate();
                awiz_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "mult")) {
                if (mult_option != null) return arg.reportDuplicate();
                mult_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "akos")) {
                if (akos_option != null) return arg.reportDuplicate();
                akos_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else {
                return arg.reportUnexpected();
            }
        },
        else => return arg.reportUnexpected(),
    };

    const index_path = index_path_opt orelse return cliargs.reportMissing("index");
    const output_path = output_path_opt orelse return cliargs.reportMissing("output");

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();

    _ = run(gpa, &diagnostic, .{
        .index_path = index_path,
        .output_path = output_path,
        .symbols_path = symbols_path,
        .options = .{
            .script = script orelse .decompile,
            .rmim = rmim_option orelse .decode,
            .scrp = scrp_option orelse .decode,
            .encd = encd_option orelse .decode,
            .excd = excd_option orelse .decode,
            .lsc2 = lsc2_option orelse .decode,
            .awiz = awiz_option orelse .decode,
            .mult = mult_option orelse .decode,
            .akos = akos_option orelse .decode,
        },
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
    };
    try diagnostic.writeToStderr();
}

const Extract = struct {
    index_path: [:0]const u8,
    output_path: [:0]const u8,
    symbols_path: ?[:0]const u8,
    options: Options,
};

const Options = struct {
    script: ScriptMode,
    rmim: RawOrDecode,
    scrp: RawOrDecode,
    encd: RawOrDecode,
    excd: RawOrDecode,
    lsc2: RawOrDecode,
    awiz: RawOrDecode,
    mult: RawOrDecode,
    akos: RawOrDecode,
};

const RawOrDecode = enum {
    raw,
    decode,
};

const ScriptMode = enum {
    disassemble,
    decompile,
};

pub const Stat = enum {
    scrp_total,
    scrp_disassemble,
    scrp_decompile,
    scrp_raw,
    excd_total,
    excd_disassemble,
    excd_decompile,
    excd_raw,
    encd_total,
    encd_disassemble,
    encd_decompile,
    encd_raw,
    lsc2_total,
    lsc2_disassemble,
    lsc2_decompile,
    lsc2_raw,
    script_unknown_byte,
};

pub fn run(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    args: Extract,
) !std.EnumArray(Stat, u16) {
    const input_path_opt, const index_name = fs.splitPathZ(args.index_path);
    var input_dir = if (input_path_opt) |input_path|
        try std.fs.cwd().openDir(input_path, .{})
    else
        std.fs.cwd();
    defer if (input_path_opt) |_|
        input_dir.close();

    try fs.makeDirIfNotExistZ(std.fs.cwd(), args.output_path);
    var output_dir = try std.fs.cwd().openDirZ(args.output_path, .{});
    defer output_dir.close();

    const game = try games.detectGameOrFatal(index_name);

    var symbols_text: []const u8 = "";
    defer gpa.free(symbols_text);

    var symbols: Symbols = if (args.symbols_path) |path| symbols: {
        symbols_text = try fs.readFileZ(gpa, std.fs.cwd(), path);
        break :symbols try .parse(gpa, game, symbols_text);
    } else .{ .game = game };
    defer symbols.deinit(gpa);

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = gpa });
    defer pool.deinit();

    var code: std.ArrayListUnmanaged(u8) = .empty;
    defer code.deinit(gpa);

    var language: lang.Language = undefined;
    var language_ptr: utils.SafeUndefined(*const lang.Language) = .undef;
    if (args.options.scrp == .decode or
        args.options.encd == .decode or
        args.options.excd == .decode or
        args.options.lsc2 == .decode)
    {
        language = lang.buildLanguage(game);
        language_ptr = .{ .defined = &language };
    }

    const index, const index_buf = try extractIndex(gpa, diagnostic, input_dir, index_name, game, output_dir, &code);
    defer gpa.free(index_buf);

    var cx: Context = .{
        .gpa = gpa,
        .pool = &pool,
        .options = args.options,
        .game = game,
        .language = language_ptr,
        .symbols = &symbols,
        .index = &index,
        .output_dir = output_dir,
        .stats = .initFill(0),
    };

    for (0..games.numberOfDisks(game)) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try extractDisk(&cx, diagnostic, input_dir, index_name, disk_number, &code);
    }

    if (symbols.globals.len() != 0) {
        for (0..symbols.globals.len()) |i|
            if (symbols.globals.get(i)) |name|
                try code.writer(gpa).print("var {s} @ {}\n", .{ name, i });
    }

    try fs.writeFileZ(output_dir, "project.scu", code.items);

    return cx.stats;
}

const Index = struct {
    maxs: Maxs,
    directories: Directories,
    lfl_offsets: utils.SafeManyPointer([*]u32),
    lfl_disks: utils.SafeUndefined(utils.SafeManyPointer([*]u8)),
    room_names: RoomNames,
};

const Maxs = extern struct {
    variables: u16,
    unknown_02: u16,
    room_variables: u16,
    objects_in_room: u16,
    arrays: u16,
    unknown_0a: u16,
    unknown_0c: u16,
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
    unknown_28: u16,
    talkies: u16,
};

const Directories = struct {
    room_images: Directory,
    rooms: Directory,
    scripts: Directory,
    sounds: Directory,
    costumes: Directory,
    charsets: Directory,
    images: Directory,
    talkies: Directory,
};

const Directory = struct {
    rooms: utils.SafeManyPointer([*]u8),
    offsets: utils.SafeManyPointer([*]u32),
    sizes: utils.SafeManyPointer([*]u32),

    pub const empty: Directory = .{
        .rooms = .empty,
        .offsets = .empty,
        .sizes = .empty,
    };
};

const RoomNames = struct {
    buffer: utils.SafeManyPointer([*]u8),
    starts: utils.SafeManyPointer([*]u16),
    lens: utils.SafeManyPointer([*]u8),

    fn get(self: *const RoomNames, room_number: u8) []const u8 {
        const len = self.lens.get(room_number);
        if (len == 0) return "";
        const start = self.starts.get(room_number);
        return self.buffer.use()[start..][0..len];
    }
};

fn extractIndex(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !struct { Index, []u8 } {
    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, index_name);

    const raw = try fs.readFileZ(gpa, input_dir, index_name);
    defer gpa.free(raw);
    for (raw) |*b|
        b.* ^= xor_key;

    var in = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader2(&in, &diag);

    const result_buf = try gpa.alloc(u8, raw.len);
    errdefer gpa.free(result_buf);
    var fba: std.heap.FixedBufferAllocator = .init(result_buf);

    try code.appendSlice(gpa, "index {\n");

    // MAXS

    const maxs_raw = try blocks.expect("MAXS").bytes();
    if (maxs_raw.len != games.maxsLen(game))
        return error.BadData;

    var maxs: Maxs = undefined;
    const maxs_present_bytes = std.mem.asBytes(&maxs)[0..maxs_raw.len];
    const maxs_missing_bytes = std.mem.asBytes(&maxs)[maxs_raw.len..@sizeOf(Maxs)];
    @memcpy(maxs_present_bytes, maxs_raw);
    @memset(maxs_missing_bytes, 0);

    try writeRawIndexBlock(gpa, output_dir, code, blockId("MAXS"), maxs_present_bytes);

    inline for (comptime std.meta.fieldNames(Maxs)) |f|
        diag.trace(@intCast(in.pos), "  {s} = {}", .{ f, @field(maxs, f) });

    // DIR*

    const diri = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRI"), maxs.rooms);
    const dirr = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRR"), maxs.rooms);
    const dirs = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRS"), maxs.scripts);
    const dirn = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRN"), maxs.sounds);
    const dirc = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRC"), maxs.costumes);
    const dirf = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRF"), maxs.charsets);
    const dirm = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRM"), maxs.images);
    const dirt: Directory = if (games.hasTalkies(game))
        try readDirectory(gpa, &fba, &blocks, code, blockId("DIRT"), maxs.talkies)
    else
        .empty;

    // DLFL

    const dlfl_raw = try blocks.expect("DLFL").bytes();
    if (dlfl_raw.len != 2 + 4 * maxs.rooms)
        return error.BadData;
    if (std.mem.readInt(u16, dlfl_raw[0..2], .little) != maxs.rooms)
        return error.BadData;
    const lfl_offsets = try fba.allocator().alloc(u32, maxs.rooms);
    @memcpy(lfl_offsets, std.mem.bytesAsSlice(u32, dlfl_raw[2..]));

    try code.appendSlice(gpa, "    index-block \"DLFL\"\n");

    for (lfl_offsets, 0..) |off, i|
        diag.trace(@intCast(in.pos), "  {:>3}: 0x{x:0>8}", .{ i, off });

    // DISK

    var lfl_disks: utils.SafeUndefined(utils.SafeManyPointer([*]u8)) = .undef;
    if (games.hasDisk(game)) {
        const disk_raw = try blocks.expect("DISK").bytes();
        if (disk_raw.len != 2 + maxs.rooms)
            return error.BadData;
        if (std.mem.readInt(u16, disk_raw[0..2], .little) != maxs.rooms)
            return error.BadData;
        const lfl_disks_slice = try fba.allocator().dupe(u8, disk_raw[2..]);
        lfl_disks = .{ .defined = .init(lfl_disks_slice) };

        try code.appendSlice(gpa, "    index-block \"DISK\"\n");

        for (lfl_disks.defined.slice(maxs.rooms), 0..) |disk, i|
            diag.trace(@intCast(in.pos), "  {:>3}: {:>3}", .{ i, disk });
    }

    // SVER

    if (games.hasIndexSver(game))
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, blockId("SVER"));

    // RNAM

    const room_names = try readRoomNames(&in, &blocks, &diag, maxs.rooms, &fba);
    try code.appendSlice(gpa, "    index-block \"RNAM\"\n");

    // remaining blocks

    for ([_]BlockId{ blockId("DOBJ"), blockId("AARY") }) |id|
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, id);
    if (games.hasIndexInib(game))
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, blockId("INIB"));

    try blocks.finishEof();

    try code.appendSlice(gpa, "}\n");

    if (Diagnostic.enable_trace) {
        for (&[_]struct { []const u8, *const Directory, u16 }{
            .{ "DIRI", &diri, maxs.rooms },
            .{ "DIRR", &dirr, maxs.rooms },
            .{ "DIRS", &dirs, maxs.scripts },
            .{ "DIRN", &dirn, maxs.sounds },
            .{ "DIRC", &dirc, maxs.costumes },
            .{ "DIRF", &dirf, maxs.charsets },
            .{ "DIRM", &dirm, maxs.images },
            .{ "DIRT", &dirt, maxs.talkies },
        }) |dir_info| {
            const block_id, const dir, const len = dir_info;
            diag.trace(@intCast(in.pos), "{s}", .{block_id});
            for (
                dir.rooms.slice(len),
                dir.offsets.slice(len),
                dir.sizes.slice(len),
                0..,
            ) |room, offset, size, i|
                diag.trace(
                    @intCast(in.pos),
                    "  {:>5}: {:>3} 0x{x:0>8} (0x{x:0>8}) 0x{x:0>8}",
                    .{ i, room, offset, lfl_offsets[room] + offset, size },
                );
        }
    }

    const index: Index = .{
        .maxs = maxs,
        .directories = .{
            .room_images = diri,
            .rooms = dirr,
            .scripts = dirs,
            .sounds = dirn,
            .costumes = dirc,
            .charsets = dirf,
            .images = dirm,
            .talkies = dirt,
        },
        .lfl_offsets = .init(lfl_offsets),
        .lfl_disks = lfl_disks,
        .room_names = room_names,
    };
    return .{ index, result_buf };
}

fn readDirectory(
    gpa: std.mem.Allocator,
    fba: *std.heap.FixedBufferAllocator,
    blocks: anytype,
    code: *std.ArrayListUnmanaged(u8),
    block_id: BlockId,
    expected_len: u32,
) !Directory {
    const block_raw = try blocks.expect(block_id).bytes();

    try code.writer(gpa).print("    index-block \"{s}\"\n", .{fmtBlockId(&block_id)});

    var in = std.io.fixedBufferStream(block_raw);

    const len = try in.reader().readInt(u16, .little);
    if (len != expected_len) return error.BadData;

    const rooms_src = try io.readInPlace(&in, len);
    const rooms = try fba.allocator().dupe(u8, rooms_src);

    const offsets_raw = try io.readInPlace(&in, len * 4);
    const offsets = try fba.allocator().alloc(u32, len);
    @memcpy(std.mem.sliceAsBytes(offsets), offsets_raw);

    const sizes_raw = try io.readInPlace(&in, len * 4);
    const sizes = try fba.allocator().alloc(u32, len);
    @memcpy(std.mem.sliceAsBytes(sizes), sizes_raw);

    return .{
        .rooms = .init(rooms),
        .offsets = .init(offsets),
        .sizes = .init(sizes),
    };
}

fn readRoomNames(
    in: anytype,
    blocks: anytype,
    diag: *const Diagnostic.ForBinaryFile,
    num_rooms: u16,
    fba: *std.heap.FixedBufferAllocator,
) !RoomNames {
    const rnam = try blocks.expect("RNAM").block();
    if (rnam.size > 0xffff) return error.BadData; // so we can index with u16

    const starts = try fba.allocator().alloc(u16, num_rooms);
    const lens = try fba.allocator().alloc(u8, num_rooms);
    // we only have to clear lens, because starts is not accessed unless lens is nonzero
    @memset(lens, 0);
    const buffer_start = fba.buffer[fba.end_index..].ptr;

    while (true) {
        const number = try in.reader().readInt(u16, .little);
        if (number == 0) break;
        const name_len = std.mem.indexOfScalar(u8, in.buffer[in.pos..], 0) orelse
            return error.BadData;
        const name_src_z = try io.readInPlace(in, name_len + 1);
        const name = try fba.allocator().dupe(u8, name_src_z[0..name_len :0]);
        starts[number] = @intCast(name.ptr - buffer_start);
        lens[number] = std.math.cast(u8, name_len) orelse return error.BadData;
        diag.trace(@intCast(in.pos), "  {:>3}: {s}", .{ number, std.fmt.fmtSliceEscapeLower(name) });
    }

    const buffer_len = fba.buffer[fba.end_index..].ptr - buffer_start;
    return .{
        .buffer = .init(buffer_start[0..buffer_len]),
        .starts = .init(starts),
        .lens = .init(lens),
    };
}

fn extractRawIndexBlock(
    gpa: std.mem.Allocator,
    blocks: anytype,
    output_dir: anytype,
    code: *std.ArrayListUnmanaged(u8),
    block_id: BlockId,
) !void {
    const data = try blocks.expect(block_id).bytes();
    try writeRawIndexBlock(gpa, output_dir, code, block_id, data);
}

fn writeRawIndexBlock(
    gpa: std.mem.Allocator,
    output_dir: anytype,
    code: *std.ArrayListUnmanaged(u8),
    block_id: BlockId,
    data: []const u8,
) !void {
    var filename_buf: ["index_XXXX.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "index_{s}.bin",
        .{fmtBlockId(&block_id)},
    );
    try fs.writeFileZ(output_dir, filename, data);

    try code.writer(gpa).print(
        "    raw-block \"{s}\" \"{s}\"\n",
        .{ fmtBlockId(&block_id), filename },
    );
}

const Context = struct {
    gpa: std.mem.Allocator,
    pool: *std.Thread.Pool,
    options: Options,
    game: games.Game,
    language: utils.SafeUndefined(*const lang.Language),
    symbols: *const Symbols,
    index: *const Index,
    output_dir: std.fs.Dir,
    stats: std.EnumArray(Stat, u16),

    fn incStat(self: *Context, stat: Stat) void {
        const old = @atomicRmw(u16, self.stats.getPtr(stat), .Add, 1, .monotonic);
        std.debug.assert(old != std.math.maxInt(u16)); // assert no overflow
    }

    fn incStatOpt(self: *Context, stat: ?Stat) void {
        if (stat) |s| self.incStat(s);
    }
};

fn extractDisk(
    cx: *Context,
    diagnostic: *Diagnostic,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    disk_number: u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    try code.writer(cx.gpa).print("disk {} {{\n", .{disk_number});

    var disk_name_buf: pathf.Path = .{};
    const disk_name = try pathf.append(&disk_name_buf, index_name);
    games.pointPathToDisk(cx.game, disk_name.full(), disk_number);

    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, disk_name.full());

    const in_file = try input_dir.openFileZ(disk_name.full(), .{});
    defer in_file.close();
    const in_xor = io.xorReader(in_file.reader(), xor_key);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in = std.io.countingReader(in_buf.reader());

    var file_blocks = streamingBlockReader(&in, &diag);

    const lecf = try file_blocks.expect("LECF").block();
    var lecf_blocks = streamingBlockReader(&in, &diag);

    while (in.bytes_read < lecf.end()) {
        const lflf = try lecf_blocks.expect("LFLF").block();
        try extractRoom(cx, disk_number, &in, &diag, lflf.end(), code);
    }

    try lecf_blocks.finish(lecf.end());

    try file_blocks.finishEof();

    try code.appendSlice(cx.gpa, "}\n");
}

const max_room_code_chunks = 5120;

const Event = union(enum) {
    end,
    err,
    code_chunk: struct {
        index: u32,
        section: Section,
        code: std.ArrayListUnmanaged(u8),
    },
};

const Section = enum {
    top,
    global_scripts,
    local_scripts,
    enter_script,
    exit_script,
    bottom,
};

fn extractRoom(
    cx: *Context,
    disk_number: u8,
    in: anytype,
    disk_diag: *const Diagnostic.ForBinaryFile,
    lflf_end: u32,
    project_code: *std.ArrayListUnmanaged(u8),
) !void {
    const room_number = findRoomNumber(cx.game, cx.index, disk_number, @intCast(in.bytes_read)) orelse
        return error.BadData;

    const diag = disk_diag.child(0, .{ .glob = .{ blockId("LFLF"), room_number } });

    var events: sync.Channel(Event, 16) = .init;

    try cx.pool.spawn(readRoomJob, .{ cx, in, &diag, lflf_end, room_number, &events });

    try emitRoom(cx, room_number, project_code, &events);
}

fn findRoomNumber(game: games.Game, index: *const Index, disk_number: u8, offset: u32) ?u8 {
    const len = index.maxs.rooms;

    if (!games.hasDisk(game)) {
        for (index.lfl_offsets.slice(len), 0..) |off, i|
            if (off == offset)
                return @intCast(i);
        return null;
    }

    for (index.lfl_offsets.slice(len), index.lfl_disks.defined.slice(len), 0..) |off, dsk, i|
        if (off == offset and dsk == disk_number)
            return @intCast(i);
    return null;
}

const RoomContext = struct {
    cx: *Context,
    room_number: u8,
    room_path: []const u8,
    room_dir: std.fs.Dir,
    room_palette: utils.SafeUndefined([0x300]u8),
    events: *sync.Channel(Event, 16),
    pending_jobs: std.atomic.Value(u32),
    next_chunk_index: u16,

    fn claimChunkIndex(self: *RoomContext) !u16 {
        const result = self.next_chunk_index;
        if (result >= max_room_code_chunks) return error.Overflow;
        self.next_chunk_index += 1;
        return result;
    }

    fn sendSync(self: *RoomContext, section: Section, code: std.ArrayListUnmanaged(u8)) !void {
        self.events.send(.{ .code_chunk = .{
            .index = try self.claimChunkIndex(),
            .section = section,
            .code = code,
        } });
    }

    fn sendSyncFmt(
        self: *RoomContext,
        section: Section,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        const code = try std.fmt.allocPrint(self.cx.gpa, fmt, args);
        try self.sendSync(section, .fromOwnedSlice(code));
    }

    fn sendChunk(
        self: *const RoomContext,
        chunk_index: u16,
        section: Section,
        code: std.ArrayListUnmanaged(u8),
    ) void {
        self.events.send(.{ .code_chunk = .{
            .index = chunk_index,
            .section = section,
            .code = code,
        } });
    }
};

fn readRoomJob(
    cx: *Context,
    in: anytype,
    diag: *const Diagnostic.ForBinaryFile,
    lflf_end: u32,
    room_number: u8,
    events: *sync.Channel(Event, 16),
) void {
    // store these a level up so they outlive the jobs
    var room_dir: ?std.fs.Dir = null;
    defer if (room_dir) |*d| d.close();

    var rcx: RoomContext = .{
        .cx = cx,
        .room_number = room_number,
        .room_path = undefined, // set below
        .room_dir = undefined, // set below
        .room_palette = .undef,
        .events = events,
        .pending_jobs = .init(0),
        .next_chunk_index = 0,
    };

    (blk: {
        rcx.room_path = cx.index.room_names.get(room_number);
        fs.makeDirIfNotExist(cx.output_dir, rcx.room_path) catch |err| break :blk err;
        room_dir = cx.output_dir.openDir(rcx.room_path, .{}) catch |err| break :blk err;
        rcx.room_dir = room_dir.?;

        readRoomInner(&rcx, in, diag, lflf_end) catch |err| break :blk err;
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(@intCast(in.bytes_read), "room {}: unexpected error: {s}", .{room_number}, err);
        events.send(.err);
    };

    diag.trace(@intCast(in.bytes_read), "waiting for jobs", .{});
    while (true) {
        const pending = rcx.pending_jobs.load(.acquire);
        if (pending == 0) break;
        std.Thread.Futex.wait(&rcx.pending_jobs, pending);
    }
    diag.trace(@intCast(in.bytes_read), "all jobs finished", .{});

    events.send(.end);
}

fn readRoomInner(
    cx: *RoomContext,
    in: anytype,
    diag: *const Diagnostic.ForBinaryFile,
    lflf_end: u32,
) !void {
    var lflf_blocks = streamingBlockReader(in, diag);

    const rmim_chunk_index = try cx.claimChunkIndex();
    const rmim_block = try lflf_blocks.expect("RMIM").block();
    var rmim_raw = try cx.cx.gpa.alloc(u8, rmim_block.size);
    defer cx.cx.gpa.free(rmim_raw);
    try in.reader().readNoEof(rmim_raw);

    {
        const rmda = try lflf_blocks.expect("RMDA").block();
        const room_palette = try extractRmda(cx, in, diag, &rmda);
        cx.room_palette = .{ .defined = room_palette };
    }

    // extract RMIM after RMDA since it needs the room palette
    try spawnBlockJob(extractRmimJob, cx, diag, &rmim_block, rmim_raw, rmim_chunk_index);
    rmim_raw.len = 0; // ownership was moved to the job, don't free it here

    while (in.bytes_read < lflf_end) {
        const block = try lflf_blocks.next().block();
        try readBlockAndSpawn(extractGlobJob, cx, in, diag, &block);
    }

    try lflf_blocks.finish(lflf_end);

    const room_vars_chunk_index = try cx.claimChunkIndex();
    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.cx.pool.spawn(runRoomVarsJob, .{ cx, diag.diagnostic, room_vars_chunk_index });
}

fn extractRmda(
    cx: *RoomContext,
    in: anytype,
    diag: *const Diagnostic.ForBinaryFile,
    rmda: *const Block,
) ![0x300]u8 {
    try cx.sendSyncFmt(.top, "rmda {{\n", .{});

    var apal_opt: ?[0x300]u8 = null;

    var rmda_blocks = streamingBlockReader(in, diag);

    while (in.bytes_read < rmda.end()) {
        const block = try rmda_blocks.next().block();
        switch (block.id) {
            blockId("PALS") => {
                var code: std.ArrayListUnmanaged(u8) = .empty;
                errdefer code.deinit(cx.cx.gpa);
                if (apal_opt != null) return error.BadData;
                apal_opt = try extractPals(cx, in, diag, &block, &code);
                try cx.sendSync(.top, code);
            },
            blockId("EXCD"), blockId("ENCD"), blockId("LSC2") => {
                try readBlockAndSpawn(extractRmdaChildJob, cx, in, diag, &block);
            },
            else => {
                var code: std.ArrayListUnmanaged(u8) = .empty;
                errdefer code.deinit(cx.cx.gpa);
                try extractRawBlock(cx.cx.gpa, in, &block, cx.room_dir, cx.room_path, &code);
                try cx.sendSync(.top, code);
            },
        }
    }

    try rmda_blocks.finish(rmda.end());

    try cx.sendSyncFmt(.top, "}}\n", .{});

    return apal_opt orelse return error.BadData;
}

fn extractPals(
    cx: *const RoomContext,
    in: anytype,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    code: *std.ArrayListUnmanaged(u8),
) ![0x300]u8 {
    const diag = disk_diag.child(block.start, .{ .block_id = blockId("PALS") });

    const expected_len = 796;
    if (block.size != expected_len) return error.BadData;
    const pals_raw = try in.reader().readBytesNoEof(expected_len);
    var pals_stream = std.io.fixedBufferStream(&pals_raw);
    var pals_blocks = fixedBlockReader2(&pals_stream, &diag);

    const pals = try pals_blocks.expect("WRAP").block();
    var wrap_blocks = fixedBlockReader2(&pals_stream, &diag);

    const off = try wrap_blocks.expect("OFFS").value(u32);
    if (off.* != 12) return error.BadData;

    const apal = try wrap_blocks.expect("APAL").value([0x300]u8);

    try wrap_blocks.finish(pals.end());
    try pals_blocks.finishEof();

    try writeRawBlock(cx.cx.gpa, block, &pals_raw, cx.room_dir, cx.room_path, code);

    return apal.*;
}

const BlockJob = fn (
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
) anyerror!void;

fn readBlockAndSpawn(
    job: BlockJob,
    cx: *RoomContext,
    in: anytype,
    diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
) !void {
    const raw = try cx.cx.gpa.alloc(u8, block.size);
    errdefer cx.cx.gpa.free(raw);
    try in.reader().readNoEof(raw);

    const chunk_index = try cx.claimChunkIndex();

    try spawnBlockJob(job, cx, diag, block, raw, chunk_index);
}

fn spawnBlockJob(
    job: BlockJob,
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
) !void {
    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.cx.pool.spawn(runBlockJob, .{ job, cx, diag, block.*, raw, chunk_index });
}

fn runBlockJob(
    job: BlockJob,
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block: Block,
    raw: []const u8,
    chunk_index: u16,
) void {
    defer cx.cx.gpa.free(raw);

    job(cx, diag, &block, raw, chunk_index) catch |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(block.offset(), "unexpected error: {s}", .{}, err);
        cx.events.send(.err);
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn extractRmimJob(
    cx: *const RoomContext,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
) !void {
    std.debug.assert(disk_diag.offset == 0);
    var diag = disk_diag.child(block.start, .{ .block_id = blockId("RMIM") });
    diag.cap_level = true;

    var code: std.ArrayListUnmanaged(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    if (cx.cx.options.rmim == .decode)
        if (tryDecodeAndSend(extractRmimInner, cx, &diag, .{raw}, &code, chunk_index, .top))
            return;

    // If decoding failed or was skipped, extract as raw
    try writeRawGlob(cx.cx.gpa, block, cx.room_number, raw, cx.room_dir, cx.room_path, &code);
    cx.sendChunk(chunk_index, .top, code);
}

fn extractRmimInner(
    cx: *const RoomContext,
    _: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var decoded = try rmim.decode(cx.cx.gpa, raw, &cx.room_palette.defined);
    defer decoded.deinit(cx.cx.gpa);

    var path_buf: [Ast.max_room_name_len + "/rmim.bmp".len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "{s}/rmim.bmp", .{cx.room_path}) catch unreachable;
    try fs.writeFileZ(cx.room_dir, "rmim.bmp", decoded.bmp.items);

    try code.writer(cx.cx.gpa).print("rmim {} \"{s}\"\n", .{ decoded.compression, path });
}

fn extractRmdaChildJob(
    cx: *RoomContext,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
) !void {
    cx.cx.incStat(switch (block.id) {
        blockId("EXCD") => .excd_total,
        blockId("ENCD") => .encd_total,
        blockId("LSC2") => .lsc2_total,
        else => unreachable,
    });

    std.debug.assert(disk_diag.offset == 0);
    var diag = disk_diag.child(block.start, .{ .block_id = block.id });
    diag.cap_level = true;

    var code: std.ArrayListUnmanaged(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    // First try to decode
    switch (block.id) {
        blockId("EXCD"), blockId("ENCD") => {
            if (extractEncdExcd(cx, &diag, block.id, raw, &code, chunk_index))
                return;
        },
        blockId("LSC2") => {
            if (extractLscr(cx, &diag, raw, &code, chunk_index))
                return;
        },
        else => unreachable, // This is only called for the above block ids
    }

    // If decoding failed or was skipped, extract as raw
    try writeRawBlock(cx.cx.gpa, block, raw, cx.room_dir, cx.room_path, &code);
    cx.sendChunk(chunk_index, .top, code);

    cx.cx.incStat(switch (block.id) {
        blockId("EXCD") => .excd_raw,
        blockId("ENCD") => .encd_raw,
        blockId("LSC2") => .lsc2_raw,
        else => unreachable,
    });
}

const EncdExcd = enum {
    encd,
    excd,

    fn from(block_id: BlockId) EncdExcd {
        return switch (block_id) {
            blockId("ENCD") => .encd,
            blockId("EXCD") => .excd,
            else => unreachable,
        };
    }
};

fn extractEncdExcd(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block_id: BlockId,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    chunk_index: u16,
) bool {
    const edge = EncdExcd.from(block_id);

    const option = switch (edge) {
        .encd => cx.cx.options.encd,
        .excd => cx.cx.options.excd,
    };
    if (option != .decode) return false;

    const section: Section = switch (edge) {
        .encd => .enter_script,
        .excd => .exit_script,
    };

    if (cx.cx.options.script == .decompile and
        tryDecode("decompile", extractEncdExcdDecompile, cx, diag, .{ edge, raw }, code))
    {
        cx.sendChunk(chunk_index, section, code.*);
        return true;
    }
    if (tryDecode("disassemble", extractEncdExcdDisassemble, cx, diag, .{ edge, raw }, code)) {
        cx.sendChunk(chunk_index, section, code.*);
        return true;
    }
    return false;
}

fn extractEncdExcdDisassemble(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    edge: EncdExcd,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;
    const id: Symbols.ScriptId = switch (edge) {
        .encd => .{ .enter = .{ .room = cx.room_number } },
        .excd => .{ .exit = .{ .room = cx.room_number } },
    };

    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(cx.cx.gpa);

    disasm.disassemble(cx.cx.gpa, cx.cx.language.defined, id, raw, cx.cx.symbols, out.writer(cx.cx.gpa), &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: ["encd.s".len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "{s}.s", .{@tagName(edge)}) catch unreachable;
    try fs.writeFileZ(cx.room_dir, path, out.items);

    try code.writer(cx.cx.gpa).print(
        "{s} \"{s}/{s}\"\n",
        .{ @tagName(edge), cx.room_path, path },
    );

    cx.cx.incStat(switch (edge) {
        .encd => .encd_disassemble,
        .excd => .excd_disassemble,
    });
    diagnostic.flushStats(cx.cx, diag);
}

fn extractEncdExcdDecompile(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    edge: EncdExcd,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const keyword = switch (edge) {
        .encd => "enter",
        .excd => "exit",
    };
    try code.writer(cx.cx.gpa).print("{s} {{\n", .{keyword});
    try decompile.run(cx.cx.gpa, diag, cx.cx.symbols, raw, code);
    try code.appendSlice(cx.cx.gpa, "}\n");

    cx.cx.incStat(switch (edge) {
        .encd => .encd_decompile,
        .excd => .excd_decompile,
    });
}

fn extractLscr(
    cx: *const RoomContext,
    diag: *Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    chunk_index: u16,
) bool {
    if (cx.cx.options.lsc2 == .raw) return false;

    const script_number, const bytecode = (hdr: {
        if (raw.len < 4) break :hdr error.EndOfStream;
        const script_number_u32 = std.mem.readInt(u32, raw[0..4], .little);
        const bytecode = raw[4..];
        const script_number = std.math.cast(u16, script_number_u32) orelse
            break :hdr error.Overflow;
        break :hdr .{ script_number, bytecode };
    }) catch |err|
        return handleDecodeResult(err, "decode", diag, code);

    // mild hack: patch the log context now that we know the script number
    diag.section = .{ .glob = .{ blockId("LSC2"), script_number } };

    if (cx.cx.options.script == .decompile and
        tryDecode("decompile", extractLscrDecompile, cx, diag, .{ script_number, bytecode }, code))
    {
        cx.sendChunk(chunk_index, .local_scripts, code.*);
        return true;
    }
    if (tryDecode("disassemble", extractLscrDisassemble, cx, diag, .{ script_number, bytecode }, code)) {
        cx.sendChunk(chunk_index, .local_scripts, code.*);
        return true;
    }
    return false;
}

fn extractLscrDisassemble(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    script_number: u32,
    bytecode: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(cx.cx.gpa);

    const id: Symbols.ScriptId = .{ .local = .{
        .room = cx.room_number,
        .number = script_number,
    } };
    disasm.disassemble(cx.cx.gpa, cx.cx.language.defined, id, bytecode, cx.cx.symbols, out.writer(cx.cx.gpa), &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: ["lscr0000.s".len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "lscr{:0>4}.s", .{script_number}) catch unreachable;
    try fs.writeFileZ(cx.room_dir, path, out.items);

    try code.writer(cx.cx.gpa).print(
        "lscr {} \"{s}/{s}\"\n",
        .{ script_number, cx.room_path, path },
    );

    cx.cx.incStat(.lsc2_disassemble);
    diagnostic.flushStats(cx.cx, diag);
}

fn extractLscrDecompile(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    script_number: u32,
    bytecode: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    try code.writer(cx.cx.gpa).print("local-script {} {{\n", .{script_number});
    try decompile.run(cx.cx.gpa, diag, cx.cx.symbols, bytecode, code);
    try code.appendSlice(cx.cx.gpa, "}\n");

    cx.cx.incStat(.lsc2_decompile);
}

fn runRoomVarsJob(cx: *RoomContext, diagnostic: *Diagnostic, chunk_index: u16) void {
    writeRoomVarsJob(cx, chunk_index) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
        cx.events.send(.err);
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn writeRoomVarsJob(cx: *RoomContext, chunk_index: u16) !void {
    var code: std.ArrayListUnmanaged(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    try writeRoomVarsInner(cx, &code);
    cx.sendChunk(chunk_index, .top, code);
}

fn writeRoomVarsInner(cx: *RoomContext, code: *std.ArrayListUnmanaged(u8)) !void {
    const room = cx.cx.symbols.getRoom(cx.room_number) orelse return;
    for (0..room.vars.len()) |i|
        if (room.vars.get(i)) |name|
            try code.writer(cx.cx.gpa).print("var {s} @ {}\n", .{ name, i });
}

fn findGlobNumber(
    index: *const Index,
    block_id: BlockId,
    room_number: u8,
    offset_in_disk: u32,
) ?u16 {
    const dir, const dir_len = switch (block_id) {
        // XXX: this list is duplicated in emit
        blockId("RMIM") => .{ &index.directories.room_images, index.maxs.rooms },
        blockId("RMDA") => .{ &index.directories.rooms, index.maxs.rooms },
        blockId("SCRP") => .{ &index.directories.scripts, index.maxs.scripts },
        blockId("SOUN"),
        blockId("DIGI"),
        blockId("TALK"),
        blockId("WSOU"),
        => .{ &index.directories.sounds, index.maxs.sounds },
        blockId("AKOS") => .{ &index.directories.costumes, index.maxs.costumes },
        blockId("CHAR") => .{ &index.directories.charsets, index.maxs.charsets },
        blockId("AWIZ"), blockId("MULT") => .{ &index.directories.images, index.maxs.images },
        blockId("TLKE") => .{ &index.directories.talkies, index.maxs.talkies },
        else => return null,
    };
    const offset_in_room = offset_in_disk - index.lfl_offsets.get(room_number);
    for (dir.rooms.slice(dir_len), dir.offsets.slice(dir_len), 0..) |r, o, i|
        if (r == room_number and o == offset_in_room)
            return @intCast(i);
    return null;
}

fn extractGlobJob(
    cx: *const RoomContext,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
) !void {
    cx.cx.incStatOpt(switch (block.id) {
        blockId("SCRP") => .scrp_total,
        else => null,
    });

    var code: std.ArrayListUnmanaged(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    const glob_number = findGlobNumber(cx.cx.index, block.id, cx.room_number, block.offset()) orelse {
        // This should normally be impossible, but there's a glitched CHAR block
        // in soccer that we need to handle in order to round-trip.
        disk_diag.trace(block.offset(), "glob missing from directory", .{});
        try writeRawBlock(cx.cx.gpa, block, raw, cx.room_dir, cx.room_path, &code);
        cx.sendChunk(chunk_index, .bottom, code);
        return;
    };

    disk_diag.trace(block.offset(), "glob number {}", .{glob_number});

    std.debug.assert(disk_diag.offset == 0);
    var diag = disk_diag.child(block.start, .{ .glob = .{ block.id, glob_number } });
    diag.cap_level = true;

    // First try to decode
    switch (block.id) {
        blockId("SCRP") => {
            if (extractScrp(cx, &diag, glob_number, raw, &code, chunk_index))
                return;
        },
        blockId("AWIZ") => if (cx.cx.options.awiz == .decode)
            if (tryDecodeAndSend(extractAwiz, cx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        blockId("MULT") => if (cx.cx.options.mult == .decode)
            if (tryDecodeAndSend(extractMult, cx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        blockId("AKOS") => if (cx.cx.options.akos == .decode)
            if (tryDecodeAndSend(extractAkos, cx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        else => {},
    }

    // If decoding failed or was skipped, extract as raw

    try writeRawGlob(cx.cx.gpa, block, glob_number, raw, cx.room_dir, cx.room_path, &code);
    cx.sendChunk(chunk_index, .bottom, code);

    cx.cx.incStatOpt(switch (block.id) {
        blockId("SCRP") => .scrp_raw,
        else => null,
    });
}

fn tryDecode(
    decoder_name: []const u8,
    decodeFn: anytype,
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    decode_args: anytype,
    code: *std.ArrayListUnmanaged(u8),
) bool {
    const result = @call(.auto, decodeFn, .{ cx, diag } ++ decode_args ++ .{code});
    return handleDecodeResult(result, decoder_name, diag, code);
}

// break out the non-generic code for better codegen
fn handleDecodeResult(
    result: anyerror!void,
    decoder_name: []const u8,
    diag: *const Diagnostic.ForBinaryFile,
    code: *std.ArrayListUnmanaged(u8),
) bool {
    if (result) {
        return true;
    } else |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(0, "unexpected error: {s}", .{}, err);

        // Clear any partial results from a failure
        code.clearRetainingCapacity();

        diag.info(0, "{s} failed", .{decoder_name});
        return false;
    }
}

fn tryDecodeAndSend(
    decodeFn: anytype,
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    decode_args: anytype,
    code: *std.ArrayListUnmanaged(u8),
    chunk_index: u16,
    section: Section,
) bool {
    const result = @call(.auto, decodeFn, .{ cx, diag } ++ decode_args ++ .{code});
    return handleDecodeAndSendResult(result, cx, diag, code, chunk_index, section);
}

// break out the non-generic code for better codegen
fn handleDecodeAndSendResult(
    result: anyerror!void,
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    code: *std.ArrayListUnmanaged(u8),
    chunk_index: u16,
    section: Section,
) bool {
    if (handleDecodeResult(result, "decode", diag, code)) {
        cx.sendChunk(chunk_index, section, code.*);
        return true;
    }
    return false;
}

fn extractScrp(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    chunk_index: u16,
) bool {
    if (cx.cx.options.scrp == .raw)
        return false;
    if (cx.cx.options.script == .decompile and
        tryDecode("decompile", extractScrpDecompile, cx, diag, .{ glob_number, raw }, code))
    {
        cx.sendChunk(chunk_index, .global_scripts, code.*);
        return true;
    }
    if (tryDecode("disassemble", extractScrpDisassemble, cx, diag, .{ glob_number, raw }, code)) {
        cx.sendChunk(chunk_index, .global_scripts, code.*);
        return true;
    }
    return false;
}

fn extractScrpDisassemble(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;

    const id: Symbols.ScriptId = .{ .global = glob_number };

    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(cx.cx.gpa);

    disasm.disassemble(cx.cx.gpa, cx.cx.language.defined, id, raw, cx.cx.symbols, out.writer(cx.cx.gpa), &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: ["scrp0000.s".len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "scrp{:0>4}.s", .{glob_number}) catch unreachable;
    try fs.writeFileZ(cx.room_dir, path, out.items);

    try code.writer(cx.cx.gpa).print("scrp {} \"{s}/{s}\"\n", .{ glob_number, cx.room_path, path });

    cx.cx.incStat(.scrp_disassemble);
    diagnostic.flushStats(cx.cx, diag);
}

/// Adapts newer Diagnostic into the old diagnostic type that disasm expects
const DisasmDiagnostic = struct {
    unknown_byte: bool,

    const init: DisasmDiagnostic = .{ .unknown_byte = false };

    pub fn warnScriptUnknownByte(self: *@This()) void {
        self.unknown_byte = true;
    }

    fn flushStats(
        self: *const @This(),
        cx: *Context,
        diag: *const Diagnostic.ForBinaryFile,
    ) void {
        if (self.unknown_byte) {
            cx.incStat(.script_unknown_byte);
            diag.info(0, "unknown script byte", .{});
        }
    }
};

fn extractScrpDecompile(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    try code.writer(cx.cx.gpa).print("script {} {{\n", .{glob_number});
    try decompile.run(cx.cx.gpa, diag, cx.cx.symbols, raw, code);
    try code.appendSlice(cx.cx.gpa, "}\n");

    cx.cx.incStat(.scrp_decompile);
}

fn extractAwiz(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var decoded = try awiz.decode(cx.cx.gpa, diag, raw, &cx.room_palette.defined);
    defer decoded.deinit(cx.cx.gpa);

    try code.writer(cx.cx.gpa).print("awiz {} {{\n", .{glob_number});

    var bmp_path_buf: ["image0000.bmp".len + 1]u8 = undefined;
    const bmp_path = std.fmt.bufPrintZ(
        &bmp_path_buf,
        "image{:0>4}.bmp",
        .{glob_number},
    ) catch unreachable;
    try awiz.extractChildren(cx.cx.gpa, cx.room_dir, cx.room_path, code, &decoded, bmp_path, 4);

    try code.appendSlice(cx.cx.gpa, "}\n");
}

fn extractMult(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    try mult.extract(cx.cx.gpa, diag, glob_number, raw, &cx.room_palette.defined, cx.room_dir, cx.room_path, code);
}

fn extractAkos(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    _ = diag;

    var name_buf: ["costume0000".len:0]u8 = undefined;
    const name = std.fmt.bufPrintZ(
        name_buf[0 .. name_buf.len + 1],
        "costume{:0>4}",
        .{glob_number},
    ) catch unreachable;

    try fs.makeDirIfNotExistZ(cx.room_dir, name);

    var dir = try cx.room_dir.openDirZ(name, .{});
    defer dir.close();

    var path_buf: [Ast.max_room_name_len + 1 + name_buf.len:0]u8 = undefined;
    const path = std.fmt.bufPrintZ(
        path_buf[0 .. path_buf.len + 1],
        "{s}/{s}",
        .{ cx.room_path, name },
    ) catch unreachable;

    try code.writer(cx.cx.gpa).print("akos {} {{\n", .{glob_number});
    try akos.decode(cx.cx.gpa, raw, path, dir, code);
    try code.appendSlice(cx.cx.gpa, "}\n");
}

fn extractRawGlob(
    cx: *const RoomContext,
    in: anytype,
    block: *const Block,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const glob_number = findGlobNumber(cx.cx.index, block.id, cx.room_number, block.offset()) orelse
        return error.BadData;

    var filename_buf: ["XXXX_0000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{:0>4}.bin",
        .{ fmtBlockId(&block.id), glob_number },
    );
    const file = try cx.room_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), block.size), file.writer());

    try code.writer(cx.cx.gpa).print(
        "raw-glob \"{s}\" {} \"{s}/{s}\"\n",
        .{ fmtBlockId(&block.id), glob_number, cx.room_path, filename },
    );
}

fn writeRawGlob(
    gpa: std.mem.Allocator,
    block: *const Block,
    glob_number: u16,
    data: []const u8,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var filename_buf: ["XXXX_0000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{:0>4}.bin",
        .{ fmtBlockId(&block.id), glob_number },
    );
    try fs.writeFileZ(output_dir, filename, data);

    try code.writer(gpa).print(
        "raw-glob \"{s}\" {} \"{s}/{s}\"\n",
        .{ fmtBlockId(&block.id), glob_number, output_path, filename },
    );
}

fn extractRawBlock(
    gpa: std.mem.Allocator,
    in: anytype,
    block: *const Block,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var filename_buf: ["XXXX_00000000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{x:0>8}.bin",
        .{ fmtBlockId(&block.id), block.offset() },
    );
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), block.size), file.writer());

    try code.writer(gpa).print(
        "    raw-block \"{s}\" \"{s}/{s}\"\n",
        .{ fmtBlockId(&block.id), output_path, filename },
    );
}

fn writeRawBlock(
    gpa: std.mem.Allocator,
    block: *const Block,
    data: []const u8,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var filename_buf: ["XXXX_00000000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{x:0>8}.bin",
        .{ fmtBlockId(&block.id), block.offset() },
    );
    try fs.writeFileZ(output_dir, filename, data);

    try code.writer(gpa).print(
        "    raw-block \"{s}\" \"{s}/{s}\"\n",
        .{ fmtBlockId(&block.id), output_path, filename },
    );
}

fn emitRoom(
    cx: *const Context,
    room_number: u8,
    project_code: *std.ArrayListUnmanaged(u8),
    events: *sync.Channel(Event, 16),
) !void {
    var chunks: std.BoundedArray(Chunk, max_room_code_chunks) = .{};
    defer for (chunks.slice()) |*chunk| chunk.code.deinit(cx.gpa);

    var ok = true;
    while (true) switch (events.receive()) {
        .end => break,
        .err => ok = false,
        .code_chunk => |chunk| {
            utils.growBoundedArray(&chunks, chunk.index + 1, .{ .section = .top, .code = .empty });
            std.debug.assert(chunks.get(chunk.index).code.items.len == 0);
            chunks.set(chunk.index, .{ .section = chunk.section, .code = chunk.code });
        },
    };

    std.sort.block(Chunk, chunks.slice(), {}, Chunk.sectionAsc);

    const room_name = cx.index.room_names.get(room_number);
    var room_scu_path_buf: [Ast.max_room_name_len + ".scu".len + 1]u8 = undefined;
    const room_scu_path = try std.fmt.bufPrintZ(&room_scu_path_buf, "{s}.scu", .{room_name});

    try project_code.writer(cx.gpa).print(
        "    room {} \"{s}\" \"{s}\"\n",
        .{ room_number, room_name, room_scu_path },
    );

    const room_scu = try cx.output_dir.createFileZ(room_scu_path, .{});
    defer room_scu.close();

    if (!ok)
        try room_scu.writeAll("#error while extracting room; this file is incomplete!\n\n");

    var iovecs_buf: [max_room_code_chunks]std.posix.iovec_const = undefined;
    const iovecs = iovecs_buf[0..chunks.len];
    for (iovecs, chunks.slice()) |*iovec, *chunk|
        iovec.* = .{ .base = chunk.code.items.ptr, .len = chunk.code.items.len };
    try room_scu.writevAll(iovecs);

    if (!ok)
        return error.AddedToDiagnostic;
}

const Chunk = struct {
    section: Section,
    code: std.ArrayListUnmanaged(u8),

    fn sectionAsc(_: void, lhs: Chunk, rhs: Chunk) bool {
        return @intFromEnum(lhs.section) < @intFromEnum(rhs.section);
    }
};
