const builtin = @import("builtin");
const std = @import("std");

const build_options = @import("build_options");

const Ast = @import("Ast.zig");
const Blinkenlights = @import("Blinkenlights.zig");
const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const UsageTracker = @import("UsageTracker.zig");
const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const Block = @import("block_reader.zig").Block;
const FixedBlockReader = @import("block_reader.zig").FixedBlockReader;
const StreamingBlockReader = @import("block_reader.zig").StreamingBlockReader;
const fxbcl = @import("block_reader.zig").fxbcl;
const cliargs = @import("cliargs.zig");
const decompile = @import("decompile.zig");
const Deque = @import("deque.zig").Deque;
const disasm = @import("disasm.zig");
const fs = @import("fs.zig");
const fsd = @import("fsd.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const lang = @import("lang.zig");
const mult = @import("mult.zig");
const music = @import("music.zig");
const obim = @import("obim.zig");
const rmim = @import("rmim.zig");
const sounds = @import("sounds.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub const xor_key = 0x69;

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var index_path_opt: ?[:0]const u8 = null;
    var output_path_opt: ?[:0]const u8 = null;
    var symbols_path: ?[:0]const u8 = null;
    var script: ?ScriptMode = null;
    var annotate: ?YesNo = null;
    var music_option: ?YesNo = null;
    var rainbow: ?YesNo = null;
    var rmim_option: ?RawOrDecode = null;
    var scrp_option: ?RawOrDecode = null;
    var encd_option: ?RawOrDecode = null;
    var excd_option: ?RawOrDecode = null;
    var lscr_option: ?RawOrDecode = null;
    var lsc2_option: ?RawOrDecode = null;
    var obim_option: ?RawOrDecode = null;
    var obcd_option: ?RawOrDecode = null;
    var digi_option: ?RawOrDecode = null;
    var talk_option: ?RawOrDecode = null;
    var awiz_option: ?RawOrDecode = null;
    var mult_option: ?RawOrDecode = null;
    var akos_option: ?RawOrDecode = null;
    var tlke_option: ?RawOrDecode = null;

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
            } else if (std.mem.eql(u8, opt.flag, "annotate")) {
                if (annotate != null) return arg.reportDuplicate();
                annotate = std.meta.stringToEnum(YesNo, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "music")) {
                if (music_option != null) return arg.reportDuplicate();
                music_option = std.meta.stringToEnum(YesNo, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "rainbow")) {
                if (rainbow != null) return arg.reportDuplicate();
                rainbow = std.meta.stringToEnum(YesNo, opt.value) orelse
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
            } else if (std.mem.eql(u8, opt.flag, "lscr")) {
                if (lscr_option != null) return arg.reportDuplicate();
                lscr_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "lsc2")) {
                if (lsc2_option != null) return arg.reportDuplicate();
                lsc2_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "obim")) {
                if (obim_option != null) return arg.reportDuplicate();
                obim_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "obcd")) {
                if (obcd_option != null) return arg.reportDuplicate();
                obcd_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "digi")) {
                if (digi_option != null) return arg.reportDuplicate();
                digi_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "talk")) {
                if (talk_option != null) return arg.reportDuplicate();
                talk_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
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
            } else if (std.mem.eql(u8, opt.flag, "tlke")) {
                if (tlke_option != null) return arg.reportDuplicate();
                tlke_option = std.meta.stringToEnum(RawOrDecode, opt.value) orelse
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
            .annotate = (annotate orelse YesNo.no).toBool(),
            .music = (music_option orelse YesNo.yes).toBool(),
            .rainbow = (rainbow orelse YesNo.no).toBool(),
            .rmim = rmim_option orelse .decode,
            .scrp = scrp_option orelse .decode,
            .encd = encd_option orelse .decode,
            .excd = excd_option orelse .decode,
            .lscr = lscr_option orelse .decode,
            .lsc2 = lsc2_option orelse .decode,
            .obim = obim_option orelse .decode,
            .obcd = obcd_option orelse .decode,
            .digi = digi_option orelse .decode,
            .talk = talk_option orelse .decode,
            .awiz = awiz_option orelse .decode,
            .mult = mult_option orelse .decode,
            .akos = akos_option orelse .decode,
            .tlke = tlke_option orelse .decode,
        },
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
    };
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

const Extract = struct {
    index_path: [:0]const u8,
    output_path: [:0]const u8,
    symbols_path: ?[:0]const u8,
    options: Options,
};

const Options = struct {
    script: ScriptMode,
    annotate: bool,
    music: bool,
    rainbow: bool,
    rmim: RawOrDecode,
    scrp: RawOrDecode,
    encd: RawOrDecode,
    excd: RawOrDecode,
    lscr: RawOrDecode,
    lsc2: RawOrDecode,
    obim: RawOrDecode,
    obcd: RawOrDecode,
    digi: RawOrDecode,
    talk: RawOrDecode,
    awiz: RawOrDecode,
    mult: RawOrDecode,
    akos: RawOrDecode,
    tlke: RawOrDecode,

    fn anyScriptDecode(self: *const Options) bool {
        return self.scrp == .decode or
            self.encd == .decode or
            self.excd == .decode or
            self.lscr == .decode or
            self.lsc2 == .decode or
            self.obcd == .decode;
    }
};

const RawOrDecode = enum {
    raw,
    decode,
};

const ScriptMode = enum {
    disassemble,
    decompile,
};

const YesNo = enum {
    no,
    yes,

    fn toBool(self: YesNo) bool {
        return self != .no;
    }
};

pub const Stat = enum {
    rmim_total,
    rmim_decode,
    rmim_raw,
    scrp_total,
    scrp_disassemble,
    scrp_decompile,
    scrp_raw,
    verb_total,
    verb_disassemble,
    verb_decompile,
    excd_total,
    excd_disassemble,
    excd_decompile,
    excd_raw,
    encd_total,
    encd_disassemble,
    encd_decompile,
    encd_raw,
    lscr_total,
    lscr_disassemble,
    lscr_decompile,
    lscr_raw,
    lsc2_total,
    lsc2_disassemble,
    lsc2_decompile,
    lsc2_raw,
    script_unknown_byte,
    digi_total,
    digi_decode,
    digi_raw,
    talk_total,
    talk_decode,
    talk_raw,
    awiz_total,
    awiz_decode,
    awiz_raw,
};

pub fn run(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    args: Extract,
) !std.EnumArray(Stat, u16) {
    const input_path_opt, const index_name = fs.splitPathZ(args.index_path);
    var input_dir = if (input_path_opt) |input_path|
        try fsd.openDir(diagnostic, std.fs.cwd(), input_path)
    else
        std.fs.cwd();
    defer if (input_path_opt) |_|
        input_dir.close();

    // Make sure tests always write to an empty dir
    if (builtin.is_test)
        fs.assertNotExists(std.fs.cwd(), args.output_path);

    try fsd.makeDirIfNotExistZ(diagnostic, std.fs.cwd(), args.output_path);
    var output_dir = try fsd.openDirZ(diagnostic, std.fs.cwd(), args.output_path);
    defer output_dir.close();

    const game = try games.detectGameOrFatal(diagnostic, index_name);

    var symbols_text: []const u8 = "";
    defer gpa.free(symbols_text);

    var symbols: Symbols = try .init(gpa, game);
    defer symbols.deinit(gpa);

    if (args.symbols_path) |path| {
        symbols_text = try fsd.readFileZ(gpa, diagnostic, std.fs.cwd(), path);
        try symbols.parse(gpa, symbols_text);
    }

    // Give the pool enough jobs for:
    //
    // - reader thread: io bound, mostly idle, 1 job
    // - block decode jobs: cpu bound, `num_cpus` jobs
    //
    // This isn't exact. The reader thread exits eagerly, so after it's done,
    // the cpu will be oversubscribed. But adding 1 results in faster wall clock
    // time on slower machines. This could be improved by putting the reader
    // thread in a separate thread pool for io-bound jobs.
    const n_jobs = 1 + @min(std.Thread.getCpuCount() catch 1, sync.max_concurrency);

    var pool: sync.ThreadPool = undefined;
    try pool.init(gpa, n_jobs);
    defer pool.deinit();

    var code: std.ArrayList(u8) = .empty;
    defer code.deinit(gpa);
    try code.ensureTotalCapacity(gpa, 8 << 10);

    try code.print(gpa, "; Extracted with Baller {s}\n\n", .{build_options.version});

    var vm: lang.Vm = undefined;
    var vm_ptr: utils.SafeUndefined(*const lang.Vm) = .undef;
    var op_map: std.EnumArray(lang.Op, decompile.Op) = undefined;
    var op_map_ptr: utils.SafeUndefined(*const std.EnumArray(lang.Op, decompile.Op)) = .undef;
    if (args.options.anyScriptDecode()) {
        vm = lang.buildVm(game);
        vm_ptr = .{ .defined = &vm };
        op_map = decompile.buildOpMap(game);
        op_map_ptr = .{ .defined = &op_map };
    }

    if (args.options.annotate)
        try code.appendSlice(gpa, "#error cannot build after extracting with --annotate\n\n");

    try code.print(gpa, "target {s}\n\n", .{@tagName(game.target())});

    const index, const index_buf = try extractIndex(gpa, diagnostic, input_dir, index_name, game, output_dir, &code);
    defer gpa.free(index_buf);

    try code.append(gpa, '\n');

    var cx: Context = .{
        .gpa = gpa,
        .pool = &pool,
        .options = args.options,
        .game = game,
        .vm = vm_ptr,
        .op_map = op_map_ptr,
        .symbols = &symbols,
        .index = &index,
        .global_var_usage = @splat(0),
        .output_dir = output_dir,
        .blinken = undefined,
        .stats = .initFill(0),
    };

    try cx.blinken.initAndStart();
    defer cx.blinken.stop();

    const num_disks = if (games.hasDisk(game)) num_disks: {
        var max: u8 = 0;
        for (index.lfl_disks.defined.slice(index.maxs.rooms)) |n|
            max = @max(max, n);
        break :num_disks max;
    } else 1;

    const bytes = try countInputBytes(
        game,
        input_dir,
        index_name,
        &args.options,
        diagnostic,
        num_disks,
    );
    cx.blinken.setText(.root, std.fs.path.stem(index_name));
    cx.blinken.setProgressStyle(.root, .bar_bytes);
    cx.blinken.setMax(.root, bytes);

    for (0..num_disks) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try extractDisk(&cx, diagnostic, input_dir, index_name, disk_number, &code);
    }

    if (cx.options.music) {
        try extractMusic(&cx, diagnostic, input_dir, index_name, output_dir, &code);
    }

    if (args.options.anyScriptDecode()) {
        try code.append(gpa, '\n');
        for (0..UsageTracker.max_global_vars) |num_usize| {
            const num: u14 = @intCast(num_usize);
            if (!UsageTracker.get(&cx.global_var_usage, num)) continue;
            try code.print(gpa, "var {f}@{}\n", .{
                symbols.fmtGlobalVarName(num),
                num,
            });
        }
    }

    try emitConsts(&cx, diagnostic, &code);

    try fsd.writeFileZ(diagnostic, output_dir, "project.scu", code.items);

    cx.blinken.debugAssertProgressFinished(.root);
    sanityCheckStats(&cx.stats);

    return cx.stats;
}

fn countInputBytes(
    game: games.Game,
    input_dir: std.fs.Dir,
    index_name: []const u8,
    options: *const Options,
    diagnostic: *Diagnostic,
    num_disks: u8,
) !u32 {
    var path_buf: [games.longest_index_name_len]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}", .{index_name}) catch unreachable;

    var result: u32 = 0;
    for (0..num_disks) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        games.pointPathToDisk(game.target(), path, disk_number);
        result = try addFileSize(result, input_dir, path, diagnostic);
    }
    if (options.music) {
        _ = std.fmt.bufPrint(&path_buf, "{s}", .{index_name}) catch unreachable;
        games.pointPathToMusic(path);
        result = try addFileSize(result, input_dir, path, diagnostic);
    }
    return result;
}

fn addFileSize(
    total: u32,
    input_dir: std.fs.Dir,
    path: []const u8,
    diagnostic: *Diagnostic,
) !u32 {
    const stat = try fsd.statFile(diagnostic, input_dir, path);
    return utils.add(u32, total, stat.size) orelse {
        diagnostic.err("file too big: {s}", .{path});
        return error.AddedToDiagnostic;
    };
}

pub const Index = struct {
    maxs: Maxs,
    directories: Directories,
    lfl_offsets: utils.SafeManyPointer([*]u32),
    lfl_disks: utils.SafeUndefined(utils.SafeManyPointer([*]u8)),
    room_names: RoomNames,

    pub fn directory(
        self: *const Index,
        dir: Symbols.GlobKind,
    ) struct { *const Directory, u32 } {
        return switch (dir) {
            .room_image => .{ &self.directories.room_images, self.maxs.rooms },
            .room => .{ &self.directories.rooms, self.maxs.rooms },
            .script => .{ &self.directories.scripts, self.maxs.scripts },
            .sound => .{ &self.directories.sounds, self.maxs.sounds },
            .costume => .{ &self.directories.costumes, self.maxs.costumes },
            .charset => .{ &self.directories.charsets, self.maxs.charsets },
            .image => .{ &self.directories.images, self.maxs.images },
            .talkie => .{ &self.directories.talkies, self.maxs.talkies },
        };
    }
};

pub const Maxs = extern struct {
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

pub const Directory = struct {
    rooms: utils.SafeManyPointer([*]u8),
    offsets: utils.SafeManyPointer([*]u32),
    sizes: utils.SafeManyPointer([*]u32),

    const empty: Directory = .{
        .rooms = .empty,
        .offsets = .empty,
        .sizes = .empty,
    };
};

const RoomNames = struct {
    buffer: utils.SafeManyPointer([*]u8),
    starts: utils.SafeManyPointer([*]u16),
    lens: utils.SafeManyPointer([*]u8),

    pub fn get(self: *const RoomNames, room_number: u8) ?[]const u8 {
        const len = self.lens.get(room_number);
        if (len == 0) return null;
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
    code: *std.ArrayList(u8),
) !struct { Index, []u8 } {
    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, index_name);

    const raw = try fsd.readFileZ(gpa, diagnostic, input_dir, index_name);
    defer gpa.free(raw);
    for (raw) |*b|
        b.* ^= xor_key;

    var in: std.io.Reader = .fixed(raw);
    var blocks: FixedBlockReader = .init(&in, &diag);

    const result_buf = try gpa.alloc(u8, raw.len);
    errdefer gpa.free(result_buf);
    var fba: std.heap.FixedBufferAllocator = .init(result_buf);

    try code.appendSlice(gpa, "index {\n");

    // MAXS

    const maxs = maxs: {
        const maxs_raw = try blocks.expect(.MAXS).bytes();
        if (maxs_raw.len != games.maxsLen(game))
            return error.BadData;

        var maxs: Maxs = undefined;
        const maxs_present_bytes = std.mem.asBytes(&maxs)[0..maxs_raw.len];
        const maxs_missing_bytes = std.mem.asBytes(&maxs)[maxs_raw.len..@sizeOf(Maxs)];
        @memcpy(maxs_present_bytes, maxs_raw);
        @memset(maxs_missing_bytes, 0);

        var path_buf: ["index_MAXS.bin".len + 1]u8 = undefined;
        const path = try std.fmt.bufPrintZ(&path_buf, "index_{f}.bin", .{BlockId.MAXS});
        try fsd.writeFileZ(diagnostic, output_dir, path, maxs_present_bytes);

        try code.print(gpa, "    maxs \"{s}\"\n", .{path});

        break :maxs maxs;
    };

    inline for (comptime std.meta.fieldNames(Maxs)) |f|
        diag.trace(@intCast(in.seek), "  {s} = {}", .{ f, @field(maxs, f) });

    // DIR*

    const diri = try readDirectory(gpa, &fba, &blocks, code, .DIRI, maxs.rooms);
    const dirr = try readDirectory(gpa, &fba, &blocks, code, .DIRR, maxs.rooms);
    const dirs = try readDirectory(gpa, &fba, &blocks, code, .DIRS, maxs.scripts);
    const dirn = try readDirectory(gpa, &fba, &blocks, code, .DIRN, maxs.sounds);
    const dirc = try readDirectory(gpa, &fba, &blocks, code, .DIRC, maxs.costumes);
    const dirf = try readDirectory(gpa, &fba, &blocks, code, .DIRF, maxs.charsets);
    const dirm = try readDirectory(gpa, &fba, &blocks, code, .DIRM, maxs.images);
    const dirt: Directory = if (games.hasTalkies(game))
        try readDirectory(gpa, &fba, &blocks, code, .DIRT, maxs.talkies)
    else
        .empty;

    // DLFL

    const dlfl_raw = try blocks.expect(.DLFL).bytes();
    if (dlfl_raw.len != 2 + 4 * maxs.rooms)
        return error.BadData;
    if (std.mem.readInt(u16, dlfl_raw[0..2], .little) != maxs.rooms)
        return error.BadData;
    const lfl_offsets = try fba.allocator().alloc(u32, maxs.rooms);
    @memcpy(lfl_offsets, std.mem.bytesAsSlice(u32, dlfl_raw[2..]));

    try code.appendSlice(gpa, "    index-block DLFL\n");

    for (lfl_offsets, 0..) |off, i|
        diag.trace(@intCast(in.seek), "  {:>3}: 0x{x:0>8}", .{ i, off });

    // DISK

    var lfl_disks: utils.SafeUndefined(utils.SafeManyPointer([*]u8)) = .undef;
    if (games.hasDisk(game)) {
        const disk_raw = try blocks.expect(.DISK).bytes();
        if (disk_raw.len != 2 + maxs.rooms)
            return error.BadData;
        if (std.mem.readInt(u16, disk_raw[0..2], .little) != maxs.rooms)
            return error.BadData;
        const lfl_disks_slice = try fba.allocator().dupe(u8, disk_raw[2..]);
        lfl_disks = .{ .defined = .init(lfl_disks_slice) };

        try code.appendSlice(gpa, "    index-block DISK\n");

        for (lfl_disks.defined.slice(maxs.rooms), 0..) |disk, i|
            diag.trace(@intCast(in.seek), "  {:>3}: {:>3}", .{ i, disk });
    }

    // SVER

    if (games.hasIndexSver(game))
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, .SVER);

    // RNAM

    const room_names = try readRoomNames(&in, &blocks, &diag, maxs.rooms, &fba);
    try code.appendSlice(gpa, "    index-block RNAM\n");

    // remaining blocks

    for ([_]BlockId{ .DOBJ, .AARY }) |id|
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, id);
    if (games.hasIndexInib(game))
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, .INIB);

    try blocks.finish();

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
            diag.trace(@intCast(in.seek), "{s}", .{block_id});
            for (
                dir.rooms.slice(len),
                dir.offsets.slice(len),
                dir.sizes.slice(len),
                0..,
            ) |room, offset, size, i|
                diag.trace(
                    @intCast(in.seek),
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
    blocks: *FixedBlockReader,
    code: *std.ArrayList(u8),
    block_id: BlockId,
    expected_len: u32,
) !Directory {
    const block_raw = try blocks.expect(block_id).bytes();

    try code.print(gpa, "    index-block {f}\n", .{block_id});

    var in: std.io.Reader = .fixed(block_raw);

    const len = try in.takeInt(u16, .little);
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
    in: *std.io.Reader,
    blocks: *FixedBlockReader,
    diag: *const Diagnostic.ForBinaryFile,
    num_rooms: u16,
    fba: *std.heap.FixedBufferAllocator,
) !RoomNames {
    const rnam = try blocks.expect(.RNAM).block();
    if (rnam.size > 0xffff) return error.BadData; // so we can index with u16

    const starts = try fba.allocator().alloc(u16, num_rooms);
    const lens = try fba.allocator().alloc(u8, num_rooms);
    // we only have to clear lens, because starts is not accessed unless lens is nonzero
    @memset(lens, 0);
    const buffer_start = fba.buffer[fba.end_index..].ptr;

    while (true) {
        const number = try in.takeInt(u16, .little);
        if (number == 0) break;
        const name_len = std.mem.indexOfScalar(u8, in.buffer[in.seek..], 0) orelse
            return error.BadData;
        const name_src_z = try io.readInPlace(in, name_len + 1);
        const name = try fba.allocator().dupe(u8, name_src_z[0..name_len :0]);
        starts[number] = @intCast(name.ptr - buffer_start);
        lens[number] = std.math.cast(u8, name_len) orelse return error.BadData;
        diag.trace(@intCast(in.seek), "  {:>3}: {f}", .{ number, std.ascii.hexEscape(name, .lower) });
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
    blocks: *FixedBlockReader,
    output_dir: std.fs.Dir,
    code: *std.ArrayList(u8),
    block_id: BlockId,
) !void {
    const block = try blocks.expect(block_id).block();
    const bytes = try io.readInPlace(blocks.stream, block.size);
    try writeRawBlock(gpa, block.id, bytes, output_dir, null, 4, .index_block, code);
}

const Context = struct {
    gpa: std.mem.Allocator,
    pool: *sync.ThreadPool,
    options: Options,
    game: games.Game,
    vm: utils.SafeUndefined(*const lang.Vm),
    op_map: utils.SafeUndefined(*const std.EnumArray(lang.Op, decompile.Op)),
    symbols: *const Symbols,
    index: *const Index,
    global_var_usage: UsageTracker.GlobalVars,
    output_dir: std.fs.Dir,
    blinken: Blinkenlights,
    stats: std.EnumArray(Stat, u16),

    fn incStat(self: *Context, stat: Stat) void {
        const old = @atomicRmw(u16, self.stats.getPtr(stat), .Add, 1, .monotonic);
        std.debug.assert(old != std.math.maxInt(u16)); // assert no overflow
    }

    fn incStatOpt(self: *Context, stat: ?Stat) void {
        if (stat) |s| self.incStat(s);
    }
};

fn attributePositionToProgress(last_progress_offset: *u32, in: *std.io.Reader) u32 {
    const pos = fxbcl.pos(in);
    const bytes = pos - last_progress_offset.*;
    last_progress_offset.* = pos;
    std.debug.assert(bytes != 0);
    return bytes;
}

fn extractDisk(
    cx: *Context,
    diagnostic: *Diagnostic,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    disk_number: u8,
    code: *std.ArrayList(u8),
) !void {
    try code.print(cx.gpa, "disk {} {{\n", .{disk_number});

    var disk_name_buf: [games.longest_index_name_len + 1]u8 = undefined;
    const disk_name = std.fmt.bufPrintZ(&disk_name_buf, "{s}", .{index_name}) catch unreachable;
    games.pointPathToDisk(cx.game.target(), disk_name, disk_number);

    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, disk_name);

    const in_file = try fsd.openFileZ(diagnostic, input_dir, disk_name);
    defer in_file.close();
    var in_buf: [4096]u8 = undefined;
    var in_raw = in_file.reader(&in_buf);
    var in_xor: io.XorReader = .init(&in_raw.interface, xor_key, &.{});
    var in_limit: std.io.Reader.Limited = .init(&in_xor.interface, .unlimited, &.{});
    const in = &in_limit.interface;

    var last_progress_offset: u32 = 0;

    var file_blocks: StreamingBlockReader = .init(in, &diag);

    const lecf = try file_blocks.expect(.LECF) orelse return error.BadData;
    var lecf_blocks: StreamingBlockReader = .init(in, &diag);

    const disk_blink = cx.blinken.addNode(.root);
    defer cx.blinken.removeNode(disk_blink);
    cx.blinken.setText(disk_blink, disk_name);
    cx.blinken.setProgressStyle(disk_blink, .bar_bytes);
    cx.blinken.setMax(disk_blink, Block.header_size + lecf.size);

    while (try lecf_blocks.expect(.LFLF)) |block| {
        const catchup = attributePositionToProgress(&last_progress_offset, in);
        cx.blinken.addProgress(disk_blink, catchup);
        cx.blinken.addProgress(.root, catchup);

        const room_blink = cx.blinken.addNode(disk_blink);
        defer cx.blinken.removeNode(room_blink);
        cx.blinken.setProgressStyle(room_blink, .bar);
        cx.blinken.setMax(room_blink, block.size);

        try extractRoom(
            cx,
            disk_number,
            in,
            &diag,
            &last_progress_offset,
            disk_blink,
            room_blink,
            code,
        );
        try lecf_blocks.finish(&block);

        cx.blinken.debugAssertProgressFinished(room_blink);
    }

    try lecf_blocks.end();

    try file_blocks.finish(&lecf);
    file_blocks.expectMismatchedEnd();

    cx.blinken.debugAssertProgressFinished(disk_blink);

    try code.appendSlice(cx.gpa, "}\n");
}

const max_room_code_chunks = 5120;

const Event = union(enum) {
    end,
    err,
    code_chunk: struct {
        index: u32,
        section: Section,
        code: std.ArrayList(u8),
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
    in: *std.io.Reader,
    disk_diag: *const Diagnostic.ForBinaryFile,
    last_progress_offset: *u32,
    disk_blink: Blinkenlights.NodeId,
    room_blink: Blinkenlights.NodeId,
    project_code: *std.ArrayList(u8),
) !void {
    const room_number = findRoomNumber(cx.game, cx.index, disk_number, fxbcl.pos(in)) orelse
        return error.BadData;

    const diag = disk_diag.child(0, .{ .glob = .{ .LFLF, room_number } });

    var events_buffer: [sync.max_concurrency]Event = undefined;
    var events: sync.Channel(Event) = .init(&events_buffer);

    try cx.pool.spawn(readRoomJob, .{
        cx,
        in,
        &diag,
        room_number,
        last_progress_offset,
        disk_blink,
        room_blink,
        &events,
    });

    try emitRoom(cx, diag.diagnostic, room_number, project_code, &events);
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
    room_var_usage: UsageTracker.RoomVars,
    /// Bitmask of which local scripts exist in the room
    lsc_mask: UsageTracker.LocalScripts,
    /// Used to assert `lsc_mask` isn't modified while being read
    lsc_mask_state: union { collecting: void, frozen: void },
    last_progress_offset: *u32,
    disk_blink: Blinkenlights.NodeId,
    room_blink: Blinkenlights.NodeId,
    events: *sync.Channel(Event),
    pending_jobs: std.atomic.Value(u32),
    next_chunk_index: u16,

    fn claimChunkIndex(self: *RoomContext) !u16 {
        const result = self.next_chunk_index;
        if (result >= max_room_code_chunks) return error.Overflow;
        self.next_chunk_index += 1;
        return result;
    }

    fn sendSync(self: *RoomContext, section: Section, code: std.ArrayList(u8)) !void {
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
        code: std.ArrayList(u8),
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
    in: *std.io.Reader,
    diag: *const Diagnostic.ForBinaryFile,
    room_number: u8,
    last_progress_offset: *u32,
    disk_blink: Blinkenlights.NodeId,
    room_blink: Blinkenlights.NodeId,
    events: *sync.Channel(Event),
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
        .room_var_usage = @splat(0),
        .lsc_mask = @splat(0),
        .lsc_mask_state = .{ .collecting = {} },
        .last_progress_offset = last_progress_offset,
        .disk_blink = disk_blink,
        .room_blink = room_blink,
        .events = events,
        .pending_jobs = .init(0),
        .next_chunk_index = 0,
    };

    (blk: {
        rcx.room_path = cx.index.room_names.get(room_number) orelse break :blk error.BadData;
        cx.blinken.setText(room_blink, rcx.room_path);
        fsd.makeDirIfNotExist(diag.diagnostic, cx.output_dir, rcx.room_path) catch |err| break :blk err;
        room_dir = fsd.openDir(diag.diagnostic, cx.output_dir, rcx.room_path) catch |err| break :blk err;
        rcx.room_dir = room_dir.?;

        readRoomInner(&rcx, in, diag) catch |err| break :blk err;
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(fxbcl.pos(in), "room {}: unexpected error: {s}", .{room_number}, err);
        events.send(.err);
    };

    diag.trace(fxbcl.pos(in), "waiting for jobs", .{});
    while (true) {
        const pending = rcx.pending_jobs.load(.acquire);
        if (pending == 0) break;
        std.Thread.Futex.wait(&rcx.pending_jobs, pending);
    }
    diag.trace(fxbcl.pos(in), "all jobs finished", .{});

    // This depends on usage data that the script jobs wrote to context. Do it
    // after the join so we know they finished.
    emitRoomVars(&rcx) catch |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(fxbcl.pos(in), "room {}: unexpected error: {s}", .{room_number}, err);
        events.send(.err);
    };

    events.send(.end);
}

fn emitRoomVars(cx: *RoomContext) !void {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.cx.gpa);

    try out.append(cx.cx.gpa, '\n');

    for (0..UsageTracker.max_room_vars) |num_usize| {
        const num: u14 = @intCast(num_usize);
        if (!UsageTracker.get(&cx.room_var_usage, num)) continue;
        try out.print(cx.cx.gpa, "var {f}@{}\n", .{
            cx.cx.symbols.fmtRoomVarName(cx.room_number, num),
            num,
        });
    }

    // If there were no vars, don't output an extra newline with nothing below it
    if (out.items.len == 1)
        out.clearRetainingCapacity();

    try cx.sendSync(.top, out);
}

fn readRoomInner(
    cx: *RoomContext,
    in: *std.io.Reader,
    diag: *const Diagnostic.ForBinaryFile,
) !void {
    var lflf_blocks: StreamingBlockReader = .init(in, diag);

    const rmim_chunk_index = try cx.claimChunkIndex();
    const rmim_block = try lflf_blocks.expect(.RMIM) orelse return error.BadData;
    var rmim_raw = try cx.cx.gpa.alloc(u8, rmim_block.size);
    errdefer cx.cx.gpa.free(rmim_raw);
    try in.readSliceAll(rmim_raw);
    try lflf_blocks.finish(&rmim_block);
    const rmim_progress_bytes = attributePositionToProgress(cx.last_progress_offset, in);

    const rmda_block = try lflf_blocks.expect(.RMDA) orelse return error.BadData;
    const pals_chunk_index, var pals_raw = try extractRmda(cx, in, diag);
    errdefer cx.cx.gpa.free(pals_raw);
    try lflf_blocks.finish(&rmda_block);

    // extract RMIM after RMDA since it needs the room palette
    try spawnBlockJob(
        extractRmimAndPalsJob,
        cx,
        diag,
        &rmim_block,
        rmim_raw,
        rmim_chunk_index,
        .{ rmim_progress_bytes, pals_raw, pals_chunk_index },
    );
    rmim_raw.len = 0; // ownership was moved to the job, don't free it here
    pals_raw.len = 0;

    while (try lflf_blocks.next()) |block| {
        try readBlockAndSpawn(extractGlobJob, cx, in, diag, &block);
        try lflf_blocks.finish(&block);
    }

    try lflf_blocks.end();
}

fn extractRmda(
    cx: *RoomContext,
    in: *std.io.Reader,
    diag: *const Diagnostic.ForBinaryFile,
) !struct { u16, []u8 } {
    try cx.sendSyncFmt(.top, "rmda {{\n", .{});

    var pals_chunk_index: ?u16 = null;
    var pals_raw: []u8 = &.{};
    errdefer cx.cx.gpa.free(pals_raw);

    // Buffer script blocks until the end, so we can collect a list of valid
    // local script numbers for the decompiler. This is needed specifically for
    // Backyard Baseball 2001 teaminfo lsc2204, which references lsc2173, which
    // does not actually exist.
    var buffered_blocks: Deque(BufferedBlock) = .empty;
    defer {
        while (buffered_blocks.popFront()) |b|
            cx.cx.gpa.free(b.data);
        buffered_blocks.deinit(cx.cx.gpa);
    }

    var rmda_blocks: StreamingBlockReader = .init(in, diag);

    while (try rmda_blocks.next()) |block| {
        switch (block.id) {
            .PALS => {
                if (pals_chunk_index != null) return error.BadData;
                pals_chunk_index = try cx.claimChunkIndex();

                pals_raw = try cx.cx.gpa.alloc(u8, block.size);
                try in.readSliceAll(pals_raw);

                cx.room_palette.setOnce(try extractPals(pals_raw, diag, &block));

                // since this skipped the thread pool, we didn't bother with a
                // blinken node, but we still need to track the progress
                const progress = attributePositionToProgress(cx.last_progress_offset, in);
                cx.cx.blinken.addProgress(cx.room_blink, progress);
                cx.cx.blinken.addProgress(cx.disk_blink, progress);
                cx.cx.blinken.addProgress(.root, progress);
            },
            .OBIM => {
                // The OBIM decoder uses the palette, so make sure it has
                // already been read. If not, fail. It's not ideal to completely
                // fail here, but this path should never be hit anyway.
                if (pals_chunk_index == null) return error.BadData;

                try readBlockAndSpawn(extractRmdaChildJob, cx, in, diag, &block);
            },
            .OBCD, .EXCD, .ENCD, .LSCR, .LSC2 => {
                try readBlockAndAddToBuffer(cx, in, &block, &buffered_blocks);
            },
            else => {
                var code: std.ArrayList(u8) = .empty;
                errdefer code.deinit(cx.cx.gpa);
                try writeRawBlockImpl(cx.cx.gpa, block.id, .{ .reader = in }, cx.room_dir, cx.room_path, 4, .{ .block_offset = block.offset() }, &code);
                try cx.sendSync(.top, code);

                // since this skipped the thread pool, we didn't bother with a
                // blinken node, but we still need to track the progress
                const progress = attributePositionToProgress(cx.last_progress_offset, in);
                cx.cx.blinken.addProgress(cx.room_blink, progress);
                cx.cx.blinken.addProgress(cx.disk_blink, progress);
                cx.cx.blinken.addProgress(.root, progress);
            },
        }
        try rmda_blocks.finish(&block);
    }

    try spawnBufferedBlockJobs(cx, diag, &buffered_blocks);

    try rmda_blocks.end();

    try cx.sendSyncFmt(.top, "}}\n", .{});

    if (pals_chunk_index == null) return error.BadData;
    return .{ pals_chunk_index.?, pals_raw };
}

pub const expected_pals_size = 796;

fn extractPals(
    pals_raw: []const u8,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
) ![0x300]u8 {
    const diag = disk_diag.child(block.start, .{ .block_id = .PALS });

    if (pals_raw.len != expected_pals_size) return error.BadData;
    var pals_stream: std.io.Reader = .fixed(pals_raw);
    var pals_blocks: FixedBlockReader = .init(&pals_stream, &diag);

    var wrap_blocks = try pals_blocks.expect(.WRAP).nested();

    const off = try wrap_blocks.expect(.OFFS).value(u32);
    if (off.* != 12) return error.BadData;

    const apal = try wrap_blocks.expect(.APAL).value([0x300]u8);

    try wrap_blocks.finish();
    try pals_blocks.finish();

    return apal.*;
}

const BufferedBlock = struct {
    block: Block,
    data: []u8,

    fn calcProgressBytes(data: []const u8) u32 {
        return @intCast(Block.header_size + data.len);
    }
};

fn readBlockAndAddToBuffer(
    cx: *RoomContext,
    in: *std.io.Reader,
    block: *const Block,
    buffered_blocks: *Deque(BufferedBlock),
) !void {
    const raw = try cx.cx.gpa.alloc(u8, block.size);
    errdefer cx.cx.gpa.free(raw);
    try in.readSliceAll(raw);

    // we recompute this later instead of storing it in memory. make sure the
    // value is what we expect
    const progress_bytes = attributePositionToProgress(cx.last_progress_offset, in);
    std.debug.assert(progress_bytes == BufferedBlock.calcProgressBytes(raw));

    try buffered_blocks.pushBack(cx.cx.gpa, .{ .block = block.*, .data = raw });
    errdefer comptime unreachable;

    switch (block.id) {
        .OBCD, .EXCD, .ENCD => {},
        .LSCR, .LSC2 => {
            _ = cx.lsc_mask_state.collecting;

            const script_number, _ = parseLscHeader(.from(block.id), raw) catch return;
            const script_index = std.math.sub(u16, script_number, games.firstLocalScript(cx.cx.game)) catch return;
            if (script_index >= UsageTracker.max_local_scripts) return;
            std.mem.writePackedInt(u1, std.mem.asBytes(&cx.lsc_mask), script_index, 1, .little);
        },
        else => unreachable,
    }
}

fn spawnBufferedBlockJobs(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    buffered_blocks: *Deque(BufferedBlock),
) !void {
    _ = cx.lsc_mask_state.collecting;
    cx.lsc_mask_state = .{ .frozen = {} };

    while (buffered_blocks.popFront()) |lsc| {
        errdefer cx.cx.gpa.free(lsc.data);

        const progress_bytes = BufferedBlock.calcProgressBytes(lsc.data);

        const chunk_index = try cx.claimChunkIndex();
        try spawnBlockJob(
            extractRmdaChildJob,
            cx,
            diag,
            &lsc.block,
            lsc.data,
            chunk_index,
            .{progress_bytes},
        );
    }
}

const BlockJob = fn (
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
    progress_bytes: u32,
) anyerror!void;

fn readBlockAndSpawn(
    job: BlockJob,
    cx: *RoomContext,
    in: *std.io.Reader,
    diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
) !void {
    const raw = try cx.cx.gpa.alloc(u8, block.size);
    errdefer cx.cx.gpa.free(raw);
    try in.readSliceAll(raw);
    const progress_bytes = attributePositionToProgress(cx.last_progress_offset, in);

    const chunk_index = try cx.claimChunkIndex();

    try spawnBlockJob(job, cx, diag, block, raw, chunk_index, .{progress_bytes});
}

fn spawnBlockJob(
    job: anytype,
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
    args: anytype,
) !void {
    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.cx.pool.spawn(runBlockJob, .{ job, cx, diag, block.*, raw, chunk_index, args });
}

fn runBlockJob(
    job: anytype,
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block: Block,
    raw: []const u8,
    chunk_index: u16,
    args: anytype,
) void {
    {
        defer cx.cx.gpa.free(raw);

        @call(.auto, job, .{ cx, diag, &block, raw, chunk_index } ++ args) catch |err| {
            if (err != error.AddedToDiagnostic)
                diag.zigErr(block.offset(), "unexpected error: {s}", .{}, err);
            cx.events.send(.err);
        };
    }

    const prev_pending = cx.pending_jobs.fetchSub(1, .acq_rel);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn extractRmimAndPalsJob(
    cx: *RoomContext,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    rmim_raw: []const u8,
    rmim_chunk_index: u16,
    progress_bytes: u32,
    pals_raw: []const u8,
    pals_chunk_index: u16,
) !void {
    defer cx.cx.gpa.free(pals_raw);

    cx.cx.incStat(.rmim_total);

    const block_blink = cx.cx.blinken.addNode(cx.room_blink);
    defer {
        cx.cx.blinken.removeNode(block_blink);
        cx.cx.blinken.addProgress(cx.room_blink, progress_bytes);
        cx.cx.blinken.addProgress(cx.disk_blink, progress_bytes);
        cx.cx.blinken.addProgress(.root, progress_bytes);
    }
    cx.cx.blinken.setTextPrint(block_blink, "{f}", .{block.id});

    std.debug.assert(disk_diag.offset == 0);
    var diag = disk_diag.child(block.start, .{ .block_id = .RMIM });
    diag.cap_level = true;

    var code: std.ArrayList(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    var tx: Transaction = .init(&code);

    if (cx.cx.options.rmim == .decode)
        if (tryDecodeAndSend(extractRmimInner, cx, &tx, &diag, .{rmim_raw}, &code, rmim_chunk_index, .top))
            return;

    // If the RMIM was decoded successfully, the .bmp specifies the palette.
    // Otherwise output them both as separate raw blocks.

    try writeRawGlob(cx, diag.diagnostic, block, cx.room_number, rmim_raw, &code);
    cx.sendChunk(rmim_chunk_index, .top, code);

    code = .empty;
    try writeRawBlock(cx.cx.gpa, .PALS, pals_raw, cx.room_dir, cx.room_path, 4, .block, &code);
    cx.sendChunk(pals_chunk_index, .top, code);

    cx.cx.incStat(.rmim_raw);
}

fn extractRmimInner(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    try rmim.decode(cx.cx.gpa, raw, diag, &cx.room_palette.defined, cx.cx.options.rainbow, code, cx.room_dir, cx.room_path);

    errdefer comptime unreachable; // if we get here, success and commit

    cx.cx.incStat(.rmim_decode);
}

fn extractRmdaChildJob(
    cx: *RoomContext,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
    progress_bytes: u32,
) !void {
    cx.cx.incStatOpt(switch (block.id) {
        .OBIM, .OBCD => null,
        .EXCD => .excd_total,
        .ENCD => .encd_total,
        .LSCR => .lscr_total,
        .LSC2 => .lsc2_total,
        else => unreachable,
    });

    const block_blink = cx.cx.blinken.addNode(cx.room_blink);
    defer {
        cx.cx.blinken.removeNode(block_blink);
        cx.cx.blinken.addProgress(cx.room_blink, progress_bytes);
        cx.cx.blinken.addProgress(cx.disk_blink, progress_bytes);
        cx.cx.blinken.addProgress(.root, progress_bytes);
    }
    cx.cx.blinken.setTextPrint(block_blink, "{f}", .{block.id});

    std.debug.assert(disk_diag.offset == 0);
    var diag = disk_diag.child(block.start, .{ .block_id = block.id });
    diag.cap_level = true;

    var code: std.ArrayList(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    var tx: Transaction = .init(&code);

    // First try to decode
    switch (block.id) {
        .OBIM => if (cx.cx.options.obim == .decode) {
            if (tryDecodeAndSend(extractObim, cx, &tx, &diag, .{raw}, &code, chunk_index, .bottom))
                return;
        },
        .OBCD => if (cx.cx.options.obcd == .decode) {
            if (tryDecodeAndSend(decodeObcd, cx, &tx, &diag, .{raw}, &code, chunk_index, .bottom))
                return;
        },
        .EXCD, .ENCD => {
            if (extractEncdExcd(cx, &tx, &diag, block.id, raw, &code, chunk_index))
                return;
        },
        .LSCR, .LSC2 => {
            if (extractLsc(cx, &tx, &diag, block_blink, block.id, raw, &code, chunk_index))
                return;
        },
        else => unreachable, // This is only called for the above block ids
    }

    // If decoding failed or was skipped, extract as raw
    try writeRawBlock(cx.cx.gpa, block.id, raw, cx.room_dir, cx.room_path, 4, .{ .block_offset = block.offset() }, &code);
    const section: Section = switch (block.id) {
        .OBIM => .bottom,
        .OBCD => .bottom,
        .EXCD => .exit_script,
        .ENCD => .enter_script,
        .LSCR, .LSC2 => .local_scripts,
        else => unreachable,
    };
    cx.sendChunk(chunk_index, section, code);

    cx.cx.incStatOpt(switch (block.id) {
        .OBIM, .OBCD => null,
        .EXCD => .excd_raw,
        .ENCD => .encd_raw,
        .LSCR => .lscr_raw,
        .LSC2 => .lsc2_raw,
        else => unreachable,
    });
}

fn extractObim(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    try obim.extract(cx.cx.gpa, diag, raw, &cx.room_palette.defined, cx.cx.options.rainbow, code, cx.room_dir, cx.room_path);
}

const Cdhd = extern struct {
    object_id: u16 align(1),
    unk_02: [15]u8,
};

pub const VerbEntry = struct {
    number: u8,
    offset: u16 align(1),
};

fn decodeObcd(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var stream: std.io.Reader = .fixed(raw);
    var obcd_blocks: FixedBlockReader = .init(&stream, diag);

    const cdhd = try obcd_blocks.expect(.CDHD).value(Cdhd);

    const verb_raw = try obcd_blocks.expect(.VERB).bytes();
    var verb_in: std.io.Reader = .fixed(verb_raw);
    while (try verb_in.takeByte() != 0)
        try verb_in.discardAll(2);
    const verbs = std.mem.bytesAsSlice(VerbEntry, verb_raw[0 .. verb_in.seek - 1]);
    const min_code_offset = Block.header_size + @as(u32, @intCast(verb_in.seek));

    const obna = try obcd_blocks.expect(.OBNA).bytes();
    if (obna.len == 0 or obna[obna.len - 1] != 0) return error.BadData;
    const name = obna[0 .. obna.len - 1];

    try obcd_blocks.finish();

    try code.print(
        cx.cx.gpa,
        "object object{0}@{0} \"{1s}\" {{\n",
        .{ cdhd.object_id, name },
    );

    try writeRawBlock(cx.cx.gpa, .CDHD, std.mem.asBytes(cdhd), cx.room_dir, cx.room_path, 4, .{ .object = cdhd.object_id }, code);

    for (verbs, 0..) |v, vi| {
        if (v.offset < min_code_offset) return error.BadData;
        const start = v.offset - Block.header_size;
        const end = if (vi == verbs.len - 1)
            verb_raw.len
        else
            verbs[vi + 1].offset - Block.header_size;
        if (end < start) return error.BadData;
        if (end > verb_raw.len) return error.BadData;
        const bytecode = verb_raw[start..end];

        var tx: Transaction = .init(code);

        cx.cx.incStat(.verb_total);

        if (cx.cx.options.script == .decompile and
            tryDecode("decompile", decompileVerb, cx, &tx, diag, .{ cdhd.object_id, v.number, bytecode }, code))
            continue;
        if (tryDecode("disassemble", disassembleVerb, cx, &tx, diag, .{ cdhd.object_id, v.number, bytecode }, code))
            continue;
        return error.AddedToDiagnostic;
    }

    try code.appendSlice(cx.cx.gpa, "}\n");
}

fn disassembleVerb(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    object: u16,
    verb: u8,
    bytecode: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;

    const id: Symbols.ScriptId = .{ .object = .{
        .room = cx.room_number,
        .number = object,
        .verb = verb,
    } };

    var out: std.io.Writer.Allocating = .init(cx.cx.gpa);
    defer out.deinit();

    var usage: UsageTracker = .init(cx.cx.game);

    disasm.disassemble(cx.cx.gpa, cx.cx.vm.defined, cx.room_number, id, bytecode, cx.cx.symbols, cx.cx.options.annotate, &out.writer, &usage, &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: ["object0000_00.s".len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "object{:0>4}_{:0>2}.s", .{ object, verb }) catch unreachable;
    try fsd.writeFileZ(diag.diagnostic, cx.room_dir, path, out.written());

    try code.print(cx.cx.gpa, "\n    verb {} \"{s}/{s}\"\n", .{ verb, cx.room_path, path });

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

    cx.cx.incStat(.verb_disassemble);
    diagnostic.flushStats(cx.cx, diag);
}

fn decompileVerb(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    object: u16,
    verb: u8,
    bytecode: []const u8,
    code: *std.ArrayList(u8),
) !void {
    const id: Symbols.ScriptId = .{ .object = .{
        .room = cx.room_number,
        .number = object,
        .verb = verb,
    } };

    var usage: UsageTracker = .init(cx.cx.game);

    try code.print(cx.cx.gpa, "\n    verb {}", .{verb});
    _ = cx.lsc_mask_state.frozen;
    try decompile.run(cx.cx.gpa, diag, cx.cx.vm.defined, cx.cx.op_map.defined, cx.cx.symbols, cx.cx.options.annotate, cx.room_number, id, bytecode, cx.cx.index, &cx.lsc_mask, code, 2, &usage);
    try code.appendSlice(cx.cx.gpa, "    }\n");

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

    cx.cx.incStat(.verb_decompile);
}

const EncdExcd = enum {
    encd,
    excd,

    fn from(block_id: BlockId) EncdExcd {
        return switch (block_id) {
            .ENCD => .encd,
            .EXCD => .excd,
            else => unreachable,
        };
    }
};

fn extractEncdExcd(
    cx: *RoomContext,
    tx: *Transaction,
    diag: *const Diagnostic.ForBinaryFile,
    block_id: BlockId,
    raw: []const u8,
    code: *std.ArrayList(u8),
    chunk_index: u16,
) bool {
    const edge: EncdExcd = .from(block_id);

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
        tryDecode("decompile", extractEncdExcdDecompile, cx, tx, diag, .{ edge, raw }, code))
    {
        cx.sendChunk(chunk_index, section, code.*);
        return true;
    }
    if (tryDecode("disassemble", extractEncdExcdDisassemble, cx, tx, diag, .{ edge, raw }, code)) {
        cx.sendChunk(chunk_index, section, code.*);
        return true;
    }
    return false;
}

fn extractEncdExcdDisassemble(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    edge: EncdExcd,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;
    const id: Symbols.ScriptId = switch (edge) {
        .encd => .{ .enter = .{ .room = cx.room_number } },
        .excd => .{ .exit = .{ .room = cx.room_number } },
    };

    var out: std.io.Writer.Allocating = .init(cx.cx.gpa);
    defer out.deinit();

    var usage: UsageTracker = .init(cx.cx.game);

    disasm.disassemble(cx.cx.gpa, cx.cx.vm.defined, cx.room_number, id, raw, cx.cx.symbols, cx.cx.options.annotate, &out.writer, &usage, &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: ["encd.s".len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "{s}.s", .{@tagName(edge)}) catch unreachable;
    try fsd.writeFileZ(diag.diagnostic, cx.room_dir, path, out.written());

    try code.print(cx.cx.gpa, "{s} \"{s}/{s}\"\n", .{ @tagName(edge), cx.room_path, path });

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

    cx.cx.incStat(switch (edge) {
        .encd => .encd_disassemble,
        .excd => .excd_disassemble,
    });
    diagnostic.flushStats(cx.cx, diag);
}

fn extractEncdExcdDecompile(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    edge: EncdExcd,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    // If there's no script block at all, the compiler emits a zero-length
    // ENCD/EXCD. If there's an empty script block, the compiler emits ENCD/EXCD
    // with just the `end-object` instruction. As usual we need to differentiate
    // between those two cases in order to roundtrip successfully. This is where
    // that happens on the decompiler side.
    if (raw.len != 0)
        try extractEncdExcdDecompileInner(cx, diag, edge, raw, code);

    cx.cx.incStat(switch (edge) {
        .encd => .encd_decompile,
        .excd => .excd_decompile,
    });
}

fn extractEncdExcdDecompileInner(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    edge: EncdExcd,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    const id: Symbols.ScriptId = switch (edge) {
        .encd => .{ .enter = .{ .room = cx.room_number } },
        .excd => .{ .exit = .{ .room = cx.room_number } },
    };

    var usage: UsageTracker = .init(cx.cx.game);

    const keyword = switch (edge) {
        .encd => "enter",
        .excd => "exit",
    };
    try code.appendSlice(cx.cx.gpa, keyword);
    _ = cx.lsc_mask_state.frozen;
    try decompile.run(cx.cx.gpa, diag, cx.cx.vm.defined, cx.cx.op_map.defined, cx.cx.symbols, cx.cx.options.annotate, cx.room_number, id, raw, cx.cx.index, &cx.lsc_mask, code, 1, &usage);
    try code.appendSlice(cx.cx.gpa, "}\n");

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);
}

const LocalScriptBlockType = enum {
    lscr,
    lsc2,

    fn from(block_id: BlockId) LocalScriptBlockType {
        return switch (block_id) {
            .LSCR => .lscr,
            .LSC2 => .lsc2,
            else => unreachable,
        };
    }
};

fn extractLsc(
    cx: *RoomContext,
    tx: *Transaction,
    diag: *Diagnostic.ForBinaryFile,
    block_blink: Blinkenlights.NodeId,
    block_id: BlockId,
    raw: []const u8,
    code: *std.ArrayList(u8),
    chunk_index: u16,
) bool {
    const block_type: LocalScriptBlockType = .from(block_id);
    const option = switch (block_type) {
        .lscr => cx.cx.options.lscr,
        .lsc2 => cx.cx.options.lsc2,
    };
    if (option == .raw) return false;

    const script_number, const bytecode = parseLscHeader(block_type, raw) catch |err|
        return handleDecodeResult(err, tx, "decode", diag, code);

    // mild hack: patch the log context now that we know the script number
    diag.section = .{ .glob = .{ block_id, script_number } };
    diag.trace(0, "found script number", .{});

    cx.cx.blinken.setTextPrint(block_blink, "{f} {}", .{ block_id, script_number });

    if (cx.cx.options.script == .decompile and
        tryDecode("decompile", extractLscDecompile, cx, tx, diag, .{ block_type, script_number, bytecode }, code))
    {
        cx.sendChunk(chunk_index, .local_scripts, code.*);
        return true;
    }
    if (tryDecode("disassemble", extractLscDisassemble, cx, tx, diag, .{ block_type, script_number, bytecode }, code)) {
        cx.sendChunk(chunk_index, .local_scripts, code.*);
        return true;
    }
    return false;
}

fn parseLscHeader(block_type: LocalScriptBlockType, raw: []const u8) !struct { u16, []const u8 } {
    switch (block_type) {
        .lscr => {
            if (raw.len == 0) return error.EndOfStream;
            const script_number = raw[0];
            const bytecode = raw[1..];
            return .{ script_number, bytecode };
        },
        .lsc2 => {
            if (raw.len < 4) return error.EndOfStream;
            const script_number_u32 = std.mem.readInt(u32, raw[0..4], .little);
            const bytecode = raw[4..];
            const script_number = std.math.cast(u16, script_number_u32) orelse
                return error.Overflow;
            return .{ script_number, bytecode };
        },
    }
}

fn extractLscDisassemble(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block_type: LocalScriptBlockType,
    script_number: u32,
    bytecode: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;

    var out: std.io.Writer.Allocating = .init(cx.cx.gpa);
    defer out.deinit();

    var usage: UsageTracker = .init(cx.cx.game);

    const id: Symbols.ScriptId = .{ .local = .{
        .room = cx.room_number,
        .number = script_number,
    } };
    disasm.disassemble(cx.cx.gpa, cx.cx.vm.defined, cx.room_number, id, bytecode, cx.cx.symbols, cx.cx.options.annotate, &out.writer, &usage, &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: utils.TinyArray(u8, Symbols.max_name_len + ".s".len + 1) = .empty;
    path_buf.printAssumeCapacity("{f}", .{
        cx.cx.symbols.fmtScriptName(cx.room_number, script_number),
    });
    const name = path_buf.slice();

    path_buf.appendSlice(".s\x00") catch unreachable;
    const path = path_buf.slice()[0 .. path_buf.len - 1 :0];
    try fsd.writeFileZ(diag.diagnostic, cx.room_dir, path, out.written());

    try code.print(
        cx.cx.gpa,
        "\nlsc {s}@{} \"{s}/{s}\"\n",
        .{ name, script_number, cx.room_path, path },
    );

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

    cx.cx.incStat(switch (block_type) {
        .lscr => .lscr_disassemble,
        .lsc2 => .lsc2_disassemble,
    });
    diagnostic.flushStats(cx.cx, diag);
}

fn extractLscDecompile(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block_type: LocalScriptBlockType,
    script_number: u32,
    bytecode: []const u8,
    code: *std.ArrayList(u8),
) !void {
    const id: Symbols.ScriptId = .{ .local = .{
        .room = cx.room_number,
        .number = script_number,
    } };

    var usage: UsageTracker = .init(cx.cx.game);

    try code.print(cx.cx.gpa, "\nlocal-script {f}@{}", .{
        cx.cx.symbols.fmtScriptName(cx.room_number, script_number),
        script_number,
    });
    _ = cx.lsc_mask_state.frozen;
    try decompile.run(cx.cx.gpa, diag, cx.cx.vm.defined, cx.cx.op_map.defined, cx.cx.symbols, cx.cx.options.annotate, cx.room_number, id, bytecode, cx.cx.index, &cx.lsc_mask, code, 1, &usage);
    try code.appendSlice(cx.cx.gpa, "}\n");

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

    cx.cx.incStat(switch (block_type) {
        .lscr => .lscr_decompile,
        .lsc2 => .lsc2_decompile,
    });
}

fn findGlobNumber(
    index: *const Index,
    block_id: BlockId,
    room_number: u8,
    offset_in_disk: u32,
) ?u16 {
    const kind = Symbols.GlobKind.fromBlockId(block_id) orelse return null;
    const dir, const dir_len = index.directory(kind);
    const offset_in_room = offset_in_disk - index.lfl_offsets.get(room_number);
    for (dir.rooms.slice(dir_len), dir.offsets.slice(dir_len), 0..) |r, o, i|
        if (r == room_number and o == offset_in_room)
            return @intCast(i);
    return null;
}

fn extractGlobJob(
    cx: *RoomContext,
    disk_diag: *const Diagnostic.ForBinaryFile,
    block: *const Block,
    raw: []const u8,
    chunk_index: u16,
    progress_bytes: u32,
) !void {
    cx.cx.incStatOpt(switch (block.id) {
        .SCRP => .scrp_total,
        .DIGI => .digi_total,
        .TALK => .talk_total,
        .AWIZ => .awiz_total,
        else => null,
    });

    const glob_blink = cx.cx.blinken.addNode(cx.room_blink);
    defer {
        cx.cx.blinken.removeNode(glob_blink);
        cx.cx.blinken.addProgress(cx.room_blink, progress_bytes);
        cx.cx.blinken.addProgress(cx.disk_blink, progress_bytes);
        cx.cx.blinken.addProgress(.root, progress_bytes);
    }
    cx.cx.blinken.setTextPrint(glob_blink, "{f}", .{block.id});

    var code: std.ArrayList(u8) = .empty;
    errdefer code.deinit(cx.cx.gpa);

    const glob_number = findGlobNumber(cx.cx.index, block.id, cx.room_number, block.offset()) orelse {
        // This should normally be impossible, but there's a glitched CHAR block
        // in soccer that we need to handle in order to round-trip.
        disk_diag.trace(block.offset(), "glob missing from directory", .{});
        try writeRawBlock(cx.cx.gpa, block.id, raw, cx.room_dir, cx.room_path, 0, .{ .block_offset = block.offset() }, &code);
        cx.sendChunk(chunk_index, .bottom, code);
        return;
    };

    disk_diag.trace(block.offset(), "glob number {}", .{glob_number});
    cx.cx.blinken.setTextPrint(glob_blink, "{f} {}", .{ block.id, glob_number });

    std.debug.assert(disk_diag.offset == 0);
    var diag = disk_diag.child(block.start, .{ .glob = .{ block.id, glob_number } });
    diag.cap_level = true;

    var tx: Transaction = .init(&code);

    // First try to decode
    switch (block.id) {
        .SCRP => {
            if (extractScrp(cx, &tx, &diag, glob_number, raw, &code, chunk_index))
                return;
        },
        .DIGI => if (cx.cx.options.digi == .decode)
            if (tryDecodeAndSend(extractSound, cx, &tx, &diag, .{ block.id, glob_number, raw }, &code, chunk_index, .bottom))
                return,
        .TALK => if (cx.cx.options.talk == .decode)
            if (tryDecodeAndSend(extractSound, cx, &tx, &diag, .{ block.id, glob_number, raw }, &code, chunk_index, .bottom))
                return,
        .AWIZ => if (cx.cx.options.awiz == .decode)
            if (tryDecodeAndSend(extractAwiz, cx, &tx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        .MULT => if (cx.cx.options.mult == .decode)
            if (tryDecodeAndSend(extractMult, cx, &tx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        .AKOS => if (cx.cx.options.akos == .decode)
            if (tryDecodeAndSend(extractAkos, cx, &tx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        .TLKE => if (cx.cx.options.tlke == .decode)
            if (tryDecodeAndSend(extractTlke, cx, &tx, &diag, .{ glob_number, raw }, &code, chunk_index, .bottom))
                return,
        else => {},
    }

    // If decoding failed or was skipped, extract as raw

    try writeRawGlob(cx, diag.diagnostic, block, glob_number, raw, &code);
    cx.sendChunk(chunk_index, .bottom, code);

    cx.cx.incStatOpt(switch (block.id) {
        .SCRP => .scrp_raw,
        .DIGI => .digi_raw,
        .TALK => .talk_raw,
        .AWIZ => .awiz_raw,
        else => null,
    });
}

const Transaction = struct {
    code_initial_len: u32,

    fn init(code: *std.ArrayList(u8)) Transaction {
        return .{
            .code_initial_len = @intCast(code.items.len),
        };
    }

    fn rollback(self: *Transaction, code: *std.ArrayList(u8)) void {
        code.shrinkRetainingCapacity(self.code_initial_len);
        self.* = undefined;
    }
};

fn tryDecode(
    decoder_name: []const u8,
    decodeFn: anytype,
    cx: *RoomContext,
    tx: *Transaction,
    diag: *const Diagnostic.ForBinaryFile,
    decode_args: anytype,
    code: *std.ArrayList(u8),
) bool {
    const result = @call(.auto, decodeFn, .{ cx, diag } ++ decode_args ++ .{code});
    return handleDecodeResult(result, tx, decoder_name, diag, code);
}

// break out the non-generic code for better codegen
fn handleDecodeResult(
    result: anyerror!void,
    tx: *Transaction,
    decoder_name: []const u8,
    diag: *const Diagnostic.ForBinaryFile,
    code: *std.ArrayList(u8),
) bool {
    if (result) {
        return true;
    } else |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(0, "unexpected error: {s}", .{}, err);

        tx.rollback(code);

        diag.info(0, "{s} failed", .{decoder_name});
        return false;
    }
}

fn tryDecodeAndSend(
    decodeFn: anytype,
    cx: *RoomContext,
    tx: *Transaction,
    diag: *const Diagnostic.ForBinaryFile,
    decode_args: anytype,
    code: *std.ArrayList(u8),
    chunk_index: u16,
    section: Section,
) bool {
    const result = @call(.auto, decodeFn, .{ cx, diag } ++ decode_args ++ .{code});
    return handleDecodeAndSendResult(result, cx, tx, diag, code, chunk_index, section);
}

// break out the non-generic code for better codegen
fn handleDecodeAndSendResult(
    result: anyerror!void,
    cx: *const RoomContext,
    tx: *Transaction,
    diag: *const Diagnostic.ForBinaryFile,
    code: *std.ArrayList(u8),
    chunk_index: u16,
    section: Section,
) bool {
    if (handleDecodeResult(result, tx, "decode", diag, code)) {
        cx.sendChunk(chunk_index, section, code.*);
        return true;
    }
    return false;
}

fn extractScrp(
    cx: *RoomContext,
    tx: *Transaction,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
    chunk_index: u16,
) bool {
    if (cx.cx.options.scrp == .raw)
        return false;
    if (cx.cx.options.script == .decompile and
        tryDecode("decompile", extractScrpDecompile, cx, tx, diag, .{ glob_number, raw }, code))
    {
        cx.sendChunk(chunk_index, .global_scripts, code.*);
        return true;
    }
    if (tryDecode("disassemble", extractScrpDisassemble, cx, tx, diag, .{ glob_number, raw }, code)) {
        cx.sendChunk(chunk_index, .global_scripts, code.*);
        return true;
    }
    return false;
}

fn extractScrpDisassemble(
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var diagnostic: DisasmDiagnostic = .init;

    const id: Symbols.ScriptId = .{ .global = glob_number };

    var out: std.io.Writer.Allocating = .init(cx.cx.gpa);
    defer out.deinit();

    var usage: UsageTracker = .init(cx.cx.game);

    disasm.disassemble(cx.cx.gpa, cx.cx.vm.defined, cx.room_number, id, raw, cx.cx.symbols, cx.cx.options.annotate, &out.writer, &usage, &diagnostic) catch |err| {
        diag.zigErr(0, "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };

    var path_buf: utils.TinyArray(u8, Symbols.max_name_len + ".s".len + 1) = .empty;
    path_buf.printAssumeCapacity("{f}", .{
        cx.cx.symbols.fmtScriptName(cx.room_number, glob_number),
    });
    const name = path_buf.slice();

    path_buf.appendSlice(".s\x00") catch unreachable;
    const path = path_buf.slice()[0 .. path_buf.len - 1 :0];
    try fsd.writeFileZ(diag.diagnostic, cx.room_dir, path, out.written());

    try code.print(
        cx.cx.gpa,
        "\nscr {s}@{} \"{s}/{s}\"\n",
        .{ name, glob_number, cx.room_path, path },
    );

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

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
    cx: *RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    const id: Symbols.ScriptId = .{ .global = glob_number };

    var usage: UsageTracker = .init(cx.cx.game);

    try code.print(cx.cx.gpa, "\nscript {f}@{}", .{
        cx.cx.symbols.fmtScriptName(cx.room_number, glob_number),
        glob_number,
    });
    _ = cx.lsc_mask_state.frozen;
    try decompile.run(cx.cx.gpa, diag, cx.cx.vm.defined, cx.cx.op_map.defined, cx.cx.symbols, cx.cx.options.annotate, cx.room_number, id, raw, cx.cx.index, &cx.lsc_mask, code, 1, &usage);
    try code.appendSlice(cx.cx.gpa, "}\n");

    errdefer comptime unreachable; // if we get here, success and commit

    UsageTracker.atomicUnion(&cx.cx.global_var_usage, &usage.global_vars);
    UsageTracker.atomicUnion(&cx.room_var_usage, &usage.room_vars);

    cx.cx.incStat(.scrp_decompile);
}

fn extractSound(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    block_id: BlockId,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var name_buf: utils.TinyArray(u8, Symbols.max_name_len + 1) = .empty;
    name_buf.printAssumeCapacity("{f}", .{cx.cx.symbols.fmtGlobName(.sound, glob_number)});
    const name = name_buf.slice();

    try code.print(cx.cx.gpa, "sound {f} {s}@{} {{\n", .{ block_id, name, glob_number });

    try sounds.extract(cx.cx.gpa, diag, name, raw, code, cx.room_dir, cx.room_path);

    try code.appendSlice(cx.cx.gpa, "}\n");

    errdefer comptime unreachable; // if we get here, success and commit

    cx.cx.incStat(switch (block_id) {
        .DIGI => .digi_decode,
        .TALK => .talk_decode,
        else => unreachable,
    });
}

fn extractAwiz(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var name_buf: utils.TinyArray(u8, Symbols.max_name_len) = .empty;
    name_buf.printAssumeCapacity("{f}", .{cx.cx.symbols.fmtGlobName(.image, glob_number)});
    const name = name_buf.slice();

    try code.print(cx.cx.gpa, "awiz {s}@{} {{\n", .{ name, glob_number });

    try awiz.decode(cx.cx.gpa, diag, raw, &cx.room_palette.defined, cx.cx.options.rainbow, name, code, 4, cx.room_dir, cx.room_path);

    try code.appendSlice(cx.cx.gpa, "}\n");

    errdefer comptime unreachable; // if we get here, success and commit

    cx.cx.incStat(.awiz_decode);
}

fn extractMult(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var name_buf: utils.TinyArray(u8, Symbols.max_name_len + 1) = .empty;
    name_buf.printAssumeCapacity("{f}", .{cx.cx.symbols.fmtGlobName(.image, glob_number)});
    name_buf.appendSlice("\x00") catch unreachable;
    const name = name_buf.slice()[0 .. name_buf.len - 1 :0];

    try code.print(cx.cx.gpa, "mult {s}@{} {{\n", .{ name, glob_number });

    try mult.extract(cx.cx.gpa, diag, name, raw, &cx.room_palette.defined, cx.cx.options.rainbow, cx.room_dir, cx.room_path, code);
}

fn extractAkos(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var name_buf: utils.TinyArray(u8, Symbols.max_name_len + 1) = .empty;
    name_buf.printAssumeCapacity("{f}", .{cx.cx.symbols.fmtGlobName(.costume, glob_number)});
    name_buf.appendSlice("\x00") catch unreachable;
    const name = name_buf.slice()[0 .. name_buf.len - 1 :0];

    try fsd.makeDirIfNotExistZ(diag.diagnostic, cx.room_dir, name);

    var dir = try fsd.openDirZ(diag.diagnostic, cx.room_dir, name);
    defer dir.close();

    var path_buf: [Ast.max_room_name_len + 1 + Symbols.max_name_len:0]u8 = undefined;
    const path = std.fmt.bufPrintZ(
        path_buf[0 .. path_buf.len + 1],
        "{s}/{s}",
        .{ cx.room_path, name },
    ) catch unreachable;

    try code.print(cx.cx.gpa, "akos {s}@{} {{\n", .{ name, glob_number });
    try akos.decode(cx.cx.gpa, raw, cx.cx.options.rainbow, path, dir, code);
    try code.appendSlice(cx.cx.gpa, "}\n");
}

fn extractTlke(
    cx: *const RoomContext,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var stream: std.io.Reader = .fixed(raw);
    var blocks: FixedBlockReader = .init(&stream, diag);
    const text_raw = try blocks.expect(.TEXT).bytes();
    try blocks.finish();

    // Check for null terminator and no interior nulls
    if (text_raw.len == 0) return error.BadData;
    if (std.mem.indexOfScalar(u8, text_raw[0 .. text_raw.len - 1], 0) != null)
        return error.BadData;
    if (text_raw[text_raw.len - 1] != 0) return error.BadData;
    const text = text_raw[0 .. text_raw.len - 1];

    try code.print(cx.cx.gpa, "talkie {f}@{} = \"", .{
        cx.cx.symbols.fmtGlobName(.talkie, glob_number),
        glob_number,
    });
    try decompile.emitStringContents(cx.cx.gpa, code, text);
    try code.appendSlice(cx.cx.gpa, "\"\n");
}

fn writeRawGlob(
    cx: *const RoomContext,
    diagnostic: *Diagnostic,
    block: *const Block,
    glob_number: u16,
    data: []const u8,
    code: *std.ArrayList(u8),
) !void {
    var filename_buf: utils.TinyArray(u8, Symbols.max_name_len + ".bin".len + 1) = .empty;
    const kind = Symbols.GlobKind.fromBlockId(block.id) orelse unreachable;
    const name = if (kind.hasName()) blk: {
        filename_buf.printAssumeCapacity("{f}", .{cx.cx.symbols.fmtGlobName(kind, glob_number)});
        break :blk filename_buf.slice();
    } else blk: {
        filename_buf.printAssumeCapacity("{f}_{:0>4}", .{ block.id, glob_number });
        break :blk null;
    };
    filename_buf.appendSlice(".bin\x00") catch unreachable;
    const filename = filename_buf.slice()[0 .. filename_buf.len - 1 :0];

    try fsd.writeFileZ(diagnostic, cx.room_dir, filename, data);

    try code.print(cx.cx.gpa, "raw-glob {f} ", .{block.id});
    if (name) |n| {
        try code.appendSlice(cx.cx.gpa, n);
        try code.append(cx.cx.gpa, '@');
    }
    try code.print(cx.cx.gpa, "{} \"{s}/{s}\"\n", .{ glob_number, cx.room_path, filename });
}

pub fn writeRawBlock(
    gpa: std.mem.Allocator,
    block_id: BlockId,
    bytes: []const u8,
    output_dir: std.fs.Dir,
    output_path: ?[]const u8,
    indent: u8,
    filename_pattern: @typeInfo(@TypeOf(writeRawBlockImpl)).@"fn".params[6].type.?,
    code: *std.ArrayList(u8),
) !void {
    try writeRawBlockImpl(gpa, block_id, .{ .bytes = bytes }, output_dir, output_path, indent, filename_pattern, code);
}

fn writeRawBlockImpl(
    gpa: std.mem.Allocator,
    block_id: BlockId,
    data_source: union(enum) {
        bytes: []const u8,
        reader: *std.io.Reader,
    },
    output_dir: std.fs.Dir,
    output_path: ?[]const u8,
    indent: u8,
    filename_pattern: union(enum) {
        block,
        block_offset: u32,
        index_block,
        block_block: BlockId,
        block_number_block: struct { BlockId, u16 },
        object: u16,
        object_block: struct { u16, BlockId },
        symbol_block: []const u8,
    },
    code: *std.ArrayList(u8),
) !void {
    const inline_threshold = 24;

    for (0..indent) |_|
        try code.append(gpa, ' ');
    try code.print(gpa, "raw-block {f} ", .{block_id});

    // If the data is short enough, write it inline

    const len = switch (data_source) {
        .bytes => |b| b.len,
        .reader => |r| fxbcl.remaining(r),
    };
    if (len <= inline_threshold) {
        var buf: [inline_threshold]u8 = undefined;
        const data = switch (data_source) {
            .bytes => |bytes| bytes,
            .reader => |r| blk: {
                const data = buf[0..len];
                try r.readSliceAll(data);
                break :blk data;
            },
        };
        try code.print(gpa, "`{x}`\n", .{data});
        return;
    }

    // If the data is long, write it to a file

    var filename_buf: [Symbols.max_name_len + "_XXXX.bin".len + 1]u8 = undefined;
    const filename = switch (filename_pattern) {
        .block => try std.fmt.bufPrintZ(
            &filename_buf,
            "{f}.bin",
            .{block_id},
        ),
        .block_offset => |offset| try std.fmt.bufPrintZ(
            &filename_buf,
            "{f}_{x:0>8}.bin",
            .{ block_id, offset },
        ),
        .index_block => try std.fmt.bufPrintZ(
            &filename_buf,
            "index_{f}.bin",
            .{block_id},
        ),
        .block_block => |id| try std.fmt.bufPrintZ(
            &filename_buf,
            "{f}_{f}.bin",
            .{ id, block_id },
        ),
        .block_number_block => |x| try std.fmt.bufPrintZ(
            &filename_buf,
            "{f}_{:0>4}_{f}.bin",
            x ++ .{block_id},
        ),
        .object => |number| try std.fmt.bufPrintZ(
            &filename_buf,
            "object{:0>4}_{f}.bin",
            .{ number, block_id },
        ),
        .object_block => |o| try std.fmt.bufPrintZ(
            &filename_buf,
            "object{:0>4}_{f}_{f}.bin",
            o ++ .{block_id},
        ),
        .symbol_block => |s| try std.fmt.bufPrintZ(
            &filename_buf,
            "{s}_{f}.bin",
            .{ s, block_id },
        ),
    };

    // TODO: use `fsd` for better errors
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    var writer = file.writer(&.{});
    switch (data_source) {
        .bytes => |bytes| try writer.interface.writeAll(bytes),
        .reader => |r| try io.copy(r, &writer.interface),
    }

    try code.appendSlice(gpa, "\"");
    if (output_path) |path|
        try code.print(gpa, "{s}/", .{path});
    try code.print(gpa, "{s}\"\n", .{filename});
}

fn emitRoom(
    cx: *const Context,
    diagnostic: *Diagnostic,
    room_number: u8,
    project_code: *std.ArrayList(u8),
    events: *sync.Channel(Event),
) !void {
    var chunks: utils.TinyArray(Chunk, max_room_code_chunks) = .empty;
    defer for (chunks.slice()) |*chunk| chunk.code.deinit(cx.gpa);

    var ok = true;
    while (true) switch (events.receive()) {
        .end => break,
        .err => ok = false,
        .code_chunk => |chunk| {
            chunks.grow(chunk.index + 1, .{ .section = .top, .code = .empty });
            std.debug.assert(chunks.at(chunk.index).code.items.len == 0);
            chunks.set(chunk.index, .{ .section = chunk.section, .code = chunk.code });
        },
    };

    std.sort.block(Chunk, chunks.slice(), {}, Chunk.sectionAsc);

    const room_name = cx.index.room_names.get(room_number) orelse return error.BadData;
    var room_scu_path_buf: [Ast.max_room_name_len + ".scu".len + 1]u8 = undefined;
    const room_scu_path = try std.fmt.bufPrintZ(&room_scu_path_buf, "{s}.scu", .{room_name});

    try project_code.print(
        cx.gpa,
        "    room {} \"{s}\" \"{s}\"\n",
        .{ room_number, room_name, room_scu_path },
    );

    const room_scu = try fsd.createFileZ(diagnostic, cx.output_dir, room_scu_path);
    defer room_scu.close();

    if (!ok)
        try room_scu.writeAll("#error while extracting room; this file is incomplete!\n\n");

    const max_iovecs = max_room_code_chunks + std.meta.fields(Section).len;
    var iovecs: utils.TinyArray(std.posix.iovec_const, max_iovecs) = .empty;
    var last_section: Section = .top;
    for (chunks.slice()) |*chunk| {
        if (chunk.section != last_section) {
            last_section = chunk.section;
            // For scripts, a blank line was already added before each decl. For
            // all the rest, add a blank line between sections.
            if (chunk.section != .global_scripts and chunk.section != .local_scripts)
                iovecs.appendAssumeCapacity(iovec("\n"));
        }
        iovecs.appendAssumeCapacity(iovec(chunk.code.items));
    }
    try room_scu.writevAll(iovecs.slice());

    if (!ok)
        return error.AddedToDiagnostic;
}

const Chunk = struct {
    section: Section,
    code: std.ArrayList(u8),

    fn sectionAsc(_: void, lhs: Chunk, rhs: Chunk) bool {
        return @intFromEnum(lhs.section) < @intFromEnum(rhs.section);
    }
};

fn iovec(s: []const u8) std.posix.iovec_const {
    return .{ .base = s.ptr, .len = s.len };
}

fn extractMusic(
    cx: *Context,
    diagnostic: *Diagnostic,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    output_parent_dir: std.fs.Dir,
    code: *std.ArrayList(u8),
) !void {
    var in_path_buf: [games.longest_index_name_len + 1]u8 = undefined;
    const in_path = std.fmt.bufPrintZ(&in_path_buf, "{s}", .{index_name}) catch unreachable;
    games.pointPathToMusic(in_path);

    const output_path = "music";
    try fsd.makeDirIfNotExistZ(diagnostic, output_parent_dir, output_path);
    var output_dir = try fsd.openDirZ(diagnostic, output_parent_dir, output_path);
    defer output_dir.close();

    try music.extract(
        cx.gpa,
        diagnostic,
        &cx.blinken,
        cx.symbols,
        input_dir,
        in_path,
        output_dir,
        output_path,
        code,
    );
}

fn emitConsts(cx: *Context, diagnostic: *Diagnostic, code: *std.ArrayList(u8)) !void {
    // Store consts as they're emitted, to track duplicates
    var emit_log: std.StringHashMapUnmanaged(i32) = .empty;
    defer emit_log.deinit(cx.gpa);

    for (
        cx.symbols.enum_names.entries.items(.key),
        cx.symbols.enum_names.entries.items(.value),
    ) |enum_name, enum_index| {
        try code.print(cx.gpa, "\n; {s}\n", .{enum_name});
        const the_enum = &cx.symbols.enums.items[enum_index];
        for (the_enum.entries.items) |*entry|
            if (entry.name) |name|
                try emitConst(cx, diagnostic, &emit_log, name, entry.value, code);
    }

    for (
        cx.symbols.map_names.entries.items(.key),
        cx.symbols.map_names.entries.items(.value),
    ) |map_name, map_index| {
        try code.print(cx.gpa, "\n; {s}\n", .{map_name});
        const map = cx.symbols.maps.at(map_index);
        for (map.entries.items) |*entry|
            if (entry.name) |name|
                try emitConst(cx, diagnostic, &emit_log, name, entry.value, code);
    }
}

fn emitConst(
    cx: *Context,
    diagnostic: *Diagnostic,
    emit_log: *std.StringHashMapUnmanaged(i32),
    name: []const u8,
    value: i32,
    code: *std.ArrayList(u8),
) !void {
    const log_entry = try emit_log.getOrPut(cx.gpa, name);
    if (log_entry.found_existing) {
        // Allow duplicates only if they have the same value
        if (log_entry.value_ptr.* != value) {
            diagnostic.err("duplicate symbol name: {s}", .{name});
            return error.AddedToDiagnostic;
        }
        try code.appendSlice(cx.gpa, "; dup ");
    } else {
        log_entry.value_ptr.* = value;
    }
    try code.print(cx.gpa, "const {s} = {}\n", .{ name, value });
}

fn sanityCheckStats(s: *const std.EnumArray(Stat, u16)) void {
    std.debug.assert(s.get(.rmim_total) == s.get(.rmim_raw) + s.get(.rmim_decode));
    std.debug.assert(s.get(.scrp_total) == s.get(.scrp_decompile) + s.get(.scrp_disassemble) + s.get(.scrp_raw));
    std.debug.assert(s.get(.verb_total) == s.get(.verb_decompile) + s.get(.verb_disassemble));
    std.debug.assert(s.get(.excd_total) == s.get(.excd_decompile) + s.get(.excd_disassemble) + s.get(.excd_raw));
    std.debug.assert(s.get(.encd_total) == s.get(.encd_decompile) + s.get(.encd_disassemble) + s.get(.encd_raw));
    std.debug.assert(s.get(.lscr_total) == s.get(.lscr_decompile) + s.get(.lscr_disassemble) + s.get(.lscr_raw));
    std.debug.assert(s.get(.lsc2_total) == s.get(.lsc2_decompile) + s.get(.lsc2_disassemble) + s.get(.lsc2_raw));
    std.debug.assert(s.get(.digi_total) == s.get(.digi_raw) + s.get(.digi_decode));
    std.debug.assert(s.get(.awiz_total) == s.get(.awiz_raw) + s.get(.awiz_decode));
}
