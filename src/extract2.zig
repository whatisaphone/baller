const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const Block = @import("block_reader.zig").Block;
const fixedBlockReader2 = @import("block_reader.zig").fixedBlockReader2;
const streamingBlockReader = @import("block_reader.zig").streamingBlockReader;
const xor_key = @import("build.zig").xor_key;
const cliargs = @import("cliargs.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const mult = @import("mult.zig");
const parser = @import("parser.zig");
const pathf = @import("pathf.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var index_path_opt: ?[:0]const u8 = null;
    var output_path_opt: ?[:0]const u8 = null;
    var awiz_option: ?@FieldType(Options, "awiz") = null;
    var mult_option: ?@FieldType(Options, "mult") = null;

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
            if (std.mem.eql(u8, opt.flag, "awiz")) {
                if (awiz_option != null) return arg.reportDuplicate();
                awiz_option = std.meta.stringToEnum(@FieldType(Options, "awiz"), opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "mult")) {
                if (mult_option != null) return arg.reportDuplicate();
                mult_option = std.meta.stringToEnum(@FieldType(Options, "mult"), opt.value) orelse
                    return arg.reportInvalidValue();
            } else {
                return arg.reportUnexpected();
            }
        },
        else => return arg.reportUnexpected(),
    };

    const index_path = index_path_opt orelse return cliargs.reportMissing("index");
    const output_path = output_path_opt orelse return cliargs.reportMissing("output");

    try run(gpa, .{
        .index_path = index_path,
        .output_path = output_path,
        .options = .{
            .awiz = awiz_option orelse .decode,
            .mult = mult_option orelse .decode,
        },
    });
}

const Extract = struct {
    index_path: [:0]const u8,
    output_path: [:0]const u8,
    options: Options,
};

const Options = struct {
    awiz: enum { raw, decode },
    mult: enum { raw, decode },
};

pub fn run(gpa: std.mem.Allocator, args: Extract) !void {
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

    const game: games.Game = .baseball_2001;

    var code: std.ArrayListUnmanaged(u8) = .empty;
    defer code.deinit(gpa);

    const index, const index_buf = try extractIndex(gpa, input_dir, index_name, output_dir, &code);
    defer gpa.free(index_buf);

    for (0..games.numberOfDisks(game)) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try extractDisk(gpa, input_dir, index_name, args.options, game, &index, disk_number, output_dir, &code);
    }

    try fs.writeFileZ(output_dir, "project.scu", code.items);
}

const Index = struct {
    maxs: Maxs,
    directories: Directories,
    lfl_offsets: utils.SafeManyPointer([*]u32),
    lfl_disks: utils.SafeManyPointer([*]u8),
    room_names: RoomNames,
};

const Maxs = struct {
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
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !struct { Index, []u8 } {
    var diagnostic: Diagnostic = .{
        .path = index_name,
        .offset = 0,
    };

    const raw = try fs.readFileZ(gpa, input_dir, index_name);
    defer gpa.free(raw);
    for (raw) |*b|
        b.* ^= xor_key;

    var in = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader2(&in, &diagnostic);

    const result_buf = try gpa.alloc(u8, raw.len);
    errdefer gpa.free(result_buf);
    var fba: std.heap.FixedBufferAllocator = .init(result_buf);

    try code.appendSlice(gpa, "index {\n");

    // MAXS

    const maxs_unaligned = try blocks.expect("MAXS").value(Maxs);
    const maxs = maxs_unaligned.*;
    try writeRawIndexBlock(gpa, output_dir, code, blockId("MAXS"), std.mem.asBytes(&maxs));

    // DIR*

    const diri = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRI"), maxs.rooms);
    const dirr = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRR"), maxs.rooms);
    const dirs = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRS"), maxs.scripts);
    const dirn = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRN"), maxs.sounds);
    const dirc = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRC"), maxs.costumes);
    const dirf = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRF"), maxs.charsets);
    const dirm = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRM"), maxs.images);
    const dirt = try readDirectory(gpa, &fba, &blocks, code, blockId("DIRT"), maxs.talkies);

    // DLFL

    const dlfl_raw = try blocks.expect("DLFL").bytes();
    if (dlfl_raw.len != 2 + 4 * maxs.rooms)
        return error.BadData;
    if (std.mem.readInt(u16, dlfl_raw[0..2], .little) != maxs.rooms)
        return error.BadData;
    const lfl_offsets = try fba.allocator().alloc(u32, maxs.rooms);
    @memcpy(lfl_offsets, std.mem.bytesAsSlice(u32, dlfl_raw[2..]));

    try code.appendSlice(gpa, "    index-block \"DLFL\"\n");

    // DISK

    const disk_raw = try blocks.expect("DISK").bytes();
    if (disk_raw.len != 2 + maxs.rooms)
        return error.BadData;
    if (std.mem.readInt(u16, disk_raw[0..2], .little) != maxs.rooms)
        return error.BadData;
    const lfl_disks = try fba.allocator().dupe(u8, disk_raw[2..]);

    try code.appendSlice(gpa, "    index-block \"DISK\"\n");

    // RNAM

    const room_names = try readRoomNames(&in, &blocks, maxs.rooms, &fba);
    try code.appendSlice(gpa, "    index-block \"RNAM\"\n");

    // remaining blocks

    for ([_]BlockId{ blockId("DOBJ"), blockId("AARY"), blockId("INIB") }) |id|
        try extractRawIndexBlock(gpa, &blocks, output_dir, code, id);

    try blocks.finishEof();

    try code.appendSlice(gpa, "}\n");

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
        .lfl_disks = .init(lfl_disks),
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

fn extractDisk(
    gpa: std.mem.Allocator,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    options: Options,
    game: games.Game,
    index: *const Index,
    disk_number: u8,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    try code.writer(gpa).print("disk {} {{\n", .{disk_number});

    var disk_name_buf: pathf.Path = .{};
    const disk_name = try pathf.append(&disk_name_buf, index_name);
    games.pointPathToDisk(game, disk_name.full(), disk_number);

    const diagnostic: Diagnostic = .{
        .path = disk_name.full(),
        .offset = 0,
    };

    const in_file = try input_dir.openFileZ(disk_name.full(), .{});
    defer in_file.close();
    const in_xor = io.xorReader(in_file.reader(), xor_key);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in = std.io.countingReader(in_buf.reader());

    var file_blocks = streamingBlockReader(&in, &diagnostic);

    const lecf = try file_blocks.expect("LECF").block();
    var lecf_blocks = streamingBlockReader(&in, &diagnostic);

    while (in.bytes_read < lecf.end()) {
        const lflf = try lecf_blocks.expect("LFLF").block();
        try extractRoom(gpa, options, index, disk_number, &in, &diagnostic, lflf.end(), output_dir, code);
    }

    try lecf_blocks.finish(lecf.end());

    try file_blocks.finishEof();

    try code.appendSlice(gpa, "}\n");
}

const max_room_code_chunks = 2048;

const Event = union(enum) {
    end,
    err,
    code_chunk: struct { index: u32, code: std.ArrayListUnmanaged(u8) },
};

fn extractRoom(
    gpa: std.mem.Allocator,
    options: Options,
    index: *const Index,
    disk_number: u8,
    in: anytype,
    diagnostic: *const Diagnostic,
    lflf_end: u32,
    output_dir: std.fs.Dir,
    project_code: *std.ArrayListUnmanaged(u8),
) !void {
    const room_number: u8 = for (
        index.lfl_offsets.slice(index.maxs.rooms),
        index.lfl_disks.slice(index.maxs.rooms),
        0..,
    ) |off, dsk, i| {
        if (off == in.bytes_read and dsk == disk_number)
            break @intCast(i);
    } else return error.BadData;

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = gpa });
    defer pool.deinit();

    var events: sync.Channel(Event, 16) = .init;

    try pool.spawn(readRoomJob, .{ gpa, options, index, in, diagnostic, lflf_end, room_number, output_dir, &pool, &events });

    try emitRoom(gpa, index, diagnostic, room_number, output_dir, project_code, &events);
}

fn readRoomJob(
    gpa: std.mem.Allocator,
    options: Options,
    index: *const Index,
    in: anytype,
    diagnostic: *const Diagnostic,
    lflf_end: u32,
    room_number: u8,
    output_dir: std.fs.Dir,
    pool: *std.Thread.Pool,
    events: *sync.Channel(Event, 16),
) void {
    // store this a level up so it outlives the jobs
    var room_dir: ?std.fs.Dir = null;
    defer if (room_dir) |*d| d.close();

    var pending_jobs: std.atomic.Value(u32) = .init(0);

    readRoomInner(gpa, options, index, in, diagnostic, lflf_end, room_number, output_dir, &room_dir, pool, events, &pending_jobs) catch {
        diagnostic.err(@intCast(in.bytes_read), "error reading room", .{});
        events.send(.err);
    };

    diagnostic.trace(@intCast(in.bytes_read), "waiting for jobs", .{});
    while (true) {
        const pending = pending_jobs.load(.acquire);
        if (pending == 0) break;
        std.Thread.Futex.wait(&pending_jobs, pending);
    }
    diagnostic.trace(@intCast(in.bytes_read), "all jobs finished", .{});

    events.send(.end);
}

fn readRoomInner(
    gpa: std.mem.Allocator,
    options: Options,
    index: *const Index,
    in: anytype,
    diagnostic: *const Diagnostic,
    lflf_end: u32,
    room_number: u8,
    output_dir: std.fs.Dir,
    room_dir: *?std.fs.Dir,
    pool: *std.Thread.Pool,
    events: *sync.Channel(Event, 16),
    pending_jobs: *std.atomic.Value(u32),
) !void {
    var next_chunk_index: u16 = 0;

    const room_path = index.room_names.get(room_number);
    try fs.makeDirIfNotExist(output_dir, room_path);
    room_dir.* = try output_dir.openDir(room_path, .{});

    var lflf_blocks = streamingBlockReader(in, diagnostic);

    {
        const chunk_index = next_chunk_index;
        next_chunk_index += 1;
        std.debug.assert(next_chunk_index < max_room_code_chunks);

        const rmim = try lflf_blocks.expect("RMIM").block();
        var code: std.ArrayListUnmanaged(u8) = .empty;
        errdefer code.deinit(gpa);
        try extractRawGlob(gpa, index, in, room_number, &rmim, room_dir.*.?, room_path, &code);
        events.send(.{ .code_chunk = .{ .index = chunk_index, .code = code } });
    }

    const room_palette = room_palette: {
        const chunk_index = next_chunk_index;
        next_chunk_index += 1;
        std.debug.assert(next_chunk_index < max_room_code_chunks);

        var code: std.ArrayListUnmanaged(u8) = .empty;
        errdefer code.deinit(gpa);
        const room_palette = try extractRmda(gpa, in, diagnostic, &lflf_blocks, room_number, room_dir.*.?, room_path, &code);
        events.send(.{ .code_chunk = .{ .index = chunk_index, .code = code } });
        break :room_palette room_palette;
    };

    while (in.bytes_read < lflf_end) {
        const block = try lflf_blocks.next().block();
        try extractGlob(gpa, options, index, diagnostic, room_number, &room_palette, in, room_dir.*.?, room_path, &block, pool, events, pending_jobs, &next_chunk_index);
    }

    try lflf_blocks.finish(lflf_end);
}

fn extractRmda(
    gpa: std.mem.Allocator,
    in: anytype,
    diagnostic: *const Diagnostic,
    lflf_blocks: anytype,
    room_number: u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) ![0x300]u8 {
    const rmda = try lflf_blocks.expect("RMDA").block();

    try code.writer(gpa).print(
        "raw-glob \"{s}\" {} {{\n",
        .{ fmtBlockId(&rmda.id), room_number },
    );

    var apal_opt: ?[0x300]u8 = null;

    var rmda_blocks = streamingBlockReader(in, diagnostic);

    while (in.bytes_read < rmda.end()) {
        const block = try rmda_blocks.next().block();
        switch (block.id) {
            blockId("PALS") => {
                if (apal_opt != null) return error.BadData;
                apal_opt = try extractPals(gpa, in, diagnostic, &block, room_dir, room_path, code);
            },
            else => {
                try extractRawBlock(gpa, in, &block, room_dir, room_path, code);
            },
        }
    }

    try rmda_blocks.finish(rmda.end());

    try code.appendSlice(gpa, "}\n");

    return apal_opt orelse return error.BadData;
}

fn extractPals(
    gpa: std.mem.Allocator,
    in: anytype,
    outer_diagnostic: *const Diagnostic,
    block: *const Block,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) ![0x300]u8 {
    const expected_len = 796;
    if (block.size != expected_len) return error.BadData;
    const pals_raw = try in.reader().readBytesNoEof(expected_len);
    var pals_stream = std.io.fixedBufferStream(&pals_raw);
    const diagnostic: Diagnostic = .{
        .path = outer_diagnostic.path,
        .offset = outer_diagnostic.offset + block.start,
    };
    var pals_blocks = fixedBlockReader2(&pals_stream, &diagnostic);

    const pals = try pals_blocks.expect("WRAP").block();
    var wrap_blocks = fixedBlockReader2(&pals_stream, &diagnostic);

    const off = try wrap_blocks.expect("OFFS").value(u32);
    if (off.* != 12) return error.BadData;

    const apal = try wrap_blocks.expect("APAL").value([0x300]u8);

    try wrap_blocks.finish(pals.end());
    try pals_blocks.finishEof();

    try writeRawBlock(gpa, block, &pals_raw, output_dir, output_path, code);

    return apal.*;
}

fn findGlobNumber(
    index: *const Index,
    block_id: BlockId,
    room_number: u8,
    offset_in_disk: u32,
) !?u16 {
    const dir, const dir_len = switch (block_id) {
        // XXX: this list is duplicated in emit
        blockId("RMIM") => .{ &index.directories.room_images, index.maxs.rooms },
        blockId("RMDA") => .{ &index.directories.rooms, index.maxs.rooms },
        blockId("SCRP") => .{ &index.directories.scripts, index.maxs.scripts },
        blockId("DIGI"), blockId("TALK") => .{ &index.directories.sounds, index.maxs.sounds },
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
    return error.BadData;
}

fn extractGlob(
    gpa: std.mem.Allocator,
    options: Options,
    index: *const Index,
    diagnostic: *const Diagnostic,
    room_number: u8,
    room_palette: *const [0x300]u8,
    in: anytype,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    block: *const Block,
    pool: *std.Thread.Pool,
    events: *sync.Channel(Event, 16),
    pending_jobs: *std.atomic.Value(u32),
    next_chunk_index: *u16,
) !void {
    const raw = try gpa.alloc(u8, block.size);
    errdefer gpa.free(raw);
    try in.reader().readNoEof(raw);

    const chunk_index = next_chunk_index.*;
    next_chunk_index.* += 1;
    if (next_chunk_index.* >= max_room_code_chunks) return error.Overflow;

    _ = pending_jobs.fetchAdd(1, .monotonic);
    try pool.spawn(extractGlobJob, .{ gpa, options, index, diagnostic, room_number, room_dir, room_path, room_palette, block.*, raw, events, pending_jobs, chunk_index });
}

fn extractGlobJob(
    gpa: std.mem.Allocator,
    options: Options,
    index: *const Index,
    diagnostic: *const Diagnostic,
    room_number: u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    room_palette: *const [0x300]u8,
    block: Block,
    raw: []const u8,
    events: *sync.Channel(Event, 16),
    pending_jobs: *std.atomic.Value(u32),
    chunk_index: u16,
) void {
    defer gpa.free(raw);

    extractGlobInner(gpa, options, index, diagnostic, room_number, room_dir, room_path, room_palette, &block, raw, events, chunk_index) catch {
        diagnostic.err(block.offset(), "error extracting glob", .{});
        events.send(.err);
    };

    const prev_pending = pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(pending_jobs, 1);
}

fn extractGlobInner(
    gpa: std.mem.Allocator,
    options: Options,
    index: *const Index,
    outer_diagnostic: *const Diagnostic,
    room_number: u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    room_palette: *const [0x300]u8,
    block: *const Block,
    raw: []const u8,
    events: *sync.Channel(Event, 16),
    chunk_index: u16,
) !void {
    const glob_number = try findGlobNumber(index, block.id, room_number, block.offset()) orelse
        return error.BadData;
    outer_diagnostic.trace(block.offset(), "glob number {}", .{glob_number});

    const diagnostic: Diagnostic = .{
        .path = outer_diagnostic.path,
        .offset = outer_diagnostic.offset + block.start,
    };

    var code: std.ArrayListUnmanaged(u8) = .empty;
    errdefer code.deinit(gpa);

    const decode: enum { skipped, ok, fallback } = decode: switch (block.id) {
        blockId("AWIZ") => {
            if (options.awiz != .decode) break :decode .skipped;
            if (extractAwiz(gpa, &diagnostic, glob_number, raw, room_palette, room_dir, room_path, &code))
                break :decode .ok
            else |err| if (err == error.Reported)
                break :decode .fallback
            else
                return err;
        },
        blockId("MULT") => {
            if (options.mult != .decode) break :decode .skipped;
            if (mult.extract(gpa, &diagnostic, glob_number, raw, room_palette, room_dir, room_path, &code))
                break :decode .ok
            else |err| if (err == error.Reported)
                break :decode .fallback
            else
                return err;
        },
        else => .skipped,
    };
    switch (decode) {
        .ok => {
            events.send(.{ .code_chunk = .{ .index = chunk_index, .code = code } });
            return;
        },
        .skipped => {},
        .fallback => {
            diagnostic.warn(0, "decode error, falling back to raw", .{});
        },
    }

    try writeRawGlob(gpa, index, room_number, block, raw, room_dir, room_path, &code);

    events.send(.{ .code_chunk = .{ .index = chunk_index, .code = code } });
}

fn extractAwiz(
    gpa: std.mem.Allocator,
    diagnostic: *const Diagnostic,
    glob_number: u16,
    raw: []const u8,
    room_palette: *const [0x300]u8,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var decoded = try awiz.decode(gpa, diagnostic, raw, null, room_palette, .{});
    defer decoded.deinit(gpa);

    try code.writer(gpa).print("awiz {} {{\n", .{glob_number});

    var bmp_path_buf: ["image0000.bmp".len + 1]u8 = undefined;
    const bmp_path = std.fmt.bufPrintZ(
        &bmp_path_buf,
        "image{:0>4}.bmp",
        .{glob_number},
    ) catch unreachable;
    try awiz.extractChildren(gpa, output_dir, output_path, code, &decoded, bmp_path, 4);

    try code.appendSlice(gpa, "}\n");
}

fn extractRawGlob(
    gpa: std.mem.Allocator,
    index: *const Index,
    in: anytype,
    room_number: u8,
    block: *const Block,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const glob_number = try findGlobNumber(index, block.id, room_number, block.offset()) orelse
        return error.BadData;

    var filename_buf: ["XXXX_0000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{:0>4}.bin",
        .{ fmtBlockId(&block.id), glob_number },
    );
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), block.size), file.writer());

    try code.writer(gpa).print(
        "raw-glob \"{s}\" {} \"{s}/{s}\"\n",
        .{ fmtBlockId(&block.id), glob_number, output_path, filename },
    );
}

fn writeRawGlob(
    gpa: std.mem.Allocator,
    index: *const Index,
    room_number: u8,
    block: *const Block,
    data: []const u8,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const glob_number = try findGlobNumber(index, block.id, room_number, block.offset()) orelse
        return error.BadData;

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
    gpa: std.mem.Allocator,
    index: *const Index,
    diagnostic: *const Diagnostic,
    room_number: u8,
    output_dir: std.fs.Dir,
    project_code: *std.ArrayListUnmanaged(u8),
    events: *sync.Channel(Event, 16),
) !void {
    var code_chunks: std.BoundedArray(std.ArrayListUnmanaged(u8), max_room_code_chunks) = .{};
    defer for (code_chunks.slice()) |*chunk| chunk.deinit(gpa);

    var ok = true;
    while (true) switch (events.receive()) {
        .end => break,
        .err => ok = false,
        .code_chunk => |chunk| {
            utils.growBoundedArray(&code_chunks, chunk.index + 1, .empty);
            std.debug.assert(code_chunks.get(chunk.index).items.len == 0);
            code_chunks.set(chunk.index, chunk.code);
        },
    };

    const room_name = index.room_names.get(room_number);
    var room_scu_path_buf: [parser.max_room_name_len + ".scu".len + 1]u8 = undefined;
    const room_scu_path = try std.fmt.bufPrintZ(&room_scu_path_buf, "{s}.scu", .{room_name});

    try project_code.writer(gpa).print(
        "    room {} \"{s}\" \"{s}\"\n",
        .{ room_number, room_name, room_scu_path },
    );

    const room_scu = try output_dir.createFileZ(room_scu_path, .{});
    defer room_scu.close();

    if (!ok)
        try room_scu.writeAll("#error while extracting room; this file is incomplete!\n\n");

    var iovecs_buf: [max_room_code_chunks]std.posix.iovec_const = undefined;
    const iovecs = iovecs_buf[0..code_chunks.len];
    for (iovecs, code_chunks.slice()) |*iovec, *chunk|
        iovec.* = .{ .base = chunk.items.ptr, .len = chunk.items.len };
    try room_scu.writevAll(iovecs);

    if (!ok) {
        diagnostic.err(null, "error extracting room", .{});
        return error.Reported;
    }
}
