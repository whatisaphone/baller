const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const blockReader = @import("block_reader.zig").blockReader;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const xor_key = @import("build.zig").xor_key;
const cliargs = @import("cliargs.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");
const utils = @import("utils.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var index_path_opt: ?[:0]const u8 = null;
    var output_path_opt: ?[:0]const u8 = null;

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
        else => return arg.reportUnexpected(),
    };

    const index_path = index_path_opt orelse return cliargs.reportMissing("index");
    const output_path = output_path_opt orelse return cliargs.reportMissing("output");

    try run(gpa, .{
        .index_path = index_path,
        .output_path = output_path,
    });
}

const Extract = struct {
    index_path: [:0]const u8,
    output_path: [:0]const u8,
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
        try extractDisk(gpa, input_dir, index_name, game, &index, disk_number, output_dir, &code);
    }

    try fs.writeFileZ(output_dir, "project.scu", code.items);
}

const Index = struct {
    maxs: Maxs,
    lfl_offsets: utils.SafeManyPointer([*]u32),
    lfl_disks: utils.SafeManyPointer([*]u8),
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

fn extractIndex(
    gpa: std.mem.Allocator,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !struct { Index, []u8 } {
    const raw = try fs.readFileZ(gpa, input_dir, index_name);
    defer gpa.free(raw);
    for (raw) |*b|
        b.* ^= xor_key;

    var in = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader(&in);

    const result_buf = try gpa.alloc(u8, raw.len);
    errdefer gpa.free(result_buf);
    var fba: std.heap.FixedBufferAllocator = .init(result_buf);

    try code.appendSlice(gpa, "index {\n");

    // MAXS

    const maxs_unaligned = try blocks.expectBlockAsValue("MAXS", Maxs);
    const maxs = maxs_unaligned.*;
    try writeRawIndexBlock(gpa, output_dir, code, blockId("MAXS"), std.mem.asBytes(&maxs));

    // DIR*

    for ([_]BlockId{
        blockId("DIRI"), blockId("DIRR"), blockId("DIRS"), blockId("DIRN"),
        blockId("DIRC"), blockId("DIRF"), blockId("DIRM"), blockId("DIRT"),
    }) |id|
        try extractRawIndexBlock(gpa, &in, &blocks, output_dir, code, id);

    // DLFL

    const dlfl_raw = try blocks.expectBlockAsSlice("DLFL");
    if (dlfl_raw.len != 2 + 4 * maxs.rooms)
        return error.BadData;
    if (std.mem.readInt(u16, dlfl_raw[0..2], .little) != maxs.rooms)
        return error.BadData;
    const lfl_offsets = try fba.allocator().alloc(u32, maxs.rooms);
    @memcpy(lfl_offsets, std.mem.bytesAsSlice(u32, dlfl_raw[2..]));

    try code.appendSlice(gpa, "    index-block \"DLFL\"\n");

    // DISK

    const disk_raw = try blocks.expectBlockAsSlice("DISK");
    if (disk_raw.len != 2 + maxs.rooms)
        return error.BadData;
    if (std.mem.readInt(u16, disk_raw[0..2], .little) != maxs.rooms)
        return error.BadData;
    const lfl_disks = try fba.allocator().dupe(u8, disk_raw[2..]);

    try code.appendSlice(gpa, "    index-block \"DISK\"\n");

    // remaining blocks

    for ([_]BlockId{
        blockId("RNAM"), blockId("DOBJ"), blockId("AARY"), blockId("INIB"),
    }) |id|
        try extractRawIndexBlock(gpa, &in, &blocks, output_dir, code, id);

    try blocks.finishEof();

    try code.appendSlice(gpa, "}\n");

    const index: Index = .{
        .maxs = maxs,
        .lfl_offsets = .init(lfl_offsets),
        .lfl_disks = .init(lfl_disks),
    };
    return .{ index, result_buf };
}

fn extractRawIndexBlock(
    gpa: std.mem.Allocator,
    in: anytype,
    blocks: anytype,
    output_dir: anytype,
    code: *std.ArrayListUnmanaged(u8),
    block_id: BlockId,
) !void {
    const size = try blocks.expect(block_id);
    const data = try io.readInPlace(in, size);
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

    const in_file = try input_dir.openFileZ(disk_name.full(), .{});
    defer in_file.close();
    const in_xor = io.xorReader(in_file.reader(), xor_key);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in = std.io.countingReader(in_buf.reader());

    var file_blocks = blockReader(&in);

    const lecf_size = try file_blocks.expectBlock("LECF");
    const lecf_end: u32 = @intCast(in.bytes_read + lecf_size);
    var lecf_blocks = blockReader(&in);

    while (in.bytes_read < lecf_end) {
        const lflf_size = try lecf_blocks.expectBlock("LFLF");
        const lflf_start: u32 = @intCast(in.bytes_read);
        const lflf_end = lflf_start + lflf_size;
        try extractRoom(gpa, index, disk_number, &in, lflf_end, output_dir, code);
    }

    try lecf_blocks.finish(lecf_end);

    try file_blocks.finishEof();

    try code.appendSlice(gpa, "}\n");
}

fn extractRoom(
    gpa: std.mem.Allocator,
    index: *const Index,
    disk_number: u8,
    in: anytype,
    lflf_end: u32,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const room_number = for (
        index.lfl_offsets.slice(index.maxs.rooms),
        index.lfl_disks.slice(index.maxs.rooms),
        0..,
    ) |off, dsk, i| {
        if (off == in.bytes_read and dsk == disk_number)
            break i;
    } else return error.BadData;

    try code.writer(gpa).print("    room {} {{\n", .{room_number});

    var lflf_blocks = blockReader(in);

    while (in.bytes_read < lflf_end) {
        const offset: u32 = @intCast(in.bytes_read);
        const id, const size = try lflf_blocks.next();
        try extractRawBlock(gpa, disk_number, in, offset, id, size, output_dir, code);
    }

    try lflf_blocks.finish(lflf_end);

    try code.appendSlice(gpa, "    }\n");
}

fn extractRawBlock(
    gpa: std.mem.Allocator,
    disk_number: u8,
    in: anytype,
    offset: u32,
    id: BlockId,
    size: u32,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var filename_buf: ["00_XXXX_00000000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{:0>2}_{s}_{x:0>8}.bin",
        .{ disk_number, fmtBlockId(&id), offset },
    );
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), size), file.writer());

    try code.writer(gpa).print(
        "        raw-block \"{s}\" \"{s}\"\n",
        .{ fmtBlockId(&id), filename },
    );
}
