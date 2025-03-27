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

    const diri = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRI"), maxs.rooms);
    const dirr = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRR"), maxs.rooms);
    const dirs = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRS"), maxs.scripts);
    const dirn = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRN"), maxs.sounds);
    const dirc = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRC"), maxs.costumes);
    const dirf = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRF"), maxs.charsets);
    const dirm = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRM"), maxs.images);
    const dirt = try readDirectory(gpa, &fba, &in, &blocks, code, blockId("DIRT"), maxs.talkies);

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

    // RNAM

    const room_names = try readRoomNames(&in, &blocks, maxs.rooms, &fba);
    try code.appendSlice(gpa, "    index-block \"RNAM\"\n");

    // remaining blocks

    for ([_]BlockId{ blockId("DOBJ"), blockId("AARY"), blockId("INIB") }) |id|
        try extractRawIndexBlock(gpa, &in, &blocks, output_dir, code, id);

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
    in_stream: anytype,
    blocks: anytype,
    code: *std.ArrayListUnmanaged(u8),
    block_id: BlockId,
    expected_len: u32,
) !Directory {
    const block_size = try blocks.expect(block_id);
    const block_raw = try io.readInPlace(in_stream, block_size);

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
    const rnam_len = try blocks.expectBlock("RNAM");
    if (rnam_len > 0xffff) return error.BadData; // so we can index with u16

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

    const room_name = index.room_names.get(room_number);
    try fs.makeDirIfNotExist(output_dir, room_name);
    var room_dir = try output_dir.openDir(room_name, .{});
    defer room_dir.close();

    var room_code: std.ArrayListUnmanaged(u8) = .empty;
    defer room_code.deinit(gpa);

    var lflf_blocks = blockReader(in);

    // RMIM
    {
        const offset: u32 = @intCast(in.bytes_read);
        const size = try lflf_blocks.expectBlock("RMIM");
        try extractRawGlob(gpa, index, in, room_number, offset, blockId("RMIM"), size, room_dir, room_name, &room_code);
    }

    try extractRmda(gpa, in, &lflf_blocks, room_number, room_dir, room_name, &room_code);

    while (in.bytes_read < lflf_end) {
        const offset: u32 = @intCast(in.bytes_read);
        const id, const size = try lflf_blocks.next();
        try extractRawGlob(gpa, index, in, room_number, offset, id, size, room_dir, room_name, &room_code);
    }

    try lflf_blocks.finish(lflf_end);

    var room_scu_path_buf: [255 + ".scu\x00".len]u8 = undefined;
    const room_scu_path = try std.fmt.bufPrintZ(&room_scu_path_buf, "{s}.scu", .{room_name});
    try fs.writeFileZ(output_dir, room_scu_path, room_code.items);

    try project_code.writer(gpa).print(
        "    room {} \"{s}\" \"{s}\"\n",
        .{ room_number, room_name, room_scu_path },
    );
}

fn extractRmda(
    gpa: std.mem.Allocator,
    in: anytype,
    lflf_blocks: anytype,
    room_number: u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const rmda_id = blockId("RMDA");
    const rmda_size = try lflf_blocks.expect(rmda_id);
    const rmda_start: u32 = @intCast(in.bytes_read);
    const rmda_end = rmda_start + rmda_size;

    try code.writer(gpa).print(
        "raw-glob \"{s}\" {} {{\n",
        .{ fmtBlockId(&rmda_id), room_number },
    );

    var rmda_blocks = blockReader(in);

    while (in.bytes_read < rmda_end) {
        const offset: u32 = @intCast(in.bytes_read);
        const block_id, const size = try rmda_blocks.next();
        try extractRawBlock(gpa, in, offset, block_id, size, room_dir, room_path, code);
    }

    try rmda_blocks.finish(rmda_end);

    try code.appendSlice(gpa, "}\n");
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

fn extractRawGlob(
    gpa: std.mem.Allocator,
    index: *const Index,
    in: anytype,
    room_number: u8,
    offset: u32,
    block_id: BlockId,
    size: u32,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const glob_number = try findGlobNumber(index, block_id, room_number, offset) orelse
        return error.BadData;

    var filename_buf: ["XXXX_0000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{:0>4}.bin",
        .{ fmtBlockId(&block_id), glob_number },
    );
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), size), file.writer());

    try code.writer(gpa).print(
        "raw-glob \"{s}\" {} \"{s}/{s}\"\n",
        .{ fmtBlockId(&block_id), glob_number, output_path, filename },
    );
}

fn extractRawBlock(
    gpa: std.mem.Allocator,
    in: anytype,
    offset: u32,
    block_id: BlockId,
    size: u32,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var filename_buf: ["XXXX_00000000.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{x:0>8}.bin",
        .{ fmtBlockId(&block_id), offset },
    );
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), size), file.writer());

    try code.writer(gpa).print(
        "    raw-block \"{s}\" \"{s}/{s}\"\n",
        .{ fmtBlockId(&block_id), output_path, filename },
    );
}
