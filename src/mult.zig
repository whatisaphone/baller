const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const awiz = @import("awiz.zig");
const blockId = @import("block_id.zig").blockId;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    mult_raw: []const u8,
    room_palette: *const [0x300]u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var in = std.io.fixedBufferStream(mult_raw);
    extractMultInner(gpa, diag, glob_number, &in, room_palette, room_dir, room_path, code) catch |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(@intCast(in.pos), "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };
}

fn extractMultInner(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    in: *std.io.FixedBufferStream([]const u8),
    room_palette: *const [0x300]u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var mult_path_buf: std.BoundedArray(u8, Ast.max_room_name_len + "/image0000".len + 1) = .{};
    mult_path_buf.appendSlice(room_path) catch unreachable;
    mult_path_buf.append('/') catch unreachable;
    const mult_path_basename_start = mult_path_buf.len;
    mult_path_buf.writer().print("image{:0>4}", .{glob_number}) catch unreachable;
    const mult_path_end = mult_path_buf.len;
    mult_path_buf.append(0) catch unreachable;
    const mult_path_basename = mult_path_buf.slice()[mult_path_basename_start..mult_path_end :0];
    const mult_path = mult_path_buf.slice()[0..mult_path_end];

    try fs.makeDirIfNotExistZ(room_dir, mult_path_basename);
    var mult_dir = try room_dir.openDirZ(mult_path_basename, .{});
    defer mult_dir.close();

    try code.writer(gpa).print("mult {} {{\n", .{glob_number});

    var mult_blocks = fixedBlockReader(in, diag);

    var mult_palette: ?*const [0x300]u8 = null;
    if (try mult_blocks.nextIf("DEFA")) |defa| {
        const defa_raw = try defa.bytes();
        mult_palette = try extractDefa(gpa, diag, defa_raw, mult_dir, mult_path, code);
    }

    var wrap_blocks = try mult_blocks.expect("WRAP").nested();

    const offs_block = try wrap_blocks.expect("OFFS").block();
    const offs_raw = try io.readInPlace(in, offs_block.size);
    const offs = std.mem.bytesAsSlice(u32, offs_raw);

    var awiz_offsets: std.BoundedArray(u32, Ast.max_mult_children) = .{};

    while (wrap_blocks.stream.pos < wrap_blocks.stream.buffer.len) {
        const awiz_block = try wrap_blocks.expect("AWIZ").block();
        const awiz_offset = awiz_block.offset() - offs_block.offset();
        try awiz_offsets.append(awiz_offset);

        const awiz_raw = try io.readInPlace(in, awiz_block.size);
        var the_awiz = try awiz.decode(gpa, diag, awiz_raw, mult_palette orelse room_palette);
        defer the_awiz.deinit(gpa);

        const first_index = for (offs, 0..) |off, i| {
            if (awiz_offset == off) break i;
        } else return error.BadData;

        var bmp_path_buf: ["0000.bmp".len + 1]u8 = undefined;
        const bmp_path = std.fmt.bufPrintZ(&bmp_path_buf, "{:0>4}.bmp", .{first_index}) catch unreachable;
        try code.appendSlice(gpa, "    awiz {\n");
        try awiz.extractChildren(gpa, mult_dir, mult_path, code, &the_awiz, bmp_path, 8);
        try code.appendSlice(gpa, "    }\n");
    }

    var off_indices: std.BoundedArray(u8, Ast.max_mult_children) = .{};
    for (offs) |off| {
        const index = std.sort.binarySearch(u32, awiz_offsets.slice(), off, orderU32) orelse
            return error.BadData;
        try off_indices.append(@intCast(index));
    }

    try code.appendSlice(gpa, "    indices [");
    for (off_indices.slice(), 0..) |index, i| {
        if (i != 0)
            try code.appendSlice(gpa, ", ");
        try code.writer(gpa).print("{}", .{index});
    }

    try code.appendSlice(gpa, "]\n}\n");

    try wrap_blocks.finishEof();
    try mult_blocks.finishEof();
}

fn orderU32(a: u32, b: u32) std.math.Order {
    return std.math.order(a, b);
}

fn extractDefa(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    defa_raw: []const u8,
    out_dir: std.fs.Dir,
    out_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !?*const [0x300]u8 {
    var rgbs: ?*const [0x300]u8 = null;

    var stream = std.io.fixedBufferStream(defa_raw);
    var blocks = fixedBlockReader(&stream, diag);

    try code.writer(gpa).print("    raw-block \"{s}\" {{\n", .{"DEFA"});

    while (stream.pos < defa_raw.len) {
        const block = try blocks.next().block();
        const bytes = try io.readInPlace(&stream, block.size);

        try writeRawBlock(gpa, block.id, .{ .bytes = bytes }, out_dir, out_path, 8, .block, code);

        if (block.id == blockId("RGBS")) {
            if (rgbs != null) return error.BadData;
            if (bytes.len != 0x300) return error.BadData;
            rgbs = bytes[0..0x300];
        }
    }

    try blocks.finishEof();

    try code.appendSlice(gpa, "    }\n");

    return rgbs;
}
