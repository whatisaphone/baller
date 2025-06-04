const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const io = @import("io.zig");

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    var in = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader(&in, diag);

    try code.appendSlice(gpa, "obim {\n");

    const imhd_raw = try blocks.expect(.IMHD).bytes();
    if (imhd_raw.len < @sizeOf(Imhd)) return error.BadData;
    const imhd = std.mem.bytesAsValue(Imhd, imhd_raw[0..@sizeOf(Imhd)]);
    try writeRawBlock(gpa, .IMHD, imhd_raw, out_dir, out_path, 4, .{ .object = imhd.object_number }, code);

    var im_index: usize = 0;
    while (!blocks.atEnd()) : (im_index += 1) {
        const im_block_id = makeImBlockId(im_index) orelse return error.BadData;
        var im_blocks = try blocks.expect(im_block_id).nested();

        try code.appendSlice(gpa, "    im {\n");

        const smap = try im_blocks.expect(.SMAP).bytes();
        try writeRawBlock(gpa, .SMAP, smap, out_dir, out_path, 8, .{ .object_block = .{ imhd.object_number, im_block_id } }, code);

        if (!im_blocks.atEnd()) {
            const zp01 = try im_blocks.expect(.ZP01).bytes();
            try writeRawBlock(gpa, .ZP01, zp01, out_dir, out_path, 8, .{ .object_block = .{ imhd.object_number, im_block_id } }, code);
        }

        try im_blocks.finish();

        try code.appendSlice(gpa, "    }\n");
    }

    try blocks.finish();

    try code.appendSlice(gpa, "}\n");
}

pub fn makeImBlockId(index: usize) ?BlockId {
    const chars = "123456789AB";

    if (index >= chars.len) return null;

    var result = BlockId.IM00;
    std.mem.asBytes(&result)[3] = chars[index];
    return result;
}

const Imhd = extern struct {
    object_number: u16,
    // TODO: fill in the rest
};
