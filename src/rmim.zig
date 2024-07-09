const std = @import("std");

const blockIdToStr = @import("block_id.zig").blockIdToStr;
const blockReader = @import("block_reader.zig").blockReader;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const report = @import("report.zig");

pub const BMCOMP_NMAJMIN_H8 = 0x8a;

pub fn decode(
    allocator: std.mem.Allocator,
    rmim_raw: []const u8,
    rmda_raw: []const u8,
) !std.ArrayListUnmanaged(u8) {
    const width = 640; // TODO: use the real size
    const height = 480;

    // RMDA

    var rmda_buf_reader = std.io.fixedBufferStream(rmda_raw);
    var rmda_reader = std.io.countingReader(rmda_buf_reader.reader());
    var rmda_blocks = blockReader(&rmda_reader);

    const pals_len = try rmda_blocks.skipUntilBlock("PALS");
    const pals_end: u32 = @intCast(rmda_reader.bytes_read + pals_len);
    var pals_blocks = blockReader(&rmda_reader);

    const wrap_len = try pals_blocks.expectBlock("WRAP");
    const wrap_end: u32 = @intCast(rmda_reader.bytes_read + wrap_len);
    var wrap_blocks = blockReader(&rmda_reader);

    const offs_len = try wrap_blocks.expectBlock("OFFS");
    try rmda_reader.reader().skipBytes(offs_len, .{});

    const apal_len = try wrap_blocks.expectBlock("APAL");
    if (apal_len != 0x300)
        return error.BadData;
    const apal = rmda_raw[rmda_reader.bytes_read..][0..0x300];
    try rmda_reader.reader().skipBytes(apal_len, .{});

    try wrap_blocks.finish(wrap_end);

    try pals_blocks.finish(pals_end);

    // Don't check EOF since we only care about PALS

    // RMIM

    var rmim_buf_reader = std.io.fixedBufferStream(rmim_raw);
    var rmim_reader = std.io.countingReader(rmim_buf_reader.reader());
    var rmim_blocks = blockReader(&rmim_reader);

    const rmih_len = try rmim_blocks.expectBlock("RMIH");
    try rmim_reader.reader().skipBytes(rmih_len, .{});

    const im00_len = try rmim_blocks.expectBlock("IM00");
    const im00_end: u32 = @intCast(rmim_reader.bytes_read + im00_len);
    var im00_blocks = blockReader(&rmim_reader);

    const bmap_len = try im00_blocks.expectBlock("BMAP");
    const bmap_end: u32 = @intCast(rmim_reader.bytes_read + bmap_len);

    const bmp_size = bmp.calcFileSize(width, height);
    var out = try std.ArrayListUnmanaged(u8).initCapacity(allocator, bmp_size);
    errdefer out.deinit(allocator);

    try bmp.writeHeader(out.writer(allocator), width, height, bmp_size);
    try bmp.writePalette(out.writer(allocator), apal);
    try decompressBmap(&rmim_reader, bmap_end, out.writer(allocator));

    if (rmim_reader.bytes_read != im00_end) {
        const id, _ = try im00_blocks.next();
        report.warn("skipping RMIM due to trailing {s}", .{blockIdToStr(&id)});
        return error.DecompressBmap;
    }

    try im00_blocks.finish(im00_end);

    try rmim_blocks.finishEof();

    return out;
}

fn decompressBmap(reader: anytype, end: u32, out: anytype) !void {
    const delta: [8]i8 = .{ -4, -3, -2, -1, 1, 2, 3, 4 };

    var in = std.io.bitReader(.little, reader.reader());

    const compression = try in.readBitsNoEof(u8, 8);
    // for now, only supporting BMCOMP_NMAJMIN_H8
    if (compression != BMCOMP_NMAJMIN_H8)
        return error.DecompressBmap;

    var color = try in.readBitsNoEof(u8, 8);
    while (reader.bytes_read < end or in.bit_count != 0) {
        try out.writeByte(color);
        if (try in.readBitsNoEof(u1, 1) != 0) {
            if (try in.readBitsNoEof(u1, 1) != 0) {
                const d = try in.readBitsNoEof(u3, 3);
                color +%= @as(u8, @bitCast(delta[d])); // TODO: how to properly do this
            } else {
                color = try in.readBitsNoEof(u8, 8);
            }
        }
    }
}
