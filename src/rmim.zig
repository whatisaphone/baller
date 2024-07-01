const std = @import("std");

const blockReader = @import("block_reader.zig").blockReader;
const io = @import("io.zig");

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

    _ = try rmda_blocks.skipUntilBlock("PALS");
    var pals_blocks = blockReader(&rmda_reader);

    _ = try pals_blocks.expectBlock("WRAP");
    var wrap_blocks = blockReader(&rmda_reader);

    const offs_len = try wrap_blocks.expectBlock("OFFS");
    try rmda_reader.reader().skipBytes(offs_len, .{});

    const apal_len = try wrap_blocks.expectBlock("APAL");
    if (apal_len != 0x300)
        return error.BadData;
    const apal = rmda_raw[rmda_reader.bytes_read..][0..0x300];
    try rmda_reader.reader().skipBytes(apal_len, .{});

    try wrap_blocks.checkSync();

    try pals_blocks.checkSync();

    try rmda_blocks.checkSync();

    // RMIM

    var rmim_buf_reader = std.io.fixedBufferStream(rmim_raw);
    var rmim_reader = std.io.countingReader(rmim_buf_reader.reader());
    var rmim_blocks = blockReader(&rmim_reader);

    const rmih_len = try rmim_blocks.expectBlock("RMIH");
    try rmim_reader.reader().skipBytes(rmih_len, .{});

    _ = try rmim_blocks.expectBlock("IM00");
    var im00_blocks = blockReader(&rmim_reader);

    const bmap_len = try im00_blocks.expectBlock("BMAP");
    const bmap_end: u32 = @intCast(rmim_reader.bytes_read + bmap_len);

    const bmp_size = calcBmpFileSize(width, height);
    var out = try std.ArrayListUnmanaged(u8).initCapacity(allocator, bmp_size);
    errdefer out.deinit(allocator);

    try writeBmpHeader(out.writer(allocator), width, height, bmp_size);
    try writeBmpPalette(out.writer(allocator), apal);
    try decompressBmap(&rmim_reader, bmap_end, out.writer(allocator));

    try im00_blocks.checkSync();

    try rmim_blocks.checkSync();
    try io.requireEof(rmim_reader.reader());

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
    while (reader.bytes_read < end) {
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

const bitmap_file_header_size = 14;
const bitmap_info_header_size = 40;
const num_colors = 256;

fn calcBmpFileSize(width: u31, height: u31) u32 {
    return bitmap_file_header_size + bitmap_info_header_size + 4 * num_colors + width * height;
}

fn calcBmpDataStart() u32 {
    return bitmap_file_header_size + bitmap_info_header_size + 4 * num_colors;
}

fn writeBmpHeader(out: anytype, width: u31, height: u31, file_size: u32) !void {
    // BITMAPFILEHEADER
    try out.writeAll("BM");
    try out.writeInt(u32, file_size, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u32, calcBmpDataStart(), .little);

    // BITMAPINFOHEADER
    try out.writeInt(u32, bitmap_info_header_size, .little);
    try out.writeInt(i32, width, .little);
    try out.writeInt(i32, -@as(i32, height), .little);
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u16, 8, .little);
    try out.writeInt(u32, 0, .little); // BI_RGB
    try out.writeInt(u32, 0, .little);
    try out.writeInt(i32, 0, .little);
    try out.writeInt(i32, 0, .little);
    try out.writeInt(u32, 0, .little);
    try out.writeInt(u32, 0, .little);
}

fn writeBmpPalette(out: anytype, pal: *const [0x300]u8) !void {
    var i: usize = 0;
    while (i < 0x300) {
        // convert 24-bit colors to 32-bit
        try out.writeAll(pal[i .. i + 3]);
        try out.writeByte(0);
        i += 3;
    }
}
