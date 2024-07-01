const std = @import("std");

const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const io = @import("io.zig");
const BMCOMP_NMAJMIN_H8 = @import("rmim.zig").BMCOMP_NMAJMIN_H8;

const num_colors = 256;

pub fn encode(bmp_raw: []const u8, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    const pixels_start = bitmap_file_header_size + bitmap_info_header_size + 4 * num_colors;
    if (bmp_raw.len < pixels_start + 1)
        return error.BadData;

    const file_header = std.mem.bytesAsValue(
        BITMAPFILEHEADER,
        bmp_raw[0..bitmap_file_header_size],
    );
    if (file_header.bfType != std.mem.bytesToValue(u16, "BM"))
        return error.BadData;

    const info_header = std.mem.bytesAsValue(
        BITMAPINFOHEADER,
        bmp_raw[bitmap_file_header_size..][0..bitmap_info_header_size],
    );

    if (info_header.biSize != bitmap_info_header_size or
        info_header.biBitCount != 8 or
        info_header.biCompression != BI_RGB or
        !(info_header.biClrUsed == 0 or info_header.biClrUsed == 256))
        return error.BadData;

    const rmih_fixup = try beginBlock(out, "RMIH");
    try out.writer().writeInt(u16, 0, .little);
    try endBlock(out, fixups, rmih_fixup);

    const im00_fixup = try beginBlock(out, "IM00");

    const bmap_fixup = try beginBlock(out, "BMAP");

    try out.writer().writeByte(BMCOMP_NMAJMIN_H8);
    try compressBmap(bmp_raw[pixels_start..], out.writer());

    try endBlock(out, fixups, bmap_fixup);

    try endBlock(out, fixups, im00_fixup);
}

fn compressBmap(pixels: []const u8, writer: anytype) !void {
    var out = std.io.bitWriter(.little, writer);
    // TODO: this doesn't actually compress
    try out.writeBits(pixels[0], 8);
    for (pixels[1..]) |pixel| {
        try out.writeBits(@as(u2, 1), 2);
        try out.writeBits(pixel, 8);
    }
}

const bitmap_file_header_size = 14;
const bitmap_info_header_size = 40;

const BI_RGB = 0;

const BITMAPFILEHEADER = packed struct {
    bfType: u16,
    bfSize: u32,
    bfReserved1: u16,
    bfReserved2: u16,
    bfOffBits: u32,
};

const BITMAPINFOHEADER = packed struct {
    biSize: u32,
    biWidth: i32,
    biHeight: i32,
    biPlanes: u16,
    biBitCount: u16,
    biCompression: u32,
    biSizeImage: u32,
    biXPelsPerMeter: i32,
    biYPelsPerMeter: i32,
    biClrUsed: u32,
    biClrImportant: u32,
};
