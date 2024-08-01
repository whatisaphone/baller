const std = @import("std");

const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const BMCOMP_NMAJMIN_H8 = @import("rmim.zig").BMCOMP_NMAJMIN_H8;
const BMCOMP_NMAJMIN_HT8 = @import("rmim.zig").BMCOMP_NMAJMIN_HT8;

const num_colors = 256;

pub fn encode(
    compression: u8,
    bmp_raw: []const u8,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    const header = try bmp.readHeader(bmp_raw);

    if (header.width() & 3 != 0)
        return error.BadData; // TODO: handle stride

    const rmih_fixup = try beginBlock(out, "RMIH");
    try out.writer().writeInt(u16, 0, .little);
    try endBlock(out, fixups, rmih_fixup);

    const im00_fixup = try beginBlock(out, "IM00");

    const bmap_fixup = try beginBlock(out, "BMAP");

    // only these are implemented
    if (compression != BMCOMP_NMAJMIN_H8 and compression != BMCOMP_NMAJMIN_HT8)
        return error.BadData;
    try out.writer().writeByte(compression);
    try compressBmap(header, out.writer());

    try endBlock(out, fixups, bmap_fixup);

    try endBlock(out, fixups, im00_fixup);
}

fn compressBmap(header: bmp.Bmp, writer: anytype) !void {
    var out = std.io.bitWriter(.little, writer);

    var pixit = header.iterPixels();

    var current = pixit.next() orelse return error.EndOfStream;
    try out.writeBits(current, 8);

    while (pixit.next()) |pixel| {
        const diff = @as(i16, pixel) - current;
        if (diff == 0) {
            try out.writeBits(@as(u1, 0), 1);
        } else if (-4 <= diff and diff < 0) {
            try out.writeBits(@as(u2, 3), 2);
            try out.writeBits(@as(u3, @intCast(diff + 4)), 3);
        } else if (0 < diff and diff <= 4) {
            try out.writeBits(@as(u2, 3), 2);
            try out.writeBits(@as(u3, @intCast(diff + 3)), 3);
        } else {
            try out.writeBits(@as(u2, 1), 2);
            try out.writeBits(pixel, 8);
        }

        current = pixel;
    }

    try out.flushBits();
}
