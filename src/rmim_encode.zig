const std = @import("std");

const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const report = @import("report.zig");
const Compression = @import("rmim.zig").Compression;

pub fn encode(
    gpa: std.mem.Allocator,
    compression: u8,
    bmp_raw: []const u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const header = try bmp.readHeader(bmp_raw, .{ .skip_bounds_check = true });

    const rmih_fixup = try beginBlockAl(gpa, out, .RMIH);
    try out.appendNTimes(gpa, 0, 2);
    try endBlockAl(out, rmih_fixup);

    const im00_fixup = try beginBlockAl(gpa, out, .IM00);

    const bmap_fixup = try beginBlockAl(gpa, out, .BMAP);

    try out.append(gpa, compression);
    try compressBmap(header, compression, out.writer(gpa));

    try endBlockAl(out, bmap_fixup);

    try endBlockAl(out, im00_fixup);
}

fn compressBmap(header: bmp.Bmp, compression: u8, writer: anytype) !void {
    switch (compression) {
        Compression.BMCOMP_NMAJMIN_H7,
        Compression.BMCOMP_NMAJMIN_H8,
        Compression.BMCOMP_NMAJMIN_HT8,
        => {
            try compressBmapNMajMin(header, compression, writer);
        },
        Compression.BMCOMP_SOLID_COLOR_FILL => {
            try compressBmapSolidColorFill(header, writer);
        },
        else => return error.BadData,
    }
}

fn compressBmapNMajMin(header: bmp.Bmp, compression: u8, writer: anytype) !void {
    const color_bits: u8 = switch (compression) {
        Compression.BMCOMP_NMAJMIN_H7 => 7,
        Compression.BMCOMP_NMAJMIN_H8, Compression.BMCOMP_NMAJMIN_HT8 => 8,
        else => unreachable,
    };
    const max_pixel: u8 = @intCast((@as(u9, 1) << @intCast(color_bits)) - 1);

    var out = std.io.bitWriter(.little, writer);

    var pixit = try header.iterPixels();

    var current = pixit.next() orelse return error.EndOfStream;
    try out.writeBits(current, 8);

    while (pixit.next()) |pixel| {
        if (pixel > max_pixel) {
            report.fatal("color index {} too large for encoding", .{pixel});
            return error.Reported;
        }

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
            try out.writeBits(pixel, color_bits);
        }

        current = pixel;
    }

    try out.flushBits();
}

fn compressBmapSolidColorFill(header: bmp.Bmp, writer: anytype) !void {
    const color = header.pixels[0];
    if (!std.mem.allEqual(u8, header.pixels[1..], color)) return error.BadData;
    try writer.writeByte(color);
}
