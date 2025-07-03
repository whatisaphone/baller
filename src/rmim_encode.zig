const std = @import("std");

const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const bmp = @import("bmp.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const report = @import("report.zig");
const Compression = @import("rmim.zig").Compression;

pub fn encode(
    gpa: std.mem.Allocator,
    target: games.Target,
    compression: u8,
    bmp_raw: []u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const header = try bmp.readHeader(bmp_raw);

    const bmap_fixup = try beginBlockAl(gpa, out, .BMAP);

    try out.append(gpa, compression);
    try compressBmap(header, target, compression, out.writer(gpa));

    endBlockAl(out, bmap_fixup);
}

fn compressBmap(
    header: bmp.Bmp,
    target: games.Target,
    compression: u8,
    writer: anytype,
) !void {
    switch (compression) {
        Compression.BMCOMP_NMAJMIN_H4,
        Compression.BMCOMP_NMAJMIN_H5,
        Compression.BMCOMP_NMAJMIN_H7,
        Compression.BMCOMP_NMAJMIN_H8,
        Compression.BMCOMP_NMAJMIN_HT4,
        Compression.BMCOMP_NMAJMIN_HT8,
        => {
            try compressBmapNMajMin(header, target, compression, writer);
        },
        Compression.BMCOMP_SOLID_COLOR_FILL => {
            try compressBmapSolidColorFill(header, writer);
        },
        else => return error.BadData,
    }
}

fn compressBmapNMajMin(
    header: bmp.Bmp,
    target: games.Target,
    compression: u8,
    writer: anytype,
) !void {
    const color_bits: u8 = switch (compression) {
        Compression.BMCOMP_NMAJMIN_H4, Compression.BMCOMP_NMAJMIN_HT4 => 4,
        Compression.BMCOMP_NMAJMIN_H5 => 5,
        Compression.BMCOMP_NMAJMIN_H7 => 7,
        Compression.BMCOMP_NMAJMIN_H8, Compression.BMCOMP_NMAJMIN_HT8 => 8,
        else => unreachable,
    };
    const max_pixel: u8 = @intCast((@as(u9, 1) << @intCast(color_bits)) - 1);

    var out = std.io.bitWriter(.little, writer);

    var current = header.pixels[0];
    if (current > max_pixel)
        return reportColorOutOfRange(current);
    try out.writeBits(current, 8);

    var it = header.iterRows();
    var y: u32 = 0;
    while (it.next()) |full_row| : (y += 1) {
        const row = if (y == 0) full_row[1..] else full_row;
        for (row) |pixel| {
            if (pixel > max_pixel)
                return reportColorOutOfRange(pixel);

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
    }

    try out.flushBits();

    // Older versions pad the output to an even number of bytes
    if (target.le(.sputm99) and writer.context.self.items.len & 1 != 0)
        try writer.writeByte(0);
}

fn reportColorOutOfRange(pixel: u8) error{Reported} {
    report.fatal("color index {} too large for encoding", .{pixel});
    return error.Reported;
}

fn compressBmapSolidColorFill(header: bmp.Bmp, writer: anytype) !void {
    const color = header.pixels[0];
    if (!std.mem.allEqual(u8, header.pixels[1..], color)) return error.BadData;
    try writer.writeByte(color);
}
