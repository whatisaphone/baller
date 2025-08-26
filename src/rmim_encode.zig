const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const bmp = @import("bmp.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const Compression = @import("rmim.zig").Compression;

pub fn encode(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    target: games.Target,
    compression: u8,
    bmp_raw: []u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const header = try bmp.readHeaderDiag(bmp_raw, diagnostic, loc);

    const bmap_fixup = try beginBlockAl(gpa, out, .BMAP);

    try out.append(gpa, compression);
    try compressBmap(diagnostic, loc, header, target, compression, out.writer(gpa));

    endBlockAl(out, bmap_fixup);
}

fn compressBmap(
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
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
            try writer.context.self.ensureUnusedCapacity(writer.context.allocator, 32 << 10);
            try compressBmapNMajMin(diagnostic, loc, header, target, compression, writer);
        },
        Compression.BMCOMP_SOLID_COLOR_FILL => {
            try compressBmapSolidColorFill(header, writer);
        },
        else => return error.BadData,
    }
}

fn compressBmapNMajMin(
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
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
        return reportColorOutOfRange(diagnostic, loc, current);
    try out.writeBits(current, 8);

    var it = header.iterRows();
    var y: u32 = 0;
    while (it.next()) |full_row| : (y += 1) {
        const row = if (y == 0) full_row[1..] else full_row;
        for (row) |pixel| {
            if (pixel > max_pixel)
                return reportColorOutOfRange(diagnostic, loc, pixel);

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

fn reportColorOutOfRange(
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    pixel: u8,
) error{AddedToDiagnostic} {
    diagnostic.errAt(loc, "color index {} too large for encoding", .{pixel});
    return error.AddedToDiagnostic;
}

fn compressBmapSolidColorFill(header: bmp.Bmp, writer: anytype) !void {
    const color = header.pixels[0];
    if (!std.mem.allEqual(u8, header.pixels[1..], color)) return error.BadData;
    try writer.writeByte(color);
}
