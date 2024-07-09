const std = @import("std");

const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const BMCOMP_NMAJMIN_H8 = @import("rmim.zig").BMCOMP_NMAJMIN_H8;

const num_colors = 256;

pub fn encode(bmp_raw: []const u8, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    const info_header, const pixels = try bmp.readHeader(bmp_raw);

    const width: u32 = @intCast(info_header.biWidth);
    const top_down = info_header.biHeight < 0;

    if (width & 3 != 0)
        return error.BadData; // TODO: handle stride

    const rmih_fixup = try beginBlock(out, "RMIH");
    try out.writer().writeInt(u16, 0, .little);
    try endBlock(out, fixups, rmih_fixup);

    const im00_fixup = try beginBlock(out, "IM00");

    const bmap_fixup = try beginBlock(out, "BMAP");

    try out.writer().writeByte(BMCOMP_NMAJMIN_H8);
    try compressBmap(width, top_down, pixels, out.writer());

    try endBlock(out, fixups, bmap_fixup);

    try endBlock(out, fixups, im00_fixup);
}

fn compressBmap(width: u32, top_down: bool, pixels: []const u8, writer: anytype) !void {
    var out = std.io.bitWriter(.little, writer);

    var pixit = PixelIter.init(width, top_down, pixels);

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

const PixelIter = union(enum) {
    top_down: struct {
        pixels: []const u8,
        i: u32,
    },
    bottom_up: struct {
        pixels: []const u8,
        i: u32,
        width: u32,
        x: u32,
    },

    fn init(width: u32, top_down: bool, pixels: []const u8) PixelIter {
        if (top_down)
            return .{ .top_down = .{
                .pixels = pixels,
                .i = 0,
            } };

        const i: u32 = @intCast(pixels.len - 1);
        return .{ .bottom_up = .{
            .pixels = pixels,
            .width = width,
            .i = i,
            .x = width - i % width,
        } };
    }

    fn next(self: *PixelIter) ?u8 {
        switch (self.*) {
            .top_down => |*s| {
                const result = s.pixels[s.i];

                s.i += 1;
                if (s.i >= s.pixels.len)
                    return null;

                return result;
            },
            .bottom_up => |*s| {
                const result = s.pixels[s.i];

                s.i += 1;
                s.x -= 1;
                if (s.x == 0) {
                    const feed = s.width * 2;
                    if (s.i < feed)
                        return null;
                    s.i -= feed;
                    s.x = s.width;
                }

                return result;
            },
        }
    }
};
