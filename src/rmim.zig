const std = @import("std");

const oldFixedBlockReader = @import("block_reader.zig").oldFixedBlockReader;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const report = @import("report.zig");

pub const BMCOMP_NMAJMIN_H7 = 0x89;
pub const BMCOMP_NMAJMIN_H8 = 0x8a;
pub const BMCOMP_NMAJMIN_HT8 = 0x94;

const Rmim = struct {
    compression: u8,
    bmp: std.ArrayListUnmanaged(u8),

    pub fn deinit(self: *Rmim, allocator: std.mem.Allocator) void {
        self.bmp.deinit(allocator);
    }
};

pub fn decode(
    allocator: std.mem.Allocator,
    rmim_raw: []const u8,
    apal: *const [0x300]u8,
) !Rmim {
    const width = 640; // TODO: use the real size
    const height = 480;

    var rmim_reader = std.io.fixedBufferStream(rmim_raw);
    var rmim_blocks = oldFixedBlockReader(&rmim_reader);

    const rmih_len = try rmim_blocks.expect(.RMIH);
    _ = try io.readInPlace(&rmim_reader, rmih_len);

    const im00_len = try rmim_blocks.expect(.IM00);
    const im00_end: u32 = @intCast(rmim_reader.pos + im00_len);
    var im00_blocks = oldFixedBlockReader(&rmim_reader);

    const bmap_len = try im00_blocks.expect(.BMAP);
    const bmap_end: u32 = @intCast(rmim_reader.pos + bmap_len);

    const compression = try rmim_reader.reader().readByte();

    const bmp_size = bmp.calcFileSize(width, height);
    var out: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_size);
    errdefer out.deinit(allocator);

    try bmp.writeHeader(out.writer(allocator), width, height, bmp_size);
    try bmp.writePalette(out.writer(allocator), apal);
    try decompressBmap(compression, &rmim_reader, bmap_end, out.writer(allocator));

    if (try im00_blocks.peek()) |id| {
        report.warn("skipping RMIM due to trailing {}", .{id});
        return error.DecompressBmap;
    }

    try im00_blocks.finish(im00_end);

    try rmim_blocks.finishEof();

    return .{
        .compression = compression,
        .bmp = out,
    };
}

pub fn findApalInRmda(rmda_raw: []const u8) !*const [0x300]u8 {
    var rmda_reader = std.io.fixedBufferStream(rmda_raw);
    var rmda_blocks = oldFixedBlockReader(&rmda_reader);

    const pals_len = try rmda_blocks.skipUntilBlock("PALS");
    const pals_end: u32 = @intCast(rmda_reader.pos + pals_len);
    var pals_blocks = oldFixedBlockReader(&rmda_reader);

    const wrap_len = try pals_blocks.expect(.WRAP);
    const wrap_end: u32 = @intCast(rmda_reader.pos + wrap_len);
    var wrap_blocks = oldFixedBlockReader(&rmda_reader);

    const offs_len = try wrap_blocks.expect(.OFFS);
    _ = try io.readInPlace(&rmda_reader, offs_len);

    const apal_len = try wrap_blocks.expect(.APAL);
    const expected_apal_len = 0x300;
    if (apal_len != expected_apal_len)
        return error.BadData;
    const result = try io.readInPlaceBytes(&rmda_reader, expected_apal_len);

    try wrap_blocks.finish(wrap_end);

    try pals_blocks.finish(pals_end);

    // Don't check EOF since we only care about PALS
    try rmda_blocks.checkSync();

    return result;
}

fn decompressBmap(compression: u8, reader: anytype, end: u32, out: anytype) !void {
    const delta: [8]i8 = .{ -4, -3, -2, -1, 1, 2, 3, 4 };

    var in = std.io.bitReader(.little, reader.reader());

    const color_bits: u8 = switch (compression) {
        BMCOMP_NMAJMIN_H7 => 7,
        BMCOMP_NMAJMIN_H8, BMCOMP_NMAJMIN_HT8 => 8,
        else => return error.DecompressBmap,
    };

    var color = try in.readBitsNoEof(u8, 8);
    while (reader.pos < end or in.count != 0) {
        try out.writeByte(color);
        if (try in.readBitsNoEof(u1, 1) != 0) {
            if (try in.readBitsNoEof(u1, 1) != 0) {
                const d = try in.readBitsNoEof(u3, 3);
                color +%= @as(u8, @bitCast(delta[d])); // TODO: how to properly do this
            } else {
                color = try in.readBitsNoEof(u8, color_bits);
            }
        }
    }
}
