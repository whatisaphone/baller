const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const bmp = @import("bmp.zig");
const utils = @import("utils.zig");

pub const Compression = struct {
    pub const BMCOMP_NMAJMIN_H7 = 137;
    pub const BMCOMP_NMAJMIN_H8 = 138;
    pub const BMCOMP_NMAJMIN_HT8 = 148;
};

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
    diag: *const Diagnostic.ForBinaryFile,
    apal: *const [0x300]u8,
) !Rmim {
    const width = 640; // TODO: use the real size
    const height = 480;

    var rmim_reader = std.io.fixedBufferStream(rmim_raw);
    var rmim_blocks = fixedBlockReader(&rmim_reader, diag);

    _ = try rmim_blocks.expect(.RMIH).bytes();

    var im00_blocks = try rmim_blocks.expect(.IM00).nested();

    const bmap = try im00_blocks.expect(.BMAP).block();
    const bmap_end = bmap.end();

    const compression = try rmim_reader.reader().readByte();

    const bmp_size = bmp.calcFileSize(width, height);
    var out: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_size);
    errdefer out.deinit(allocator);

    try bmp.writeHeader(out.writer(allocator), width, height, bmp_size);
    try bmp.writePalette(out.writer(allocator), apal);
    try decompressBmap(diag, compression, &rmim_reader, bmap_end, out.writer(allocator));

    if (!im00_blocks.atEnd()) {
        const block = try im00_blocks.next().block();
        diag.err(block.offset(), "skipping RMIM due to trailing {}", .{block.id});
        return error.AddedToDiagnostic;
    }

    try im00_blocks.finish();

    try rmim_blocks.finish();

    return .{
        .compression = compression,
        .bmp = out,
    };
}

fn decompressBmap(
    diag: *const Diagnostic.ForBinaryFile,
    compression: u8,
    reader: anytype,
    end: u32,
    out: anytype,
) !void {
    switch (compression) {
        Compression.BMCOMP_NMAJMIN_H7,
        Compression.BMCOMP_NMAJMIN_H8,
        Compression.BMCOMP_NMAJMIN_HT8,
        => {
            try decompressBmapNMajMin(compression, reader, end, out);
        },
        else => {
            diag.err(@intCast(reader.pos), "unsupported BMAP compression {}", .{compression});
            return error.AddedToDiagnostic;
        },
    }
}

fn decompressBmapNMajMin(compression: u8, reader: anytype, end: u32, out: anytype) !void {
    const delta: [8]i8 = .{ -4, -3, -2, -1, 1, 2, 3, 4 };

    var in = std.io.bitReader(.little, reader.reader());

    const color_bits: u8 = switch (compression) {
        Compression.BMCOMP_NMAJMIN_H7 => 7,
        Compression.BMCOMP_NMAJMIN_H8, Compression.BMCOMP_NMAJMIN_HT8 => 8,
        else => unreachable,
    };

    var color = try in.readBitsNoEof(u8, 8);
    while (reader.pos < end or in.count != 0) {
        try out.writeByte(color);
        if (try in.readBitsNoEof(u1, 1) != 0) {
            if (try in.readBitsNoEof(u1, 1) != 0) {
                const d = try in.readBitsNoEof(u3, 3);
                color = utils.add(u8, color, delta[d]) orelse return error.BadData;
            } else {
                color = try in.readBitsNoEof(u8, color_bits);
            }
        }
    }
}
