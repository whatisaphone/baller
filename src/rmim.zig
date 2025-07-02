const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const bmp = @import("bmp.zig");
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const utils = @import("utils.zig");

pub const Compression = struct {
    pub const BMCOMP_NMAJMIN_H4 = 134;
    pub const BMCOMP_NMAJMIN_H5 = 135;
    pub const BMCOMP_NMAJMIN_H7 = 137;
    pub const BMCOMP_NMAJMIN_H8 = 138;
    pub const BMCOMP_NMAJMIN_HT4 = 144;
    pub const BMCOMP_NMAJMIN_HT8 = 148;
    pub const BMCOMP_SOLID_COLOR_FILL = 150;
};

pub fn decode(
    allocator: std.mem.Allocator,
    rmim_raw: []const u8,
    diag: *const Diagnostic.ForBinaryFile,
    apal: *const [0x300]u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    const width = 640; // TODO: use the real size
    const height = 480;

    try code.appendSlice(allocator, "rmim {\n");

    var rmim_reader = std.io.fixedBufferStream(rmim_raw);
    var rmim_blocks = fixedBlockReader(&rmim_reader, diag);

    const rmih = try rmim_blocks.expect(.RMIH).bytes();
    try writeRawBlock(allocator, .RMIH, rmih, out_dir, out_path, 4, .block, code);

    var im00_blocks = try rmim_blocks.expect(.IM00).nested();

    try code.appendSlice(allocator, "    im {\n");

    const bmap = try im00_blocks.expect(.BMAP).block();
    const bmap_end = bmap.end();

    const compression = try rmim_reader.reader().readByte();

    const bmp_size = bmp.calcFileSize(width, height);
    var out: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_size);
    defer out.deinit(allocator);

    try bmp.writeHeader(out.fixedWriter(), width, height, bmp_size);
    try bmp.writePalette(out.fixedWriter(), apal);
    try decompressBmap(diag, compression, &rmim_reader, bmap_end, &out);

    // All decompressors must fully fill the buffer
    std.debug.assert(out.items.len == out.capacity);

    try fs.writeFileZ(out_dir, "RMIM.bmp", out.items);
    try code.writer(allocator).print(
        "        bmap {} \"{s}/{s}\"\n",
        .{ compression, out_path, "RMIM.bmp" },
    );

    while (!im00_blocks.atEnd()) {
        const block = try im00_blocks.next().block();
        const bytes = try io.readInPlace(&rmim_reader, block.size);
        try writeRawBlock(allocator, block.id, bytes, out_dir, out_path, 8, .{ .block_block = .RMIM }, code);
    }

    try im00_blocks.finish();

    try rmim_blocks.finish();

    try code.appendSlice(allocator, "    }\n}\n");
}

fn decompressBmap(
    diag: *const Diagnostic.ForBinaryFile,
    compression: u8,
    reader: anytype,
    end: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    switch (compression) {
        Compression.BMCOMP_NMAJMIN_H4,
        Compression.BMCOMP_NMAJMIN_H5,
        Compression.BMCOMP_NMAJMIN_H7,
        Compression.BMCOMP_NMAJMIN_H8,
        Compression.BMCOMP_NMAJMIN_HT4,
        Compression.BMCOMP_NMAJMIN_HT8,
        => {
            try decompressBmapNMajMin(compression, reader, end, out);
        },
        Compression.BMCOMP_SOLID_COLOR_FILL => {
            try decompressBmapSolidColorFill(reader, end, out);
        },
        else => {
            diag.err(@intCast(reader.pos), "unsupported BMAP compression {}", .{compression});
            return error.AddedToDiagnostic;
        },
    }
}

fn decompressBmapNMajMin(
    compression: u8,
    reader: anytype,
    end: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const delta: [8]i8 = .{ -4, -3, -2, -1, 1, 2, 3, 4 };

    var in = std.io.bitReader(.little, reader.reader());

    const color_bits: u8 = switch (compression) {
        Compression.BMCOMP_NMAJMIN_H4, Compression.BMCOMP_NMAJMIN_HT4 => 4,
        Compression.BMCOMP_NMAJMIN_H5 => 5,
        Compression.BMCOMP_NMAJMIN_H7 => 7,
        Compression.BMCOMP_NMAJMIN_H8, Compression.BMCOMP_NMAJMIN_HT8 => 8,
        else => unreachable,
    };

    var color = try in.readBitsNoEof(u8, 8);
    while (reader.pos < end or in.count != 0) {
        if (out.items.len == out.capacity) break;

        out.appendAssumeCapacity(color);

        if (try in.readBitsNoEof(u1, 1) != 0) {
            if (try in.readBitsNoEof(u1, 1) != 0) {
                const d = try in.readBitsNoEof(u3, 3);
                color = utils.add(u8, color, delta[d]) orelse return error.BadData;
            } else {
                color = try in.readBitsNoEof(u8, color_bits);
            }
        }
    }

    // Allow at most one extra input byte. The encoder is allowed to pad the
    // compressed data to an even number of bytes.
    if (reader.pos != end) {
        _ = reader.reader().readByte() catch unreachable;
        if (reader.pos != end)
            return error.BadData;
    }

    // Allow the output to be missing at most one pixel. I'm not sure why this
    // happens.
    if (out.items.len != out.capacity) {
        out.appendAssumeCapacity(out.getLast());
        if (out.items.len != out.capacity)
            return error.BadData;
    }
}

fn decompressBmapSolidColorFill(
    reader: anytype,
    end: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const color = try reader.reader().readByte();
    if (reader.pos != end) return error.BadData;

    @memset(out.unusedCapacitySlice(), color);
    out.items.len = out.capacity;
}
