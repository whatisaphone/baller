const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockReader = @import("block_reader.zig").blockReader;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const rmim = @import("rmim.zig");

const max_supported_width = 640;
const transparent = 255;

pub const Awiz = struct {
    blocks: std.BoundedArray(Block, 5) = .{},

    pub fn deinit(self: *Awiz, allocator: std.mem.Allocator) void {
        for (self.blocks.slice()) |*block| switch (block.*) {
            .two_ints, .wizh => {},
            .wizd => |*a| a.deinit(allocator),
        };
    }
};

const Block = union(enum) {
    two_ints: struct { id: BlockId, ints: [2]i32 },
    wizh,
    /// A raw BMP file
    wizd: std.ArrayListUnmanaged(u8),
};

pub fn decode(
    allocator: std.mem.Allocator,
    awiz_raw: []const u8,
    rmda_raw: []const u8,
) !Awiz {
    var result: Awiz = .{};
    var wizh_opt: ?struct {
        compression: i32,
        width: u31,
        height: u31,
    } = null;

    var buf_reader = std.io.fixedBufferStream(awiz_raw);
    var reader = std.io.countingReader(buf_reader.reader());
    var awiz_blocks = blockReader(&reader);

    const wizd_len = while (true) {
        const id, const len = try awiz_blocks.next();
        switch (id) {
            blockId("CNVS"), blockId("SPOT"), blockId("RELO") => {
                if (len != 8)
                    return error.BadData;
                const int1 = try reader.reader().readInt(i32, .little);
                const int2 = try reader.reader().readInt(i32, .little);

                try result.blocks.append(.{
                    .two_ints = .{
                        .id = id,
                        .ints = .{ int1, int2 },
                    },
                });
            },
            blockId("WIZH") => {
                if (len != 12)
                    return error.BadData;
                const compression = try reader.reader().readInt(i32, .little);
                const width_signed = try reader.reader().readInt(i32, .little);
                const width = std.math.cast(u31, width_signed) orelse return error.BadData;
                const height_signed = try reader.reader().readInt(i32, .little);
                const height = std.math.cast(u31, height_signed) orelse return error.BadData;

                wizh_opt = .{
                    .compression = compression,
                    .width = width,
                    .height = height,
                };

                try result.blocks.append(.wizh);
            },
            blockId("WIZD") => break len,
            else => return error.DecodeAwiz,
        }
    };
    const wizd_end = buf_reader.pos + wizd_len;

    const wizh = wizh_opt orelse return error.BadData;
    const width = wizh.width;
    const height = wizh.height;

    if (width > max_supported_width)
        return error.BadData;
    if (wizh.compression != 1) // 1 is RLE
        return error.BadData;

    const bmp_file_size = bmp.calcFileSize(width, height);
    var bmp_buf = try std.ArrayListUnmanaged(u8).initCapacity(allocator, bmp_file_size);
    errdefer bmp_buf.deinit(allocator);

    var bmp_writer = bmp_buf.writer(allocator);

    try bmp.writeHeader(bmp_writer, width, height, bmp_file_size);

    const apal = try rmim.findApalInRmda(rmda_raw);
    try bmp.writePalette(bmp_writer, apal);

    // based on ScummVM's auxDecompTRLEPrim
    for (0..height) |_| {
        const out_row_end = bmp_buf.items.len + width;

        const line_size = try reader.reader().readInt(u16, .little);
        const line_end = buf_reader.pos + line_size;

        while (buf_reader.pos < line_end) {
            const n = try reader.reader().readByte();
            if (n & 1 != 0) {
                const count = n >> 1;
                try bmp_writer.writeByteNTimes(transparent, count);
            } else if (n & 2 != 0) {
                const count = (n >> 2) + 1;
                const color = try reader.reader().readByte();
                try bmp_writer.writeByteNTimes(color, count);
            } else {
                const count = (n >> 2) + 1;
                try io.copy(std.io.limitedReader(reader.reader(), count), bmp_writer);
            }
        }

        try bmp_buf.appendNTimes(allocator, transparent, out_row_end - bmp_buf.items.len);

        try bmp.padRow(bmp_writer, width);
    }

    // Allow one byte of padding from the encoder
    if (buf_reader.pos < wizd_end)
        _ = try reader.reader().readByte();

    try result.blocks.append(.{ .wizd = bmp_buf });

    try awiz_blocks.finishEof();

    return result;
}

pub fn encode(wiz: *const Awiz, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    // First find the bitmap block so we can preload all data before we start

    const bmp_raw = for (wiz.blocks.slice()) |block| {
        if (block == .wizd)
            break block.wizd;
    } else return error.BadData;

    const header, const pixels = try bmp.readHeader(bmp_raw.items);
    const width: u31 = @intCast(header.biWidth);
    const height: u31 = @intCast(@abs(header.biHeight));

    // Now write the blocks in the requested order

    for (wiz.blocks.slice()) |block| switch (block) {
        .two_ints => |b| {
            const fixup = try beginBlockImpl(out, b.id);
            try out.writer().writeInt(i32, b.ints[0], .little);
            try out.writer().writeInt(i32, b.ints[1], .little);
            try endBlock(out, fixups, fixup);
        },
        .wizh => {
            const fixup = try beginBlock(out, "WIZH");
            try out.writer().writeInt(i32, 1, .little); // compression type RLE
            try out.writer().writeInt(i32, width, .little);
            try out.writer().writeInt(i32, height, .little);
            try endBlock(out, fixups, fixup);
        },
        .wizd => |_| {
            const fixup = try beginBlock(out, "WIZD");
            try encodeRle(header, pixels, out.writer());
            try endBlock(out, fixups, fixup);
        },
    };
}

fn encodeRle(
    header: *align(1) const bmp.BITMAPINFOHEADER,
    pixels: []const u8,
    out: anytype,
) !void {
    var rows = try bmp.RowIter.init(header, pixels);
    while (rows.next()) |row| {
        // worst-case encoding is 2 bytes for the line size, then 2 output bytes
        // for every input byte
        var line_buf = std.BoundedArray(u8, 2 + max_supported_width * 2){};

        // reserve space for line size, to be filled in later
        line_buf.len = 2;

        var i: usize = 0;
        while (i < row.len) {
            var run_len: u8 = 1;
            const color = row[i];
            i += 1;

            while (i < row.len and row[i] == color and i < 63) {
                i += 1;
                run_len += 1;
            }

            if (color == transparent) {
                if (i == row.len and line_buf.len == 2)
                    break;
                const n = 1 | @shlExact(run_len, 1);
                try line_buf.append(n);
            } else {
                // TODO: this is incomplete
                const n = 2 | @shlExact(run_len - 1, 2);
                try line_buf.append(n);
                try line_buf.append(color);
            }
        }

        // fill in line size
        std.mem.writeInt(i16, line_buf.buffer[0..2], line_buf.len - 2, .little);

        // flush line to output stream
        try out.writeAll(line_buf.slice());
    }
}
