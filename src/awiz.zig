const std = @import("std");

const blockReader = @import("block_reader.zig").blockReader;
const bmp = @import("bmp.zig");
const io = @import("io.zig");

pub fn decode(allocator: std.mem.Allocator, awiz_raw: []const u8) ![]const u8 {
    var buf_reader = std.io.fixedBufferStream(awiz_raw);
    var reader = std.io.countingReader(buf_reader.reader());
    var awiz_blocks = blockReader(&reader);

    const wizh_len = awiz_blocks.skipUntilBlock("WIZH") catch |err| {
        if (err == error.EndOfStream)
            return error.DecodeAwiz;
        return err;
    };
    if (wizh_len != 12)
        return error.BadData;
    const compression = try reader.reader().readInt(i32, .little);
    const width_signed = try reader.reader().readInt(i32, .little);
    const width = std.math.cast(u31, width_signed) orelse return error.BadData;
    const height_signed = try reader.reader().readInt(i32, .little);
    const height = std.math.cast(u31, height_signed) orelse return error.BadData;

    if (compression != 1) // 1 is RLE
        return error.BadData;

    const wizd_len = try awiz_blocks.expectBlock("WIZD");
    const wizd_end = buf_reader.pos + wizd_len;

    const bmp_file_size = bmp.calcFileSize(width, height);
    const bmp_buf = try allocator.alloc(u8, bmp_file_size);
    errdefer allocator.free(bmp_buf);

    var bmp_writer = std.io.fixedBufferStream(bmp_buf);

    try bmp.writeHeader(bmp_writer.writer(), width, height, bmp_file_size);

    // TODO: actual palette
    for (0..256) |i_usize| {
        const i: u8 = @intCast(i_usize);
        try bmp_writer.writer().writeAll(&.{ i, i, i, 0 });
    }

    // based on ScummVM's auxDecompTRLEPrim
    for (0..height) |_| {
        const line_size = try reader.reader().readInt(u16, .little);
        const line_end = buf_reader.pos + line_size;

        while (buf_reader.pos < line_end) {
            const n = try reader.reader().readByte();
            if (n & 1 != 0) {
                const count = n >> 1;
                try bmp_writer.writer().writeByteNTimes(0, count);
            } else if (n & 2 != 0) {
                const count = (n >> 2) + 1;
                const color = try reader.reader().readByte();
                try bmp_writer.writer().writeByteNTimes(color, count);
            } else {
                const count = (n >> 2) + 1;
                try io.copy(std.io.limitedReader(reader.reader(), count), bmp_writer.writer());
            }
        }

        try bmp.padRow(bmp_writer.writer(), width);
    }

    // Allow one byte of padding from the encoder
    if (buf_reader.pos < wizd_end)
        _ = try reader.reader().readByte();

    try awiz_blocks.finishEof();

    return bmp_buf;
}
