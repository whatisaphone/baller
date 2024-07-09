const std = @import("std");

const blockReader = @import("block_reader.zig").blockReader;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const rmim = @import("rmim.zig");

pub fn decode(
    allocator: std.mem.Allocator,
    awiz_raw: []const u8,
    rmda_raw: []const u8,
) ![]const u8 {
    const apal = try rmim.findApalInRmda(rmda_raw);

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
    try bmp.writePalette(bmp_writer.writer(), apal);

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

pub fn encode(bmp_raw: []const u8, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    const header, const pixels = try bmp.readHeader(bmp_raw);

    const width: u31 = @intCast(header.biWidth);
    // TODO: handle bottom-up
    std.debug.assert(header.biHeight < 0);
    const height: u31 = @intCast(-header.biHeight);

    // TODO: all these numbers are hardcoded and wrong

    const cnvs_fixup = try beginBlock(out, "CNVS");
    try out.writer().writeInt(i32, 480, .little);
    try out.writer().writeInt(i32, 120, .little);
    try endBlock(out, fixups, cnvs_fixup);

    const spot_fixup = try beginBlock(out, "SPOT");
    try out.writer().writeInt(i32, 6, .little);
    try out.writer().writeInt(i32, 5, .little);
    try endBlock(out, fixups, spot_fixup);

    const relo_fixup = try beginBlock(out, "RELO");
    try out.writer().writeInt(i32, 448, .little);
    try out.writer().writeInt(i32, 88, .little);
    try endBlock(out, fixups, relo_fixup);

    const wizh_fixup = try beginBlock(out, "WIZH");
    try out.writer().writeInt(i32, 1, .little); // compression type RLE
    try out.writer().writeInt(i32, width, .little);
    try out.writer().writeInt(i32, height, .little);
    try endBlock(out, fixups, wizh_fixup);

    const wizd_fixup = try beginBlock(out, "WIZD");
    try encodeRle(header, pixels, out.writer());
    try endBlock(out, fixups, wizd_fixup);
}

fn encodeRle(
    header: *align(1) const bmp.BITMAPINFOHEADER,
    pixels: []const u8,
    out: anytype,
) !void {
    var rows = bmp.RowIter.init(header, pixels);
    while (rows.next()) |row| {
        try out.writeInt(i16, @intCast(row.len * 2), .little);
        for (row) |px| {
            try out.writeByte(0);
            try out.writeByte(px);
        }
    }
}
