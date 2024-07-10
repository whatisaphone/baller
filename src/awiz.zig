const std = @import("std");

const blockReader = @import("block_reader.zig").blockReader;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const rmim = @import("rmim.zig");

const max_supported_width = 640;
const transparent = 255;

pub fn decode(
    allocator: std.mem.Allocator,
    awiz_raw: []const u8,
    rmda_raw: []const u8,
) !std.ArrayListUnmanaged(u8) {
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

    if (width > max_supported_width)
        return error.BadData;
    if (compression != 1) // 1 is RLE
        return error.BadData;

    const wizd_len = try awiz_blocks.expectBlock("WIZD");
    const wizd_end = buf_reader.pos + wizd_len;

    const bmp_file_size = bmp.calcFileSize(width, height);
    var bmp_buf = try std.ArrayListUnmanaged(u8).initCapacity(allocator, bmp_file_size);
    errdefer bmp_buf.deinit(allocator);

    var bmp_writer = bmp_buf.writer(allocator);

    try bmp.writeHeader(bmp_writer, width, height, bmp_file_size);
    try bmp.writePalette(bmp_writer, apal);

    // based on ScummVM's auxDecompTRLEPrim
    for (0..height) |_| {
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

        try bmp.padRow(bmp_writer, width);
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

            while (i < row.len and row[i] == color and i < 127) {
                i += 1;
                run_len += 1;
            }

            if (color == transparent) {
                const n = 1 | @shlExact(run_len, 1);
                try line_buf.append(n);
            } else {
                // TODO: this is not actually compressing, is it
                for (0..run_len) |_| {
                    try line_buf.append(0);
                    try line_buf.append(color);
                }
            }
        }

        // fill in line size
        std.mem.writeInt(i16, line_buf.buffer[0..2], line_buf.len - 2, .little);

        // flush line to output stream
        try out.writeAll(line_buf.slice());
    }
}
