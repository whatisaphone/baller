const std = @import("std");

const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const io = @import("io.zig");
const report = @import("report.zig");
const wav = @import("wav.zig");

const expected_hshd_len = 0x10;
const expected_hshd = &.{
    0,    0,    0x80, 0x80,
    0xff, 0xff, 0x11, 0x2b,
    0,    0,    0,    0,
    0,    0,    0,    0,
};

pub fn decode(digi_raw: []const u8, out: anytype) !void {
    var reader = std.io.fixedBufferStream(digi_raw);
    var blocks = fixedBlockReader(&reader);

    const hshd_len = try blocks.expectBlock("HSHD");
    if (hshd_len != expected_hshd_len)
        return error.BadData;
    const hshd = try io.readInPlace(&reader, expected_hshd_len);
    if (!std.mem.eql(u8, hshd, expected_hshd))
        return error.BadData;

    const sdat_len = try blocks.expectBlock("SDAT");
    const sdat = try io.readInPlace(&reader, sdat_len);

    try wav.writeHeader(sdat_len, out);
    try out.writeAll(sdat);

    try blocks.finishEof();
}

pub fn encode(wav_in: anytype, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    const header = try wav.readHeader(wav_in);

    if (header.channels != 1 or
        header.samples_per_sec != 11025 or
        header.bits_per_sample != 8)
    {
        report.fatal("WAV must be 11025 KHz 8-bit mono", .{});
        return error.BadData;
    }

    const data_len = try wav.findData(wav_in);

    const hshd_fixup = try beginBlock(out, "HSHD");
    try out.writer().writeAll(expected_hshd);
    try endBlock(out, fixups, hshd_fixup);

    const sdat_fixup = try beginBlock(out, "SDAT");
    try io.copy(std.io.limitedReader(wav_in, data_len), out.writer());
    try endBlock(out, fixups, sdat_fixup);
}
