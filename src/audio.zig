const std = @import("std");

const blockId = @import("block_id.zig").blockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");
const report = @import("report.zig");
const wav = @import("wav.zig");

pub const hshd_len = 0x10;

pub const Sound = struct {
    hshd: [hshd_len]u8,
    sbng: bool,
};

pub fn decode(digi_raw: []const u8, path_buf: *const pathf.PrintedPath) !Sound {
    var reader = std.io.fixedBufferStream(digi_raw);
    var blocks = fixedBlockReader(&reader);

    const hshd = try blocks.expectBlockAsValue("HSHD", [hshd_len]u8);

    var has_sbng = false;
    if (try blocks.peek() == comptime blockId("SBNG")) {
        const sbng = try blocks.expectBlockAsSlice("SBNG");

        const path = try pathf.append(path_buf.buf, "_SBNG.bin");
        defer path.restore();
        try fs.writeFileZ(std.fs.cwd(), path_buf.full(), sbng);

        has_sbng = true;
    }

    {
        const sdat_len = try blocks.expectBlock("SDAT");
        const sdat = try io.readInPlace(&reader, sdat_len);

        const path = try pathf.append(path_buf.buf, ".wav");
        defer path.restore();
        const file = try std.fs.cwd().createFileZ(path_buf.full(), .{});
        defer file.close();
        var out = std.io.bufferedWriter(file.writer());

        try wav.writeHeader(sdat_len, out.writer());
        try out.writer().writeAll(sdat);

        try out.flush();
    }

    try blocks.finishEof();

    return .{
        .hshd = hshd.*,
        .sbng = has_sbng,
    };
}

pub fn encode(
    path_buf: *pathf.Path,
    hshd: *const [hshd_len]u8,
    sbng_path_opt: ?[]const u8,
    sdat_path: []const u8,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    const hshd_fixup = try beginBlock(out, "HSHD");
    try out.writer().writeAll(hshd);
    try endBlock(out, fixups, hshd_fixup);

    if (sbng_path_opt) |sbng_path| {
        const path = try pathf.append(path_buf, sbng_path);
        defer path.restore();

        const sdat_fixup = try beginBlock(out, "SBNG");
        try fs.readFileIntoZ(std.fs.cwd(), path.full(), out.writer());
        try endBlock(out, fixups, sdat_fixup);
    }

    {
        const path = try pathf.append(path_buf, sdat_path);
        defer path.restore();

        const file = try std.fs.cwd().openFileZ(path.full(), .{});
        defer file.close();
        var wav_in = std.io.bufferedReader(file.reader());

        const header = try wav.readHeader(wav_in.reader());
        if (header.channels != 1 or
            header.samples_per_sec != 11025 or
            header.bits_per_sample != 8)
        {
            report.fatal("WAV must be 11025 KHz 8-bit mono", .{});
            return error.BadData;
        }

        const data_len = try wav.findData(wav_in.reader());

        const sdat_fixup = try beginBlock(out, "SDAT");
        try io.copy(std.io.limitedReader(wav_in.reader(), data_len), out.writer());
        try endBlock(out, fixups, sdat_fixup);
    }
}
