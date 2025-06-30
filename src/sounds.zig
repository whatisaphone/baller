const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const BlockId = @import("block_id.zig").BlockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const io = @import("io.zig");
const encodeRawBlock = @import("plan.zig").encodeRawBlock;

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    glob_number: u16,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    const block_id: BlockId = .DIGI;

    var stream = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader(&stream, diag);

    const hshd = try blocks.expect(.HSHD).value([16]u8);
    try writeRawBlock(gpa, .HSHD, hshd, out_dir, out_path, 4, .{ .block_number_block = .{ block_id, glob_number } }, code);

    const sdat = try blocks.expect(.SDAT).bytes();

    var wav_path_buf: ["DIGI_0000.wav".len + 1]u8 = undefined;
    const wav_path = std.fmt.bufPrintZ(&wav_path_buf, "{}_{:0>4}.wav", .{ block_id, glob_number }) catch unreachable;
    try writeWav(out_dir, wav_path, sdat);

    try code.writer(gpa).print("    sdat \"{s}/{s}\"\n", .{ out_path, wav_path });

    try blocks.finish();
}

pub fn build(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    file: *const Project.SourceFile,
    children: Ast.ExtraSlice,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    for (file.ast.getExtra(children)) |node| {
        switch (file.ast.nodes.items[node]) {
            .raw_block => |n| {
                try encodeRawBlock(gpa, out, n.block_id, project_dir, file.ast.strings.get(n.path));
            },
            .sdat => |n| {
                const start = try beginBlockAl(gpa, out, .SDAT);
                try readWav(gpa, project_dir, file.ast.strings.get(n.path), out);
                try endBlockAl(out, start);
            },
            else => unreachable,
        }
    }
}

const WAVE_FORMAT_PCM = 1;

const PCMWAVEFORMAT = extern struct {
    wFormatTag: u16,
    nChannels: u16,
    nSamplesPerSec: u32,
    nAvgBytesPerSec: u32,
    nBlockAlign: u16,
    wBitsPerSample: u16,
};

fn writeWav(dir: std.fs.Dir, path: [*:0]const u8, samples: []const u8) !void {
    const file = try dir.createFileZ(path, .{});
    defer file.close();

    var buf = std.io.bufferedWriter(file.writer());

    try buf.writer().writeAll("RIFF");
    try buf.writer().writeInt(u32, @intCast(4 + 8 + 16 + 8 + samples.len), .little);
    try buf.writer().writeAll("WAVE");

    try buf.writer().writeAll("fmt ");
    try buf.writer().writeInt(u32, @sizeOf(PCMWAVEFORMAT), .little);
    try buf.writer().writeAll(std.mem.asBytes(&PCMWAVEFORMAT{
        .wFormatTag = WAVE_FORMAT_PCM,
        .nChannels = 1,
        .nSamplesPerSec = 11025,
        .nAvgBytesPerSec = 11025,
        .nBlockAlign = 1,
        .wBitsPerSample = 8,
    }));

    try buf.writer().writeAll("data");
    try buf.writer().writeInt(u32, @intCast(samples.len), .little);
    try buf.writer().writeAll(samples);

    try buf.flush();
}

fn readWav(
    gpa: std.mem.Allocator,
    dir: std.fs.Dir,
    path: []const u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    var file = try dir.openFile(path, .{});
    defer file.close();

    var in = std.io.bufferedReader(file.reader());

    if (try in.reader().readInt(u32, .little) != std.mem.bytesToValue(u32, "RIFF"))
        return error.BadData;
    _ = try in.reader().readInt(u32, .little); // XXX: file length. ideally i would handle this
    if (try in.reader().readInt(u32, .little) != std.mem.bytesToValue(u32, "WAVE"))
        return error.BadData;

    const fmt_size = try skipToChunk(&in, std.mem.bytesToValue(u32, "fmt "));
    if (fmt_size < @sizeOf(PCMWAVEFORMAT)) return error.BadData;

    var fmt: PCMWAVEFORMAT = undefined;
    try in.reader().readNoEof(std.mem.asBytes(&fmt));
    if (fmt.wFormatTag != WAVE_FORMAT_PCM) return error.BadData;
    if (fmt.nChannels != 1) return error.BadData;
    if (fmt.nSamplesPerSec != 11025) return error.BadData;
    if (fmt.wBitsPerSample != 8) return error.BadData;

    const data_size = try skipToChunk(&in, std.mem.bytesToValue(u32, "data"));
    try io.copy(std.io.limitedReader(in.reader(), data_size), out.writer(gpa));
}

const BufferedFile = std.io.BufferedReader(4096, std.fs.File.Reader);

fn skipToChunk(in: *BufferedFile, chunk_id: u32) !u32 {
    while (true) {
        const id = try in.reader().readInt(u32, .little);
        const size = try in.reader().readInt(u32, .little);
        if (id == chunk_id) return size;
        try seekByAndResetBuffer(in, size);
    }
}

fn seekByAndResetBuffer(in: *BufferedFile, offset: i64) !void {
    const rewind: u32 = @intCast(in.end - in.start);
    try in.unbuffered_reader.context.seekBy(offset - rewind);
    in.end = in.start;
}
