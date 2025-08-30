const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fsd = @import("fsd.zig");
const io = @import("io.zig");
const iold = @import("iold.zig");
const encodeRawBlock = @import("plan.zig").encodeRawBlock;

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    name: []const u8,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    var stream = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader(&stream, diag);

    const hshd = try blocks.expect(.HSHD).value([16]u8);
    try writeRawBlock(gpa, .HSHD, hshd, out_dir, out_path, 4, .{ .symbol_block = name }, code);

    while (try blocks.peek() != .SDAT) {
        const block = try blocks.next().block();
        const bytes = try io.readInPlace(&stream, block.size);
        try writeRawBlock(gpa, block.id, bytes, out_dir, out_path, 4, .{ .symbol_block = name }, code);
    }

    const sdat = try blocks.assume(.SDAT).bytes();

    var wav_path_buf: [Symbols.max_name_len + ".wav".len + 1]u8 = undefined;
    const wav_path = std.fmt.bufPrintZ(&wav_path_buf, "{s}.wav", .{name}) catch unreachable;
    try writeWav(diag.diagnostic, out_dir, wav_path, sdat);

    try code.writer(gpa).print("    sdat \"{s}/{s}\"\n", .{ out_path, wav_path });

    try blocks.finish();
}

pub fn build(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    project_dir: std.fs.Dir,
    file: *const Project.SourceFile,
    children: Ast.ExtraSlice,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    for (file.ast.getExtra(children)) |node| {
        switch (file.ast.nodes.at(node).*) {
            .raw_block => |*n| {
                try encodeRawBlock(gpa, project_dir, file, n, out);
            },
            .sdat => |*n| {
                const start = try beginBlockAl(gpa, out, .SDAT);
                try readWav(gpa, diagnostic, loc, project_dir, file.ast.strings.get(n.path), out);
                endBlockAl(out, start);
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

fn writeWav(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    path: [*:0]const u8,
    samples: []const u8,
) !void {
    const file = try fsd.createFileZ(diagnostic, dir, path);
    defer file.close();

    var buf = iold.bufferedWriter(file.writer());

    try writeWavHeader(buf.writer(), @intCast(samples.len));
    try buf.writer().writeAll(samples);

    try buf.flush();
}

pub fn writeWavHeader(out: anytype, num_samples: u32) !void {
    try out.writeAll("RIFF");
    try out.writeInt(u32, @intCast(4 + 8 + 16 + 8 + num_samples), .little);
    try out.writeAll("WAVE");

    try out.writeAll("fmt ");
    try out.writeInt(u32, @sizeOf(PCMWAVEFORMAT), .little);
    try out.writeAll(std.mem.asBytes(&PCMWAVEFORMAT{
        .wFormatTag = WAVE_FORMAT_PCM,
        .nChannels = 1,
        .nSamplesPerSec = 11025,
        .nAvgBytesPerSec = 11025,
        .nBlockAlign = 1,
        .wBitsPerSample = 8,
    }));

    try out.writeAll("data");
    try out.writeInt(u32, num_samples, .little);
}

fn readWav(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    dir: std.fs.Dir,
    path: []const u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    var file = try fsd.openFile(diagnostic, loc, dir, path);
    defer file.close();

    var in = iold.bufferedReader(file.reader());

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
    try out.ensureUnusedCapacity(gpa, data_size);
    try io.copy(std.io.limitedReader(in.reader(), data_size), out.writer(gpa));
}

const BufferedFile = iold.BufferedReader(4096, std.fs.File.Reader);

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
