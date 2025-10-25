const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const FixedBlockReader = @import("block_reader.zig").FixedBlockReader;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fsd = @import("fsd.zig");
const io = @import("io.zig");
const encodeRawBlock = @import("plan.zig").encodeRawBlock;

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    name: []const u8,
    raw: []const u8,
    code: *std.ArrayList(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    var stream: std.io.Reader = .fixed(raw);
    var blocks: FixedBlockReader = .init(&stream, diag);

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

    try code.print(gpa, "    sdat \"{s}/{s}\"\n", .{ out_path, wav_path });

    try blocks.finish();
}

pub fn build(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    project_dir: std.fs.Dir,
    file: *const Project.SourceFile,
    children: Ast.ExtraSlice,
    out: *std.ArrayList(u8),
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
    var buf: [4096]u8 = undefined;
    var writer = file.writer(&buf);

    try writeWavHeader(&writer.interface, @intCast(samples.len));
    try writer.interface.writeAll(samples);

    try writer.interface.flush();
}

pub fn writeWavHeader(out: *std.io.Writer, num_samples: u32) !void {
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
    out: *std.ArrayList(u8),
) !void {
    var file = try fsd.openFile(diagnostic, loc, dir, path);
    defer file.close();
    var buf: [4096]u8 = undefined;
    var in = file.reader(&buf);

    if (try in.interface.takeInt(u32, .little) != std.mem.bytesToValue(u32, "RIFF"))
        return error.BadData;
    _ = try in.interface.takeInt(u32, .little); // XXX: file length. ideally i would handle this
    if (try in.interface.takeInt(u32, .little) != std.mem.bytesToValue(u32, "WAVE"))
        return error.BadData;

    const fmt_size = try skipToChunk(&in, std.mem.bytesToValue(u32, "fmt "));
    if (fmt_size < @sizeOf(PCMWAVEFORMAT)) return error.BadData;

    const fmt = try in.interface.takeStructPointer(PCMWAVEFORMAT);
    if (fmt.wFormatTag != WAVE_FORMAT_PCM) return error.BadData;
    if (fmt.nChannels != 1) return error.BadData;
    if (fmt.nSamplesPerSec != 11025) return error.BadData;
    if (fmt.wBitsPerSample != 8) return error.BadData;

    const data_size = try skipToChunk(&in, std.mem.bytesToValue(u32, "data"));
    try out.ensureUnusedCapacity(gpa, data_size);
    try in.interface.readSliceAll(out.unusedCapacitySlice()[0..data_size]);
    out.items.len += data_size;
}

fn skipToChunk(in: *std.fs.File.Reader, chunk_id: u32) !u32 {
    while (true) {
        const id = try in.interface.takeInt(u32, .little);
        const size = try in.interface.takeInt(u32, .little);
        if (id == chunk_id) return size;
        try in.seekBy(size);
    }
}
