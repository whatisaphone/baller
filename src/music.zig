const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const Block = @import("block_reader.zig").Block;
const FxbclReader = @import("block_reader.zig").FxbclReader;
const StreamingBlockReader = @import("block_reader.zig").StreamingBlockReader;
const Fixup = @import("block_writer.zig").Fixup;
const writeFixups = @import("block_writer.zig").writeFixups;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlock = @import("block_writer.zig").endBlock;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const sounds = @import("sounds.zig");
const utils = @import("utils.zig");

pub fn extract(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    symbols: *const Symbols,
    input_dir: std.fs.Dir,
    input_path: [:0]const u8,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const in_file = try input_dir.openFileZ(input_path, .{});
    defer in_file.close();
    const in_xor = io.xorReader(in_file.reader(), 0x00);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in_count = std.io.countingReader(in_buf.reader());
    var in = std.io.limitedReader(in_count.reader(), std.math.maxInt(u32));

    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, input_path);

    try code.appendSlice(gpa, "\nmusic {\n");

    var file_blocks: StreamingBlockReader = .init(&in, &diag);

    const song_block = try file_blocks.expect(.SONG) orelse return error.BadData;
    var song_blocks: StreamingBlockReader = .init(&in, &diag);

    const sghd_block = try song_blocks.expect(.SGHD) orelse return error.BadData;
    const sghd = try readBlockAsValue(&in, &sghd_block, Sghd);
    try song_blocks.finish(&sghd_block);

    const sgens = try gpa.alloc(Sgen, sghd.count);
    defer gpa.free(sgens);

    for (sgens) |*sgen| {
        const sgen_block = try song_blocks.expect(.SGEN) orelse return error.BadData;
        sgen.* = try readBlockAsValue(&in, &sgen_block, Sgen);
        try song_blocks.finish(&sgen_block);
    }

    const cx: Cx = .{
        .gpa = gpa,
        .symbols = symbols,
        .in = &in,
        .diag = &diag,
        .code = code,
        .output_dir = output_dir,
        .output_path = output_path,
    };

    for (sgens) |*sgen| {
        if (in_count.bytes_read != sgen.offset) return error.BadData;
        const block = try song_blocks.next() orelse return error.BadData;
        switch (block.id) {
            .DIGI => try extractDigi(&cx, sgen, &block),
            else => return error.BadData,
        }
        try song_blocks.finish(&block);
    }

    try song_blocks.end();

    try file_blocks.finish(&song_block);
    file_blocks.expectMismatchedEnd();

    try code.appendSlice(gpa, "}\n");
}

const Cx = struct {
    gpa: std.mem.Allocator,
    symbols: *const Symbols,
    in: *FxbclReader,
    diag: *const Diagnostic.ForBinaryFile,
    code: *std.ArrayListUnmanaged(u8),
    output_dir: std.fs.Dir,
    output_path: []const u8,
};

fn extractDigi(cx: *const Cx, sgen: *const Sgen, digi_block: *const Block) !void {
    std.debug.assert(digi_block.id == .DIGI);

    if (sgen.size != digi_block.full_size()) return error.BadData;

    var name_buf: std.BoundedArray(u8, Symbols.max_name_len + ".wav".len + 1) = .{};
    cx.symbols.writeGlobName(.sound, sgen.number, name_buf.writer()) catch unreachable;
    const name = name_buf.slice();
    name_buf.appendSlice(".wav\x00") catch unreachable;
    const wav_path = name_buf.slice()[0 .. name_buf.len - 1 :0];

    try cx.code.writer(cx.gpa).print(
        "    sound {} {s}@{} {{\n",
        .{ digi_block.id, name, sgen.number },
    );

    var digi_blocks: StreamingBlockReader = .init(cx.in, cx.diag);

    const hshd_block = try digi_blocks.expect(.HSHD) orelse return error.BadData;
    const hshd = try readBlockAsValue(cx.in, &hshd_block, Hshd);
    try writeRawBlock(cx.gpa, .HSHD, &hshd, cx.output_dir, cx.output_path, 8, .{ .symbol_block = name }, cx.code);
    try digi_blocks.finish(&hshd_block);

    const sdat_block = try digi_blocks.expect(.SDAT) orelse return error.BadData;
    const wav_file = try cx.output_dir.createFileZ(wav_path, .{});
    defer wav_file.close();
    var wav_out = std.io.bufferedWriter(wav_file.writer());
    try sounds.writeWavHeader(wav_out.writer(), sdat_block.size);
    try io.copy(cx.in.reader(), wav_out.writer());
    try wav_out.flush();
    try digi_blocks.finish(&sdat_block);

    try cx.code.writer(cx.gpa).print("        sdat \"{s}/{s}\"\n", .{ cx.output_path, wav_path });

    try digi_blocks.end();

    try cx.code.appendSlice(cx.gpa, "    }\n");
}

fn readBlockAsValue(in: *FxbclReader, block: *const Block, T: type) !T {
    if (block.size != @sizeOf(T)) return error.BadData;
    var result: T = undefined;
    try in.reader().readNoEof(std.mem.asBytes(&result));
    return result;
}

pub fn build(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    project: *const Project,
    out_dir: std.fs.Dir,
    out_name: [:0]const u8,
) !void {
    const file = &project.files.items[0].?;
    const root = &file.ast.nodes.at(file.ast.root).project;
    const music_node = for (file.ast.getExtra(root.children)) |child_index| {
        const node = file.ast.nodes.at(child_index);
        if (node.* == .music) break &node.music;
    } else return;

    const out_file = try out_dir.createFileZ(out_name, .{});
    defer out_file.close();
    var out_buf = std.io.bufferedWriter(out_file.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    const song_start = try beginBlock(&out, .SONG);

    const sghd_start = try beginBlock(&out, .SGHD);
    const num_sounds = music_node.children.len;
    try out.writer().writeInt(u32, num_sounds, .little);
    try out.writer().writeAll(&@as([28]u8, @splat(0)));
    try endBlock(&out, &fixups, sghd_start);

    const sgens_start: u32 = @intCast(out.bytes_written);
    const sgens_total_size = num_sounds * (Block.header_size + @sizeOf(Sgen));
    // Write SGENs all at once at the end
    try out.writer().writeByteNTimes(undefined, sgens_total_size);

    // Buffer SGENs here until then
    var sgens: std.ArrayListUnmanaged(u8) = try .initCapacity(gpa, sgens_total_size);
    defer sgens.deinit(gpa);

    // TODO: avoid allocating, copy streams directly
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(gpa);

    for (file.ast.getExtra(music_node.children)) |sound_index| {
        const sound = &file.ast.nodes.at(sound_index).sound;
        const start = try beginBlock(&out, sound.block_id);
        buf.clearRetainingCapacity();
        try sounds.build(gpa, project_dir, file, sound.children, &buf);
        try out.writer().writeAll(buf.items);
        try endBlock(&out, &fixups, start);
        const end: u32 = @intCast(out.bytes_written);
        const size = end - start;

        const sgen_start = beginBlockAl(utils.null_allocator, &sgens, .SGEN) catch unreachable;
        sgens.fixedWriter().writeAll(std.mem.asBytes(&Sgen{
            .number = sound.glob_number,
            .offset = start,
            .size = size,
            .unk_0c = 0,
        })) catch unreachable;
        endBlockAl(&sgens, sgen_start);
    }

    try endBlock(&out, &fixups, song_start);

    try out_buf.flush();

    try writeFixups(out_file, out_file.writer(), fixups.items);

    try out_file.seekTo(sgens_start);
    std.debug.assert(sgens.items.len == sgens.capacity);
    try out_file.writeAll(sgens.items);
}

const Sghd = extern struct {
    count: u32,
    unk_04: [7]u32,
};

const Sgen = extern struct {
    number: u32 align(1),
    offset: u32 align(1),
    size: u32 align(1),
    unk_0c: u8 align(1),
};

const Hshd = [16]u8;
