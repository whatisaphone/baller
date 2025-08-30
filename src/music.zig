const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const BlockId = @import("block_id.zig").BlockId;
const Block = @import("block_reader.zig").Block;
const FxbclReader = @import("block_reader.zig").FxbclReader;
const StreamingBlockReader = @import("block_reader.zig").StreamingBlockReader;
const Fixup = @import("block_writer.zig").Fixup;
const writeFixups = @import("block_writer.zig").writeFixups;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlock = @import("block_writer.zig").endBlock;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const BoundedArray = @import("bounded_array.zig").BoundedArray;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const fsd = @import("fsd.zig");
const io = @import("io.zig");
const iold = @import("iold.zig");
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
    const in_file = try fsd.openFileZ(diagnostic, input_dir, input_path);
    defer in_file.close();
    const in_xor = io.xorReader(in_file.reader(), 0x00);
    var in_buf = iold.bufferedReader(in_xor.reader());
    var in_count = std.io.countingReader(in_buf.reader());
    var in = iold.limitedReader(in_count.reader(), std.math.maxInt(u32));

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

        // Basketball dumps raw wav files here and they don't fit into the usual
        // block structure, so the parsing gets a little hacky, beware.
        const block_offset, const block_header = try song_blocks.readHeader() orelse
            return error.BadData;
        switch (block_header.raw[0]) {
            BlockId.DIGI.raw() => {
                const block = song_blocks.validate(block_offset, block_header) orelse
                    return error.BadData;
                song_blocks.commit(&block);
                try extractDigi(&cx, sgen, &block);
                try song_blocks.finish(&block);
            },
            std.mem.bytesToValue(u32, "RIFF") => {
                try extractRiff(&cx, sgen, std.mem.toBytes(block_header));
            },
            else => return error.BadData,
        }
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

    var name_buf: BoundedArray(u8, Symbols.max_name_len + ".wav".len + 1) = .{};
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
    const wav_file = try fsd.createFileZ(cx.diag.diagnostic, cx.output_dir, wav_path);
    defer wav_file.close();
    var wav_out = iold.bufferedWriter(wav_file.writer());
    try sounds.writeWavHeader(wav_out.writer(), sdat_block.size);
    try io.copy(cx.in.reader(), wav_out.writer());
    try wav_out.flush();
    try digi_blocks.finish(&sdat_block);

    try cx.code.writer(cx.gpa).print("        sdat \"{s}/{s}\"\n", .{ cx.output_path, wav_path });

    try digi_blocks.end();

    try cx.code.appendSlice(cx.gpa, "    }\n");
}

fn extractRiff(cx: *const Cx, entry: *const Sgen, peeked_bytes: [8]u8) !void {
    var name_buf: BoundedArray(u8, Symbols.max_name_len + ".wav".len + 1) = .{};
    cx.symbols.writeGlobName(.sound, entry.number, name_buf.writer()) catch unreachable;
    const name = name_buf.slice();
    name_buf.appendSlice(".wav\x00") catch unreachable;
    const wav_path = name_buf.slice()[0 .. name_buf.len - 1 :0];

    try cx.code.writer(cx.gpa).print(
        "    riff {s}@{} \"{s}/{s}\"\n",
        .{ name, entry.number, cx.output_path, wav_path },
    );

    const wav_file = try fsd.createFileZ(cx.diag.diagnostic, cx.output_dir, wav_path);
    defer wav_file.close();
    try wav_file.writer().writeAll(&peeked_bytes);
    try io.copy(
        iold.limitedReader(cx.in.reader(), entry.size - peeked_bytes.len),
        wav_file.writer(),
    );
}

fn readBlockAsValue(in: *FxbclReader, block: *const Block, T: type) !T {
    if (block.size != @sizeOf(T)) return error.BadData;
    var result: T = undefined;
    try in.reader().readNoEof(std.mem.asBytes(&result));
    return result;
}

pub fn build(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
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

    const out_file = try fsd.createFileZ(diagnostic, out_dir, out_name);
    defer out_file.close();
    var out_buf = iold.bufferedWriter(out_file.writer());
    var out = iold.countingWriter(out_buf.writer());

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
    try buf.ensureTotalCapacity(gpa, 2 << 20);

    for (file.ast.getExtra(music_node.children)) |node_index| {
        const node = file.ast.nodes.at(node_index);

        const start: u32 = @intCast(out.bytes_written);

        const glob_number = glob_number: switch (node.*) {
            .sound => |*sound| {
                const fixup = try beginBlock(&out, sound.block_id);
                std.debug.assert(fixup == start);
                buf.clearRetainingCapacity();
                try sounds.build(gpa, diagnostic, .node(file, node_index), project_dir, file, sound.children, &buf);
                try out.writer().writeAll(buf.items);
                try endBlock(&out, &fixups, fixup);
                break :glob_number sound.glob_number;
            },
            .riff => |*riff| {
                try fs.readFileInto(project_dir, file.ast.strings.get(riff.path), out.writer());
                break :glob_number riff.glob_number;
            },
            else => unreachable,
        };

        const end: u32 = @intCast(out.bytes_written);
        const size = end - start;

        const sgen_start = beginBlockAl(utils.null_allocator, &sgens, .SGEN) catch unreachable;
        sgens.fixedWriter().writeAll(std.mem.asBytes(&Sgen{
            .number = glob_number,
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
