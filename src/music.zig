const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const BlockId = @import("block_id.zig").BlockId;
const Block = @import("block_reader.zig").Block;
const StreamingBlockReader = @import("block_reader.zig").StreamingBlockReader;
const fxbcl = @import("block_reader.zig").fxbcl;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlock = @import("block_writer.zig").endBlock;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const fxbc = @import("block_writer.zig").fxbc;
const writeFixups = @import("block_writer.zig").writeFixups;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const fsd = @import("fsd.zig");
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
    code: *std.ArrayList(u8),
) !void {
    const in_file = try fsd.openFileZ(diagnostic, input_dir, input_path);
    defer in_file.close();
    var in_buf: [4096]u8 = undefined;
    var in_raw = in_file.reader(&in_buf);
    var in_xor: io.XorReader = .init(&in_raw.interface, 0x00, &.{});
    var in_limit: std.io.Reader.Limited = .init(&in_xor.interface, .unlimited, &.{});
    const in = &in_limit.interface;

    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, input_path);

    try code.appendSlice(gpa, "\nmusic {\n");

    var file_blocks: StreamingBlockReader = .init(in, &diag);

    const song_block = try file_blocks.expect(.SONG) orelse return error.BadData;
    var song_blocks: StreamingBlockReader = .init(in, &diag);

    const sghd_block = try song_blocks.expect(.SGHD) orelse return error.BadData;
    const sghd = try readBlockAsValue(in, &sghd_block, Sghd);
    try song_blocks.finish(&sghd_block);

    const sgens = try gpa.alloc(Sgen, sghd.count);
    defer gpa.free(sgens);

    for (sgens) |*sgen| {
        const sgen_block = try song_blocks.expect(.SGEN) orelse return error.BadData;
        sgen.* = try readBlockAsValue(in, &sgen_block, Sgen);
        try song_blocks.finish(&sgen_block);
    }

    const cx: Cx = .{
        .gpa = gpa,
        .symbols = symbols,
        .in = in,
        .diag = &diag,
        .code = code,
        .output_dir = output_dir,
        .output_path = output_path,
    };

    for (sgens) |*sgen| {
        if (fxbcl.pos(in) != sgen.offset) return error.BadData;

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
                // Normally the block reader sets this limit, but RIFF isn't a
                // block, so we have to do it manually instead.
                if (sgen.size < Block.header_size) return error.BadData;
                const riff_size = sgen.size - Block.header_size;

                const parent_remaining = fxbcl.remaining(cx.in);
                fxbcl.setRemaining(cx.in, riff_size);

                try extractRiff(&cx, sgen, std.mem.toBytes(block_header));

                if (fxbcl.remaining(cx.in) != 0) return error.BadData;
                fxbcl.setRemaining(cx.in, parent_remaining - riff_size);
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
    in: *std.io.Reader,
    diag: *const Diagnostic.ForBinaryFile,
    code: *std.ArrayList(u8),
    output_dir: std.fs.Dir,
    output_path: []const u8,
};

fn extractDigi(cx: *const Cx, sgen: *const Sgen, digi_block: *const Block) !void {
    std.debug.assert(digi_block.id == .DIGI);

    if (sgen.size != digi_block.full_size()) return error.BadData;

    var name_buf: utils.TinyArray(u8, Symbols.max_name_len + ".wav".len + 1) = .empty;
    name_buf.printAssumeCapacity("{f}", .{cx.symbols.fmtGlobName(.sound, sgen.number)});
    const name = name_buf.slice();
    name_buf.appendSlice(".wav\x00") catch unreachable;
    const wav_path = name_buf.slice()[0 .. name_buf.len - 1 :0];

    try cx.code.print(
        cx.gpa,
        "    sound {f} {s}@{} {{\n",
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
    var wav_buf: [4096]u8 = undefined;
    var wav_out = wav_file.writer(&wav_buf);
    try sounds.writeWavHeader(&wav_out.interface, sdat_block.size);
    try io.copy(cx.in, &wav_out.interface);
    try wav_out.interface.flush();
    try digi_blocks.finish(&sdat_block);

    try cx.code.print(cx.gpa, "        sdat \"{s}/{s}\"\n", .{ cx.output_path, wav_path });

    try digi_blocks.end();

    try cx.code.appendSlice(cx.gpa, "    }\n");
}

fn extractRiff(cx: *const Cx, entry: *const Sgen, peeked_bytes: [8]u8) !void {
    var name_buf: utils.TinyArray(u8, Symbols.max_name_len + ".wav".len + 1) = .empty;
    name_buf.printAssumeCapacity("{f}", .{cx.symbols.fmtGlobName(.sound, entry.number)});
    const name = name_buf.slice();
    name_buf.appendSlice(".wav\x00") catch unreachable;
    const wav_path = name_buf.slice()[0 .. name_buf.len - 1 :0];

    try cx.code.print(
        cx.gpa,
        "    riff {s}@{} \"{s}/{s}\"\n",
        .{ name, entry.number, cx.output_path, wav_path },
    );

    const wav_file = try fsd.createFileZ(cx.diag.diagnostic, cx.output_dir, wav_path);
    defer wav_file.close();
    var wav_writer = wav_file.writer(&.{});
    try wav_writer.interface.writeAll(&peeked_bytes);
    try io.copy(cx.in, &wav_writer.interface);
}

fn readBlockAsValue(in: *std.io.Reader, block: *const Block, T: type) !T {
    if (block.size != @sizeOf(T)) return error.BadData;
    var result: T = undefined;
    try in.readSliceAll(std.mem.asBytes(&result));
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
    var file_buf: [4096]u8 = undefined;
    var file_writer = out_file.writer(&file_buf);
    var xor_writer: io.XorWriter = .init(&file_writer.interface, 0x00, &.{});
    const out = &xor_writer.interface;

    var fixups: std.ArrayList(Fixup) = .empty;
    defer fixups.deinit(gpa);

    const song_start = try beginBlock(out, .SONG);

    const sghd_start = try beginBlock(out, .SGHD);
    const num_sounds = music_node.children.len;
    try out.writeInt(u32, num_sounds, .little);
    try out.writeAll(&@as([28]u8, @splat(0)));
    try endBlock(gpa, out, &fixups, sghd_start);

    const sgens_start = fxbc.pos(out);
    const sgens_total_size = num_sounds * (Block.header_size + @sizeOf(Sgen));
    // Write SGENs all at once at the end
    try fxbc.skip(out, sgens_total_size);

    // Buffer SGENs here until then
    var sgens: std.ArrayList(u8) = try .initCapacity(gpa, sgens_total_size);
    defer sgens.deinit(gpa);

    // TODO: avoid allocating, copy streams directly
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(gpa);
    try buf.ensureTotalCapacity(gpa, 2 << 20);

    for (file.ast.getExtra(music_node.children)) |node_index| {
        const node = file.ast.nodes.at(node_index);

        const start = fxbc.pos(out);

        const glob_number = glob_number: switch (node.*) {
            .sound => |*sound| {
                const fixup = try beginBlock(out, sound.block_id);
                std.debug.assert(fixup == start);
                buf.clearRetainingCapacity();
                try sounds.build(gpa, diagnostic, .node(file, node_index), project_dir, file, sound.children, &buf);
                try out.writeAll(buf.items);
                try endBlock(gpa, out, &fixups, fixup);
                break :glob_number sound.glob_number;
            },
            .riff => |*riff| {
                try fs.readFileInto(project_dir, file.ast.strings.get(riff.path), out);
                break :glob_number riff.glob_number;
            },
            else => unreachable,
        };

        const end = fxbc.pos(out);
        const size = end - start;

        const sgen_start = beginBlockAl(utils.null_allocator, &sgens, .SGEN) catch unreachable;
        sgens.appendSliceAssumeCapacity(std.mem.asBytes(&Sgen{
            .number = glob_number,
            .offset = start,
            .size = size,
            .unk_0c = 0,
        }));
        endBlockAl(&sgens, sgen_start);
    }

    try endBlock(gpa, out, &fixups, song_start);

    try file_writer.interface.flush();

    try writeFixups(&file_writer, out, fixups.items);

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
