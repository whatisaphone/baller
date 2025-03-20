const std = @import("std");

const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const writeFixups = @import("block_writer.zig").writeFixups;
const xor_key = @import("build.zig").xor_key;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const parser = @import("parser.zig");
const pathf = @import("pathf.zig");

pub fn run(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    ast: *const parser.Ast,
) !void {
    try emitIndex(project_dir, output_dir, index_name, ast);

    for (0..games.numberOfDisks(game)) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try emitDisk(gpa, project_dir, output_dir, index_name, game, ast, disk_number);
    }
}

fn emitIndex(
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    ast: *const parser.Ast,
) !void {
    const project = &ast.nodes.items[ast.root].project;

    const out_file = try output_dir.createFileZ(index_name, .{});
    defer out_file.close();
    const out = io.xorWriter(out_file.writer(), xor_key);

    const in_file = try project_dir.openFile(project.index_path, .{});
    defer in_file.close();

    try io.copy(in_file, out.writer());
}

fn emitDisk(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    ast: *const parser.Ast,
    disk_number: u8,
) !void {
    const project = &ast.nodes.items[ast.root].project;
    const disks = ast.getExtra(project.disks);
    const disk_index = disk_number - 1;
    const disk_node = disks[disk_index];
    if (disk_node == parser.null_node)
        return;
    const disk = &ast.nodes.items[disk_node].disk;

    var out_name_buf: pathf.Path = .{};
    const out_name = try pathf.append(&out_name_buf, index_name);
    games.pointPathToDisk(game, out_name.full(), disk_number);

    const out_file = try output_dir.createFileZ(out_name.full(), .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    const lecf_start = try beginBlock(&out, "LECF");

    const child_nodes = ast.getExtra(disk.children);
    for (child_nodes) |child_node| {
        const child = &ast.nodes.items[child_node].raw_block;
        try emitRawBlock(project_dir, &out, &fixups, child);
    }

    try endBlock(&out, &fixups, lecf_start);

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

fn emitRawBlock(
    project_dir: std.fs.Dir,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    raw_block: *const @FieldType(parser.Node, "raw_block"),
) !void {
    const lflf_start = try beginBlock(out, "LFLF");

    const in_file = try project_dir.openFile(raw_block.path, .{});
    defer in_file.close();

    try io.copy(in_file, out.writer());

    try endBlock(out, fixups, lflf_start);
}
