const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const writeRawBlock = @import("extract.zig").writeRawBlock;
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
    var stream = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader(&stream, diag);

    const hshd = try blocks.expect(.HSHD).value([16]u8);
    try writeRawBlock(gpa, .HSHD, hshd, out_dir, out_path, 4, .{ .block_number_block = .{ .DIGI, glob_number } }, code);

    const sdat = try blocks.expect(.SDAT).bytes();
    try writeRawBlock(gpa, .SDAT, sdat, out_dir, out_path, 4, .{ .block_number_block = .{ .DIGI, glob_number } }, code);

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
            else => unreachable,
        }
    }
}
