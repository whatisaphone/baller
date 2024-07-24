const std = @import("std");

const io = @import("io.zig");

pub fn readFileZ(
    allocator: std.mem.Allocator,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
) ![]u8 {
    const file = try dir.openFileZ(sub_path, .{});
    defer file.close();

    const stat = try file.stat();
    const buf = try allocator.alloc(u8, stat.size);
    errdefer allocator.free(buf);

    try file.reader().readNoEof(buf);
    return buf;
}

pub fn writeFileZ(dir: std.fs.Dir, sub_path: [*:0]const u8, bytes: []const u8) !void {
    const file = try dir.createFileZ(sub_path, .{});
    defer file.close();

    try file.writeAll(bytes);
}

pub fn readFileIntoZ(dir: std.fs.Dir, sub_path: [*:0]const u8, output: anytype) !void {
    const file = try dir.openFileZ(sub_path, .{});
    defer file.close();

    try io.copy(file, output);
}

pub fn makeDirIfNotExistZ(dir: std.fs.Dir, sub_path: [*:0]const u8) !void {
    dir.makeDirZ(sub_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}
