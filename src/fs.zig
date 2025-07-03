const builtin = @import("builtin");
const std = @import("std");

const io = @import("io.zig");

pub fn readFile(
    allocator: std.mem.Allocator,
    dir: std.fs.Dir,
    sub_path: []const u8,
) ![]u8 {
    const file = try dir.openFile(sub_path, .{});
    defer file.close();

    const stat = try file.stat();
    const buf = try allocator.alloc(u8, stat.size);
    errdefer allocator.free(buf);

    try file.reader().readNoEof(buf);
    return buf;
}

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

pub fn readFileInto(dir: std.fs.Dir, sub_path: []const u8, output: anytype) !void {
    const file = try dir.openFile(sub_path, .{});
    defer file.close();

    try io.copy(file, output);
}

pub fn readFileIntoZ(dir: std.fs.Dir, sub_path: [*:0]const u8, output: anytype) !void {
    const file = try dir.openFileZ(sub_path, .{});
    defer file.close();

    try io.copy(file, output);
}

pub fn makeDirIfNotExist(dir: std.fs.Dir, sub_path: []const u8) !void {
    dir.makeDir(sub_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

pub fn makeDirIfNotExistZ(dir: std.fs.Dir, sub_path: [*:0]const u8) !void {
    dir.makeDirZ(sub_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

pub fn splitPathZ(path: [:0]const u8) struct { ?[]const u8, [:0]const u8 } {
    const dir = std.fs.path.dirname(path) orelse
        return .{ null, path };
    const name = std.fs.path.basename(path);
    const name_offset = name.ptr - path.ptr;
    return .{ dir, path[name_offset..] };
}

pub fn assertNotExists(dir: std.fs.Dir, sub_path: []const u8) void {
    std.debug.assert(builtin.is_test);
    const ok = if (dir.statFile(sub_path)) |_| false else |err| err == error.FileNotFound;
    std.debug.assert(ok);
}
