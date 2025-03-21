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

pub fn readFileIntoSliceZ(
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
    buf: []u8,
) !void {
    const file = try dir.openFileZ(sub_path, .{});
    defer file.close();

    const stat = try file.stat();
    if (stat.size < buf.len)
        return error.EndOfStream;
    if (stat.size > buf.len)
        return error.StreamTooLong;

    try file.reader().readNoEof(buf);
}

pub fn readFileZIntoBoundedArray(
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
    bounded_array: anytype,
) !void {
    comptime std.debug.assert(std.mem.startsWith(
        u8,
        @typeName(@TypeOf(bounded_array)),
        "*bounded_array.BoundedArrayAligned(u8,",
    ));

    const file = try dir.openFileZ(sub_path, .{});
    defer file.close();

    const buf = bounded_array.unusedCapacitySlice();

    const stat = try file.stat();
    if (stat.size > buf.len)
        return error.StreamTooLong;

    try file.reader().readNoEof(buf[0..stat.size]);
    bounded_array.len = @intCast(bounded_array.len + stat.size);
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

pub fn makeParentDirIfNotExist(dir: std.fs.Dir, sub_path: []u8) !void {
    const dirname = std.fs.path.dirname(sub_path) orelse return error.BadPathName;
    const slash_index = dirname.len;

    // Borrow the slash character temporarily to get a null-terminated dirname.
    const old_slash = sub_path[slash_index];
    sub_path[slash_index] = 0;
    defer sub_path[slash_index] = old_slash;

    try makeDirIfNotExistZ(dir, sub_path[0..slash_index :0]);
}

pub fn splitPathZ(path: [:0]const u8) struct { ?[]const u8, [:0]const u8 } {
    const dir = std.fs.path.dirname(path) orelse
        return .{ null, path };
    const name = std.fs.path.basename(path);
    const name_offset = name.ptr - path.ptr;
    return .{ dir, path[name_offset..] };
}
