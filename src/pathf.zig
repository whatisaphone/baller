const std = @import("std");

const blockIdToStr = @import("block_id.zig").blockIdToStr;
const BlockId = @import("block_id.zig").BlockId;

const Path = std.BoundedArray(u8, 4095);

pub fn append(buf: *Path, items: []const u8) !PrintedPath {
    const prev_len = buf.len;

    try buf.appendSlice(items);

    // append a null terminator, but don't include it in the len
    try buf.append(0);
    buf.len -= 1;

    return .{ .buf = buf, .prev_len = prev_len };
}

pub fn print(buf: *Path, comptime format: []const u8, args: anytype) !PrintedPath {
    const prev_len = buf.len;

    try buf.writer().print(format, args);

    // append a null terminator, but don't include it in the len
    try buf.append(0);
    buf.len -= 1;

    return .{ .buf = buf, .prev_len = prev_len };
}

pub fn appendBlockPath(
    buf: *Path,
    block_id: BlockId,
    number: u32,
    ext: []const u8,
) !PrintedPath {
    std.debug.assert(ext.len == 3 and ext[0] != '.');

    return print(buf, "{s}_{:0>4}.{s}", .{ blockIdToStr(&block_id), number, ext });
}

pub fn popFile(buf: *Path) !void {
    const dirname = std.fs.path.dirname(buf.slice()) orelse return error.BadPathName;
    buf.len = @intCast(dirname.len + 1);
}

pub const PrintedPath = struct {
    buf: *Path,
    prev_len: u12,

    pub fn restore(self: *const PrintedPath) void {
        std.debug.assert(self.buf.len > self.prev_len);
        self.buf.len = self.prev_len;
    }

    pub fn full(self: *const PrintedPath) [:0]const u8 {
        return self.buf.buffer[0..self.buf.len :0];
    }

    pub fn relative(self: *const PrintedPath) [:0]const u8 {
        return self.buf.buffer[self.prev_len..self.buf.len :0];
    }
};
