const std = @import("std");

const Path = std.BoundedArray(u8, 4095);

pub fn print(buf: *Path, comptime format: []const u8, args: anytype) !PrintedPath {
    const prev_len = buf.len;

    try buf.writer().print(format, args);

    // append a null terminator, but don't include it in the len
    try buf.append(0);
    buf.len -= 1;

    return .{ .buf = buf, .prev_len = prev_len };
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
