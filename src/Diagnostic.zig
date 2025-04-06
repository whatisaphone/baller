const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @This();

path: []const u8,
offset: u32,

pub fn err(self: *const Diagnostic, offset: ?u32, comptime fmt: []const u8, args: anytype) void {
    print(self, offset, fmt, args);
}

pub fn warn(self: *const Diagnostic, offset: ?u32, comptime fmt: []const u8, args: anytype) void {
    print(self, offset, fmt, args);
}

pub fn trace(self: *const Diagnostic, offset: ?u32, comptime fmt: []const u8, args: anytype) void {
    if (builtin.mode != .Debug) return;
    print(self, offset, fmt, args);
}

fn print(self: *const Diagnostic, offset: ?u32, comptime fmt: []const u8, args: anytype) void {
    var buf: std.BoundedArray(u8, 256) = .{};
    buf.writer().print("[{}] ", .{std.Thread.getCurrentId()}) catch {};
    buf.writer().print("{s}:", .{self.path}) catch {};
    if (offset) |o|
        buf.writer().print("{x:0>8}:", .{self.offset + o}) catch {};
    buf.append(' ') catch {};
    buf.writer().print(fmt, args) catch {};
    buf.append('\n') catch {};

    std.io.getStdErr().writeAll(buf.slice()) catch {};
}
