const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @This();

path: []const u8,
offset: u32,

pub fn err(self: *const Diagnostic, offset: u32, comptime fmt: []const u8, args: anytype) void {
    print(self, offset, fmt, args);
}

pub fn warn(self: *const Diagnostic, offset: u32, comptime fmt: []const u8, args: anytype) void {
    print(self, offset, fmt, args);
}

pub fn trace(self: *const Diagnostic, offset: u32, comptime fmt: []const u8, args: anytype) void {
    if (builtin.mode != .Debug) return;
    print(self, offset, fmt, args);
}

fn print(self: *const Diagnostic, offset: u32, comptime fmt: []const u8, args: anytype) void {
    const stderr = std.io.getStdErr();
    stderr.writer().print("{s}:{x:0>8}: ", .{ self.path, self.offset + offset }) catch {};
    stderr.writer().print(fmt, args) catch {};
    stderr.writer().writeByte('\n') catch {};
}
