const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @This();

path: []const u8,

pub fn err(self: *const Diagnostic, offset: u32, comptime fmt: []const u8, args: anytype) void {
    const stderr = std.io.getStdErr();
    stderr.writer().print("{s}:{x:0>8}: ", .{ self.path, offset }) catch {};
    stderr.writer().print(fmt, args) catch {};
    stderr.writer().writeByte('\n') catch {};
}

pub fn trace(self: *const Diagnostic, offset: u32, comptime fmt: []const u8, args: anytype) void {
    if (builtin.mode != .Debug) return;

    const stderr = std.io.getStdErr();
    stderr.writer().print("{s}:{x:0>8}: ", .{ self.path, offset }) catch {};
    stderr.writer().print(fmt, args) catch {};
    stderr.writer().writeByte('\n') catch {};
}
