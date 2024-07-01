const std = @import("std");

const build = @import("build.zig");
const extract = @import("extract.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    if (std.os.argv.len < 1 + 1)
        return error.CommandLine;

    const command = std.mem.sliceTo(std.os.argv[1], 0);
    if (std.mem.eql(u8, command, "build")) {
        try build.runCli(allocator);
    } else if (std.mem.eql(u8, command, "extract")) {
        try extract.runCli(allocator);
    } else {
        return error.CommandLine;
    }
}

test {
    _ = @import("tests.zig");
}
