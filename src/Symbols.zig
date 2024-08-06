const std = @import("std");

const utils = @import("utils.zig");

const Symbols = @This();

/// Lookup table from number to name
globals: std.ArrayListUnmanaged(?[]const u8) = .{},
/// Map from name to number
global_names: std.StringArrayHashMapUnmanaged(u16) = .{},

pub fn deinit(self: *Symbols, allocator: std.mem.Allocator) void {
    self.global_names.deinit(allocator);
    self.globals.deinit(allocator);
}

pub fn parse(allocator: std.mem.Allocator, ini_text: []const u8) !Symbols {
    var result = Symbols{};
    errdefer result.deinit(allocator);

    var line_number: u32 = 0;
    var lines = std.mem.splitScalar(u8, ini_text, '\n');
    while (lines.next()) |line| {
        line_number += 1;
        parseLine(allocator, line, &result) catch {
            try std.io.getStdErr().writer().print(
                "error on line {}\n",
                .{line_number},
            );
            return error.Reported;
        };
    }
    return result;
}

fn parseLine(allocator: std.mem.Allocator, line: []const u8, result: *Symbols) !void {
    if (line.len == 0) // skip empty lines
        return;
    if (line[0] == ';') // skip comments
        return;

    const eq = std.mem.indexOfScalar(u8, line, '=') orelse return error.BadData;
    const key = std.mem.trim(u8, line[0..eq], " ");
    const value = std.mem.trim(u8, line[eq + 1 ..], " ");

    if (std.mem.startsWith(u8, key, "global.")) {
        const number = try std.fmt.parseInt(u16, key[7..], 10);

        try utils.growArrayList(?[]const u8, &result.globals, allocator, number + 1, null);
        if (result.globals.items[number] != null)
            return error.BadData;
        result.globals.items[number] = value;

        const entry = try result.global_names.getOrPut(allocator, value);
        if (entry.found_existing)
            return error.BadData;
        entry.value_ptr.* = number;
    } else {
        return error.BadData;
    }
}
