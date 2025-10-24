const std = @import("std");

pub const Iterator = struct {
    input: []const [:0]const u8,
    pos: usize = 0,

    pub fn init(input: []const [:0]const u8) Iterator {
        return .{ .input = input };
    }

    pub fn next(self: *Iterator) ?Arg {
        if (self.pos == self.input.len)
            return null;

        const raw = self.input[self.pos];
        self.pos += 1;

        if (raw.len < 2)
            return .{ .positional = raw };

        if (raw[0] == '-' and raw[1] != '-')
            if (raw.len == 2)
                return .{ .short_flag = raw[1] }
            else
                return .{ .short_option = .{
                    .flag = raw[1],
                    .value = raw[2..],
                } };

        if (raw[0] == '-' and raw[1] == '-')
            if (raw.len == 2)
                // TODO: handle -- properly
                return .{ .positional = raw }
            else if (std.mem.indexOfScalarPos(u8, raw, 2, '=')) |eq|
                return .{ .long_option = .{
                    .flag = raw[2..eq],
                    .value = raw[eq + 1 ..],
                } }
            else
                return .{ .long_flag = raw[2..] };

        return .{ .positional = raw };
    }
};

const Arg = union(enum) {
    short_flag: u8,
    short_option: ShortOption,
    long_flag: [:0]const u8,
    long_option: LongOption,
    positional: [:0]const u8,

    pub fn reportUnexpected(self: Arg) error{CommandLineReported} {
        return report("unexpected argument {f}\n", .{self});
    }

    pub fn reportInvalidValue(self: Arg) error{CommandLineReported} {
        return report("invalid value for {f}\n", .{self});
    }

    pub fn reportDuplicate(self: Arg) error{CommandLineReported} {
        return report("duplicate argument {f}\n", .{self});
    }

    pub fn format(self: Arg, w: *std.io.Writer) !void {
        switch (self) {
            .short_flag => |flag| try w.print("-{c}", .{flag}),
            .short_option => |opt| try w.print("-{c}", .{opt.flag}),
            .long_flag => |flag| try w.print("--{s}", .{flag}),
            .long_option => |opt| try w.print("--{s}", .{opt.flag}),
            .positional => |str| try w.print("{s}", .{str}),
        }
    }
};

const ShortOption = struct {
    flag: u8,
    value: [:0]const u8,
};

const LongOption = struct {
    flag: []const u8,
    value: [:0]const u8,
};

pub fn reportMissing(name: []const u8) error{CommandLineReported} {
    return report("missing argument {s}\n", .{name});
}

fn report(comptime fmt: []const u8, args: anytype) error{CommandLineReported} {
    var buf: [4096]u8 = undefined;
    var out = std.fs.File.stderr().writer(&buf);
    out.interface.print(fmt, args) catch {};
    out.interface.flush() catch {};
    return error.CommandLineReported;
}
