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
        self.reportUnexpectedInner() catch {};
        return error.CommandLineReported;
    }

    fn reportUnexpectedInner(self: Arg) !void {
        const out = std.io.getStdErr().writer();
        try out.writeAll("unexpected argument ");
        switch (self) {
            .short_flag => |flag| try out.print("-{c}", .{flag}),
            .short_option => |opt| try out.print("-{c}", .{opt.flag}),
            .long_flag => |flag| try out.print("--{s}", .{flag}),
            .long_option => |opt| try out.print("--{s}", .{opt.flag}),
            .positional => |str| try out.print("{s}", .{str}),
        }
        try out.writeByte('\n');
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
    const out = std.io.getStdErr().writer();
    out.print("missing argument {s}\n", .{name}) catch {};
    return error.CommandLineReported;
}
