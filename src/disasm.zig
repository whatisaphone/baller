const std = @import("std");

const Language = struct {
    // TODO: don't hardcode maximum
    /// 0 to 255 are normal opcodes. The rest are dynamically-assigned
    /// 256-element chunks for two-byte opcodes.
    opcodes: [256 * 16]Opcode = .{.unknown} ** (256 * 16),
    num_nested: u8 = 0,

    fn add(self: *Language, byte: u8, name: []const u8, args: []const Arg) void {
        if (self.opcodes[byte] != .unknown)
            unreachable;

        self.opcodes[byte] = .{ .ins = .{
            .name = name,
            .args = std.BoundedArray(Arg, 2).fromSlice(args) catch unreachable,
        } };
    }

    fn addNested(
        self: *Language,
        byte1: u8,
        byte2: u8,
        name: []const u8,
        args: []const Arg,
    ) void {
        const n = switch (self.opcodes[byte1]) {
            .unknown => n: {
                self.num_nested += 1;
                self.opcodes[byte1] = .{ .nested = self.num_nested };
                break :n self.num_nested;
            },
            .nested => |n| n,
            else => unreachable,
        };

        self.opcodes[n << 8 | byte2] = .{ .ins = .{
            .name = name,
            .args = std.BoundedArray(Arg, 2).fromSlice(args) catch unreachable,
        } };
    }
};

const Opcode = union(enum) {
    unknown,
    ins: Ins,
    nested: u16,
};

const Ins = struct {
    name: []const u8,
    args: std.BoundedArray(Arg, 2),
};

const Arg = enum {
    u8,
    i16,
    i32,
    variable,
    string,
};

const Variable = struct {
    raw: u16,

    const Decoded = union(enum) {
        global: u16,
        local: u16,
        room: u16,
    };

    fn decode(self: Variable) !Decoded {
        return switch (self.raw & 0xc000) {
            0x0000 => .{ .global = self.raw & 0x3fff },
            0x4000 => .{ .local = self.raw & 0x3fff },
            0x8000 => .{ .room = self.raw & 0x3fff },
            0xc000 => error.BadData,
            else => unreachable,
        };
    }
};

fn buildLanguage() Language {
    var lang = Language{};

    lang.add(0x00, "push", &.{.u8});
    lang.add(0x01, "push", &.{.i16});
    lang.add(0x02, "push", &.{.i32});
    lang.add(0x03, "push", &.{.variable});
    lang.add(0x04, "push", &.{.string});
    lang.add(0x07, "get-array-item", &.{.variable});
    lang.add(0x0c, "dup", &.{});
    lang.add(0x0d, "not", &.{});
    lang.add(0x0e, "compare-equal", &.{});
    lang.add(0x0f, "compare-not-equal", &.{});
    lang.add(0x10, "compare-greater", &.{});
    lang.add(0x11, "compare-less", &.{});
    lang.add(0x12, "compare-less-or-equal", &.{});
    lang.add(0x14, "add", &.{});
    lang.add(0x15, "sub", &.{});
    lang.add(0x19, "logical-or", &.{});
    lang.add(0x1a, "pop", &.{});
    lang.add(0x1b, "in-list", &.{});

    lang.addNested(0x26, 0x39, "sprite-select-range", &.{});
    lang.addNested(0x26, 0x7d, "sprite-class", &.{});
    lang.addNested(0x26, 0x9e, "sprite-restart", &.{});

    lang.add(0x37, "dim-array", &.{ .u8, .variable });
    lang.add(0x43, "assign", &.{.variable});
    lang.add(0x47, "set-array-item", &.{.variable});
    lang.add(0x4f, "inc", &.{.variable});
    lang.add(0x5c, "jump-if", &.{.i16});
    lang.add(0x5d, "jump-unless", &.{.i16});

    lang.addNested(0x5e, 0x01, "start-script", &.{});

    lang.add(0x66, "end", &.{});

    lang.addNested(0x6b, 0x91, "cursor-off", &.{});
    lang.addNested(0x6b, 0x93, "userput-off", &.{});
    lang.addNested(0x6b, 0x9c, "charset", &.{});

    lang.add(0x6c, "break", &.{});
    lang.add(0x73, "jump", &.{.i16});
    lang.add(0x7b, "current-room", &.{});
    lang.add(0x87, "random", &.{});

    lang.addNested(0x9b, 0x64, "load-script", &.{});
    lang.addNested(0x9b, 0x6c, "lock-script", &.{});
    lang.addNested(0x9b, 0x75, "load-charset", &.{});

    lang.addNested(0x9c, 0xb5, "fades", &.{});

    lang.addNested(0xa4, 0x07, "assign-string", &.{.variable});
    lang.addNested(0xa4, 0xc2, "sprintf", &.{.variable});

    lang.addNested(0xb6, 0x4b, "print-debug-string", &.{.string});
    lang.addNested(0xb6, 0xfe, "print-debug-start", &.{});

    lang.addNested(0xbc, 0xcc, "undim", &.{.variable});

    lang.add(0xd0, "get-time-date", &.{});

    lang.addNested(0xf3, 0x06, "read-ini-int", &.{});
    lang.addNested(0xf3, 0x07, "read-ini-string", &.{});

    lang.addNested(0xfa, 0xf3, "title-bar", &.{});

    return lang;
}

pub fn disasm(bytecode: []const u8, out: anytype) !void {
    const lang = buildLanguage(); // TODO: cache this

    var reader = std.io.fixedBufferStream(bytecode);

    while (reader.pos < bytecode.len) {
        const b1 = try reader.reader().readByte();
        switch (lang.opcodes[b1]) {
            .unknown => try flushUnknownBytes(&reader, out, 1),
            .ins => |*ins| try disasmIns(ins, &reader, out),
            .nested => |n| {
                const b2 = try reader.reader().readByte();
                switch (lang.opcodes[n << 8 | b2]) {
                    .unknown => try flushUnknownBytes(&reader, out, 2),
                    .ins => |*ins| try disasmIns(ins, &reader, out),
                    .nested => unreachable,
                }
            },
        }
    }
}

fn flushUnknownBytes(reader: anytype, out: anytype, leading: u8) !void {
    reader.pos -= leading;
    while (reader.pos < reader.buffer.len) {
        const b = try reader.reader().readByte();
        try out.print(".db 0x{x:0>2}\n", .{b});
    }
}

fn disasmIns(ins: *const Ins, reader: anytype, out: anytype) !void {
    try out.writeAll(ins.name);
    for (ins.args.slice()) |*arg| {
        try out.writeByte(' ');
        try disasmArg(arg, reader, out);
    }
    try out.writeByte('\n');
}

fn disasmArg(arg: *const Arg, reader: anytype, out: anytype) !void {
    switch (arg.*) {
        .u8 => {
            const n = try reader.reader().readInt(u8, .little);
            try out.print("{}", .{n});
        },
        .i16 => {
            const n = try reader.reader().readInt(i16, .little);
            try out.print("{}", .{n});
        },
        .i32 => {
            const n = try reader.reader().readInt(i32, .little);
            try out.print("{}", .{n});
        },
        .variable => {
            const variable = try readVariable(reader);
            try emitVariable(out, variable);
        },
        .string => {
            // TODO: escaping
            try out.writeByte('"');
            try out.writeAll(try readString(reader));
            try out.writeByte('"');
        },
    }
}

fn readVariable(reader: anytype) !Variable {
    const raw = try reader.reader().readInt(u16, .little);
    return .{ .raw = raw };
}

fn emitVariable(out: anytype, variable: Variable) !void {
    switch (try variable.decode()) {
        .global => |num| try out.print("global{}", .{num}),
        .local => |num| try out.print("local{}", .{num}),
        .room => |num| try out.print("room{}", .{num}),
    }
}

fn readString(reader: anytype) ![]const u8 {
    const start = reader.pos;
    const null_pos = std.mem.indexOfScalarPos(u8, reader.buffer, start, 0) orelse
        return error.BadData;
    reader.pos = null_pos + 1;
    return reader.buffer[start..null_pos];
}
