const std = @import("std");

const Game = @import("games.zig").Game;
const langdef = @import("langdef.zig");
const utils = @import("utils.zig");

pub const LangOperand = langdef.Operand;
pub const Op = langdef.Op;
pub const max_operands = langdef.max_operands;

pub const Vm = struct {
    const op_count = @typeInfo(Op).@"enum".fields.len;

    opcodes: [op_count]utils.TinyArray(u8, 3),
    operands: [op_count]utils.TinyArray(LangOperand, max_operands),
    /// Mapping from one or more opcode bytes, to `Op`s, stored as a flat array
    /// of `Entry`s.
    opcode_lookup: [256 * 53]OpcodeEntry,
};

const OpcodeEntry = struct {
    raw: u16,

    const Decoded = union(enum) {
        op: Op,
        /// Points to the starting offset of 256 subentries.
        nested: u16,
        unset,
    };

    fn encode(entry: Decoded) OpcodeEntry {
        const raw = switch (entry) {
            .op => |op| @intFromEnum(op),
            .nested => |i| 0x8000 | i,
            .unset => 0xffff,
        };
        return .{ .raw = raw };
    }

    fn decode(self: OpcodeEntry) Decoded {
        if (self.raw & 0x8000 == 0)
            return .{ .op = @enumFromInt(self.raw & 0x7fff) };
        if (self.raw == 0xffff)
            return .unset;
        return .{ .nested = self.raw & 0x7fff };
    }
};

const VmBuilder = struct {
    vm: Vm,
    opcode_lookup_pos: u16,

    fn init() VmBuilder {
        return .{
            .vm = .{
                .opcodes = @splat(.empty),
                .operands = undefined,
                .opcode_lookup = @splat(.encode(.unset)),
            },
            .opcode_lookup_pos = 256,
        };
    }

    fn add(
        self: *VmBuilder,
        bytes: utils.TinyArray(u8, 3),
        op: Op,
        operands: utils.TinyArray(LangOperand, max_operands),
    ) void {
        std.debug.assert(self.vm.opcodes[@intFromEnum(op)].len == 0);
        self.vm.opcodes[@intFromEnum(op)] = bytes;
        self.vm.operands[@intFromEnum(op)] = operands;

        var start: u16 = 0;
        for (bytes.slice()[0 .. bytes.len - 1]) |byte| {
            const index = start + byte;
            switch (self.vm.opcode_lookup[index].decode()) {
                .op => unreachable,
                .nested => {},
                .unset => self.makeNested(index),
            }
            start = self.vm.opcode_lookup[index].decode().nested;
        }
        const index = start + bytes.get(bytes.len - 1);
        std.debug.assert(self.vm.opcode_lookup[index].decode() == .unset);
        self.vm.opcode_lookup[index] = .encode(.{ .op = op });
    }

    fn makeNested(self: *VmBuilder, index: u16) void {
        const pos = self.opcode_lookup_pos;

        std.debug.assert(self.vm.opcode_lookup[index].decode() == .unset);
        self.vm.opcode_lookup[index] = .encode(.{ .nested = pos });

        for (self.vm.opcode_lookup[pos..][0..256]) |e|
            std.debug.assert(e.decode() == .unset);

        self.opcode_lookup_pos += 256;
    }
};

pub fn buildVm(game: Game) Vm {
    var b: VmBuilder = .init();
    const target = game.target();
    for (langdef.inss) |ins| {
        if (!(target.ge(ins.target_min) and target.le(ins.target_max))) continue;
        b.add(ins.opcode, ins.op, ins.operands);
    }
    return b.vm;
}

pub const LangIns = struct {
    op: Op,
    opcode: utils.TinyArray(u8, 3),
    operands: utils.TinyArray(LangOperand, max_operands),
};

pub fn lookup(vm: *const Vm, name: []const u8) ?LangIns {
    const op = std.meta.stringToEnum(Op, name) orelse return null;
    const opcode = vm.opcodes[@intFromEnum(op)];
    if (opcode.len == 0) return null;
    const operands = vm.operands[@intFromEnum(op)];
    return .{ .op = op, .opcode = opcode, .operands = operands };
}

pub const Ins = struct {
    start: u16,
    end: u16,
    op: union(enum) { op: Op, unknown_byte },
    operands: utils.TinyArray(Operand, max_operands),
};

pub const Operand = union(LangOperand) {
    u8: u8,
    i16: i16,
    i32: i32,
    relative_offset: i16,
    variable: Variable,
    string: []const u8,
};

pub const Variable = struct {
    raw: u16,

    pub const Kind = enum {
        global,
        local,
        room,
    };

    pub fn init(kind: Kind, num: u14) Variable {
        const k: u16 = switch (kind) {
            .global => 0,
            .local => 0x4000,
            .room => 0x8000,
        };
        return .{ .raw = k | num };
    }

    pub fn decode(self: Variable) !struct { Kind, u14 } {
        const kind: Kind = switch (self.raw & 0xc000) {
            0x0000 => .global,
            0x4000 => .local,
            0x8000 => .room,
            0xc000 => return error.BadData,
            else => unreachable,
        };
        return .{ kind, @truncate(self.raw) };
    }
};

pub const Disasm = struct {
    vm: *const Vm,
    reader: std.io.FixedBufferStream([]const u8),
    poison: bool,

    pub fn init(vm: *const Vm, bytecode: []const u8) Disasm {
        const reader = std.io.fixedBufferStream(bytecode);

        return .{
            .vm = vm,
            .reader = reader,
            .poison = false,
        };
    }

    pub fn next(self: *Disasm) !?Ins {
        if (self.reader.pos == self.reader.buffer.len)
            return null;

        if (self.poison)
            return unknownByte(&self.reader);

        const ins_start: u16 = @intCast(self.reader.pos);
        // Follow the nested entries until we find the op
        var group_pos: u16 = 0;
        while (true) {
            const byte = self.reader.reader().readByte() catch unreachable;
            switch (self.vm.opcode_lookup[group_pos + byte].decode()) {
                .op => |op| return try disasmIns(self.vm, &self.reader, ins_start, op),
                .unset => return self.becomePoison(1),
                .nested => |next_start| group_pos = next_start,
            }
        }
    }

    // The stream is not self-synchronizing, so if we fail to decode any byte,
    // it's not possible to recover.
    fn becomePoison(self: *Disasm, rewind: u8) !?Ins {
        self.reader.pos -= rewind;
        self.poison = true;
        return unknownByte(&self.reader);
    }
};

// precondition: not at EOF
fn unknownByte(reader: anytype) !?Ins {
    const start: u16 = @intCast(reader.pos);
    const byte = reader.reader().readByte() catch unreachable;
    const end: u16 = @intCast(reader.pos);
    var operands: utils.TinyArray(Operand, max_operands) = .empty;
    operands.append(.{ .u8 = byte }) catch unreachable;
    return .{
        .start = start,
        .end = end,
        .op = .unknown_byte,
        .operands = operands,
    };
}

fn disasmIns(vm: *const Vm, reader: anytype, start: u16, op: Op) !Ins {
    var lang_operands = vm.operands[@intFromEnum(op)];
    var operands: utils.TinyArray(Operand, max_operands) = .empty;
    for (lang_operands.slice()) |lang_op| {
        const operand = try disasmOperand(reader, lang_op);
        operands.append(operand) catch unreachable;
    }
    const end: u16 = @intCast(reader.pos);
    return .{
        .start = start,
        .end = end,
        .op = .{ .op = op },
        .operands = operands,
    };
}

fn disasmOperand(reader: anytype, op: LangOperand) !Operand {
    switch (op) {
        .u8 => {
            const n = try reader.reader().readInt(u8, .little);
            return .{ .u8 = n };
        },
        .i16 => {
            const n = try reader.reader().readInt(i16, .little);
            return .{ .i16 = n };
        },
        .i32 => {
            const n = try reader.reader().readInt(i32, .little);
            return .{ .i32 = n };
        },
        .relative_offset => {
            const n = try reader.reader().readInt(i16, .little);
            return .{ .relative_offset = n };
        },
        .variable => {
            const variable = try readVariable(reader);
            return .{ .variable = variable };
        },
        .string => {
            const string = try readString(reader);
            return .{ .string = string };
        },
    }
}

fn readVariable(reader: anytype) !Variable {
    const raw = try reader.reader().readInt(u16, .little);
    return .{ .raw = raw };
}

fn readString(reader: anytype) ![]const u8 {
    const start = reader.pos;
    const null_pos = std.mem.indexOfScalarPos(u8, reader.buffer, start, 0) orelse
        return error.BadData;
    reader.pos = null_pos + 1;
    return reader.buffer[start..null_pos];
}
