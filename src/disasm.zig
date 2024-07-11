const disasm = @import("disasm_decode.zig");

pub fn disassemble(bytecode: []const u8, out: anytype) !void {
    var dasm = disasm.Disasm.init(bytecode);

    while (try dasm.next()) |ins| {
        try out.writeAll(ins.name);
        for (ins.operands.slice()) |op| {
            try out.writeByte(' ');
            try emitOperand(op, out);
        }
        try out.writeByte('\n');
    }
}

fn emitOperand(op: disasm.Operand, out: anytype) !void {
    switch (op) {
        .u8, .i16, .i32 => |n| {
            try out.print("{}", .{n});
        },
        .variable => |v| {
            try emitVariable(out, v);
        },
        .string => |s| {
            // TODO: escaping
            try out.writeByte('"');
            try out.writeAll(s);
            try out.writeByte('"');
        },
    }
}

fn emitVariable(out: anytype, variable: disasm.Variable) !void {
    switch (try variable.decode()) {
        .global => |num| try out.print("global{}", .{num}),
        .local => |num| try out.print("local{}", .{num}),
        .room => |num| try out.print("room{}", .{num}),
    }
}
