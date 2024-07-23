const std = @import("std");

const lang = @import("lang.zig");
const utils = @import("utils.zig");

pub fn disassemble(
    allocator: std.mem.Allocator,
    bytecode: []const u8,
    out: anytype,
) !void {
    disassembleInner(allocator, bytecode, out) catch |err| switch (err) {
        error.BadData, error.EndOfStream => return error.BadData,
        else => return err,
    };
}

pub fn disassembleInner(
    allocator: std.mem.Allocator,
    bytecode: []const u8,
    out: anytype,
) !void {
    // Lots of stuff seems to assume pc fits in u16.
    if (bytecode.len > 0xffff)
        return error.BadData;

    var dasm = lang.Disasm.init(bytecode);

    var jump_targets = try findJumpTargets(allocator, bytecode);
    defer jump_targets.deinit(allocator);

    var next_jump_index: u16 = 0;
    while (try dasm.next()) |ins| {
        // If we're at a jump target, emit the label
        if (next_jump_index < jump_targets.items.len) {
            const next_jump_target = jump_targets.items[next_jump_index];
            if (ins.start == next_jump_target) {
                try emitLabel(ins.start, out);
                try out.writeAll(":\n");

                next_jump_index += 1;
            } else if (ins.start > next_jump_target) {
                // This would mean jumping between opcodes?
                return error.BadData;
            }
        }

        // Emit the instruction
        try out.writeAll("    ");
        try out.writeAll(ins.name);
        for (ins.operands.slice()) |op| {
            try out.writeByte(' ');
            try emitOperand(op, ins.end, out);
        }
        try out.writeByte('\n');
    }

    // If we didn't reach the last jump target, it means an instruction jumped
    // beyond the end of the script? Not good.
    if (next_jump_index != jump_targets.items.len)
        return error.BadData;
}

fn findJumpTargets(
    allocator: std.mem.Allocator,
    bytecode: []const u8,
) !std.ArrayListUnmanaged(u16) {
    const initial_capacity = bytecode.len / 10;
    var targets =
        try std.ArrayListUnmanaged(u16).initCapacity(allocator, initial_capacity);
    errdefer targets.deinit(allocator);

    var dasm = lang.Disasm.init(bytecode);
    while (try dasm.next()) |ins| {
        // Check if it's a jump
        if (ins.operands.len != 1) continue;
        if (ins.operands.get(0) != .relative_offset) continue;

        // Calc and store the absolute jump target
        const rel = ins.operands.get(0).relative_offset;
        const abs = utils.addUnsignedSigned(ins.end, rel) orelse return error.BadData;
        try insertSortedNoDup(allocator, &targets, abs);
    }
    std.debug.assert(std.sort.isSorted(u16, targets.items, {}, std.sort.asc(u16)));
    return targets;
}

fn insertSortedNoDup(
    allocator: std.mem.Allocator,
    list: *std.ArrayListUnmanaged(u16),
    item: u16,
) !void {
    const index = std.sort.upperBound(u16, item, list.items, {}, std.sort.asc(u16));
    if (index > 0 and list.items[index - 1] == item)
        return;
    try list.insert(allocator, index, item);
}

fn emitLabel(pc: u16, out: anytype) !void {
    try out.print("L_{x:0>4}", .{pc});
}

fn emitOperand(op: lang.Operand, pc: u16, out: anytype) !void {
    switch (op) {
        .u8, .i16, .i32 => |n| {
            try out.print("{}", .{n});
        },
        .relative_offset => |rel| {
            // This was already verified to be valid in findJumpTargets
            const abs = utils.addUnsignedSigned(pc, rel).?;
            try emitLabel(abs, out);
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

fn emitVariable(out: anytype, variable: lang.Variable) !void {
    switch (try variable.decode()) {
        .global => |num| try out.print("global{}", .{num}),
        .local => |num| try out.print("local{}", .{num}),
        .room => |num| try out.print("room{}", .{num}),
    }
}
