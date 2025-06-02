const std = @import("std");

const Symbols = @import("Symbols.zig");
const UsageTracker = @import("UsageTracker.zig");
const lang = @import("lang.zig");
const utils = @import("utils.zig");

pub fn disassemble(
    allocator: std.mem.Allocator,
    language: *const lang.Language,
    room_number: u8,
    id: Symbols.ScriptId,
    bytecode: []const u8,
    symbols: *const Symbols,
    annotate: bool,
    out: anytype,
    usage: *UsageTracker,
    diagnostic: anytype,
) !void {
    disassembleInner(
        allocator,
        language,
        room_number,
        id,
        bytecode,
        symbols,
        annotate,
        out,
        usage,
        diagnostic,
    ) catch |err| switch (err) {
        error.BadData, error.EndOfStream => return error.BadData,
        else => return err,
    };
}

fn disassembleInner(
    allocator: std.mem.Allocator,
    language: *const lang.Language,
    room_number: u8,
    id: Symbols.ScriptId,
    bytecode: []const u8,
    symbols: *const Symbols,
    annotate: bool,
    out: anytype,
    usage: *UsageTracker,
    diagnostic: anytype,
) !void {
    var warned_for_unknown_byte = false;

    // Lots of stuff seems to assume pc fits in u16.
    if (bytecode.len > 0xffff)
        return error.BadData;

    try writePreamble(room_number, id, symbols, out);

    var dasm: lang.Disasm = .init(language, bytecode);

    var jump_targets = try findJumpTargets(allocator, language, bytecode);
    defer jump_targets.deinit(allocator);

    var next_jump_index: u16 = 0;
    while (try dasm.next()) |ins| {
        // If we're at a jump target, emit the label
        if (next_jump_index < jump_targets.items.len) {
            const next_jump_target = jump_targets.items[next_jump_index];
            if (ins.start == next_jump_target) {
                if (annotate)
                    try out.writeAll("        ");
                try emitLabel(ins.start, out);
                try out.writeAll(":\n");

                next_jump_index += 1;
            } else if (ins.start > next_jump_target) {
                // This would mean jumping between opcodes?
                return error.BadData;
            }
        }

        if (ins.op == .unknown_byte and !warned_for_unknown_byte) {
            warned_for_unknown_byte = true;
            diagnostic.warnScriptUnknownByte();
        }

        // Emit the instruction
        if (annotate)
            try out.print("0x{x:0>4}  ", .{ins.start});
        try out.writeAll("    ");
        try out.writeAll(@tagName(ins.op.op));
        for (ins.operands.slice()) |op| {
            try out.writeByte(' ');
            try emitOperand(op, ins.end, jump_targets.items, out, usage, symbols, room_number, id);
        }
        try out.writeByte('\n');
    }

    // If we didn't reach the last jump target, it means an instruction jumped
    // beyond the end of the script? Not good.
    if (next_jump_index != jump_targets.items.len)
        return error.BadData;
}

fn writePreamble(room_number: u8, id: Symbols.ScriptId, symbols: *const Symbols, out: anytype) !void {
    const script = symbols.getScript(id) orelse return;

    if (script.name) |name|
        try out.print("; {s}\n\n", .{name});

    if (script.locals.len() != 0) {
        for (0..script.locals.len()) |i| {
            const variable: lang.Variable = .init(.{ .local = @intCast(i) });
            try out.writeAll(".local ");
            try emitVariable(out, variable, symbols, room_number, id);
            try out.writeByte('\n');
        }
        try out.writeByte('\n');
    }
}

fn findJumpTargets(
    allocator: std.mem.Allocator,
    language: *const lang.Language,
    bytecode: []const u8,
) !std.ArrayListUnmanaged(u16) {
    const initial_capacity = bytecode.len / 10;
    var targets: std.ArrayListUnmanaged(u16) = try .initCapacity(allocator, initial_capacity);
    errdefer targets.deinit(allocator);

    var dasm: lang.Disasm = .init(language, bytecode);
    while (try dasm.next()) |ins| {
        // Check if it's a jump
        if (ins.operands.len == 0) continue;
        const op = ins.operands.get(ins.operands.len - 1);
        if (op != .relative_offset) continue;
        const rel = op.relative_offset;

        // Calc and store the absolute jump target
        const abs = utils.add(u16, ins.end, rel) orelse return error.BadData;
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
    const index = std.sort.upperBound(u16, list.items, item, orderU16);
    if (index > 0 and list.items[index - 1] == item)
        return;
    try list.insert(allocator, index, item);
}

fn orderU16(a: u16, b: u16) std.math.Order {
    return std.math.order(a, b);
}

fn emitLabel(pc: u16, out: anytype) !void {
    try out.print("L_{x:0>4}", .{pc});
}

fn emitOperand(
    op: lang.Operand,
    pc: u16,
    jump_targets: []const u16,
    out: anytype,
    usage: *UsageTracker,
    symbols: *const Symbols,
    room_number: u8,
    id: Symbols.ScriptId,
) !void {
    switch (op) {
        .u8, .i16, .i32 => |n| {
            try out.print("{}", .{n});
        },
        .relative_offset => |rel| {
            // This was already verified not to overflow in findJumpTargets
            const abs = utils.add(u16, pc, rel).?;
            std.debug.assert(std.mem.indexOfScalar(u16, jump_targets, abs) != null);
            try emitLabel(abs, out);
        },
        .variable => |v| {
            try emitVariable(out, v, symbols, room_number, id);
            try usage.track(v);
        },
        .string => |s| {
            // TODO: escaping
            try out.writeByte('"');
            try out.writeAll(s);
            try out.writeByte('"');
        },
    }
}

fn emitVariable(
    out: anytype,
    variable: lang.Variable,
    symbols: *const Symbols,
    room_number: u8,
    id: Symbols.ScriptId,
) !void {
    switch (try variable.decode()) {
        .global => |num| {
            const name_opt = symbols.globals.get(num);
            if (name_opt) |name|
                try out.writeAll(name)
            else
                try out.print("global{}", .{num});
        },
        .local => |num| {
            if (symbols.getScript(id)) |script| {
                if (script.locals.get(num)) |name| {
                    try out.writeAll(name);
                    return;
                }
            }
            try out.print("local{}", .{num});
        },
        .room => |num| {
            if (symbols.getRoom(room_number)) |room| {
                if (room.vars.get(num)) |name| {
                    try out.writeAll(name);
                    return;
                }
            }
            try out.print("room{}", .{num});
        },
    }
}
