const std = @import("std");

const Ast = @import("Ast.zig");
const lang = @import("lang.zig");

pub fn compile(
    gpa: std.mem.Allocator,
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    ast: *const Ast,
    statements: Ast.ExtraSlice,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    var cx: Cx = .{
        .gpa = gpa,
        .language = language,
        .ins_map = ins_map,
        .ast = ast,
        .out = out,
    };

    for (ast.getExtra(statements)) |i|
        try emitStatement(&cx, i);
}

const Cx = struct {
    gpa: std.mem.Allocator,
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    ast: *const Ast,
    out: *std.ArrayListUnmanaged(u8),
};

fn emitStatement(cx: *const Cx, node_index: u32) !void {
    const node = &cx.ast.nodes.items[node_index];
    switch (node.*) {
        .call => try emitCall(cx, node_index),
        else => return error.BadData,
    }
}

fn emitCall(cx: *const Cx, node_index: u32) !void {
    const call = &cx.ast.nodes.items[node_index].call;

    const opcode, const ins = try findIns(cx, call.callee);

    const args = cx.ast.getExtra(call.args);
    // TODO: type check number of stack pushes
    if (args.len < ins.operands.len) return error.BadData;
    const args_operands = args[0..ins.operands.len];
    const args_stack = args[ins.operands.len..];

    for (args_stack) |ei|
        try pushExpr(cx, ei);

    try cx.out.appendSlice(cx.gpa, opcode.slice());

    for (ins.operands.slice(), args_operands) |op, ei|
        try emitOperand(cx, op, ei);
}

fn findIns(
    cx: *const Cx,
    node_index: u32,
) !struct { std.BoundedArray(u8, 2), *const lang.LangIns } {
    const expr = &cx.ast.nodes.items[node_index];
    switch (expr.*) {
        .identifier => |id| return lang.lookup(cx.language, cx.ins_map, id) orelse error.BadData,
        else => return error.BadData,
    }
}

fn pushExpr(cx: *const Cx, node_index: u32) !void {
    const expr = &cx.ast.nodes.items[node_index];
    switch (expr.*) {
        .integer => |int| try pushInt(cx, int),
        .identifier => |id| {
            const variable = parseVariable(id) orelse return error.BadData;
            try emitOpcodeByName(cx, "push-var");
            try emitVariable(cx, variable);
        },
        else => return error.BadData,
    }
}

fn pushInt(cx: *const Cx, integer: i32) !void {
    if (std.math.cast(u8, integer)) |i| {
        try emitOpcodeByName(cx, "push-u8");
        try cx.out.append(cx.gpa, i);
    } else {
        return error.BadData;
    }
}

// TODO: this is why this is slow! don't use strings!
fn emitOpcodeByName(cx: *const Cx, name: []const u8) !void {
    const opcode = cx.ins_map.get(name) orelse return error.BadData;
    try cx.out.appendSlice(cx.gpa, opcode.slice());
}

fn emitOperand(cx: *const Cx, op: lang.LangOperand, node_index: u32) !void {
    switch (op) {
        .variable => try emitVariableByExpr(cx, node_index),
        else => unreachable,
    }
}

fn emitVariableByExpr(cx: *const Cx, node_index: u32) !void {
    const expr = &cx.ast.nodes.items[node_index];
    if (expr.* != .identifier) return error.BadData;
    const id = expr.identifier;
    const variable = parseVariable(id) orelse return error.BadData;
    try emitVariable(cx, variable);
}

fn emitVariable(cx: *const Cx, variable: lang.Variable) !void {
    try cx.out.writer(cx.gpa).writeInt(u16, variable.raw, .little);
}

fn parseVariable(str: []const u8) ?lang.Variable {
    const kind: lang.Variable.Kind, const num_str =
        if (str.len >= 6 and std.mem.startsWith(u8, str, "global"))
            .{ .global, str[6..] }
        else if (str.len >= 5 and std.mem.startsWith(u8, str, "local"))
            .{ .local, str[5..] }
        else if (str.len >= 4 and std.mem.startsWith(u8, str, "room"))
            .{ .room, str[4..] }
        else
            return null;
    const num = std.fmt.parseInt(u14, num_str, 10) catch return null;
    return .init2(kind, num);
}
