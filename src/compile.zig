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
        .label_offsets = .empty,
        .label_fixups = .empty,
    };
    defer cx.label_fixups.deinit(gpa);
    defer cx.label_offsets.deinit(gpa);

    for (ast.getExtra(statements)) |i|
        try emitStatement(&cx, i);

    for (cx.label_fixups.items) |fixup| {
        const label_offset = cx.label_offsets.get(fixup.label_name) orelse return error.BadData;
        const rel_wide = @as(i32, label_offset) - @as(i32, fixup.offset) - 2;
        const rel = std.math.cast(i16, rel_wide) orelse return error.BadData;
        std.mem.writeInt(i16, cx.out.items[fixup.offset..][0..2], rel, .little);
    }
}

const Cx = struct {
    gpa: std.mem.Allocator,
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    ast: *const Ast,

    out: *std.ArrayListUnmanaged(u8),
    label_offsets: std.StringHashMapUnmanaged(u16),
    label_fixups: std.ArrayListUnmanaged(struct { offset: u16, label_name: []const u8 }),
};

fn emitStatement(cx: *Cx, node_index: u32) !void {
    const node = &cx.ast.nodes.items[node_index];
    switch (node.*) {
        .label => |name| {
            const entry = try cx.label_offsets.getOrPut(cx.gpa, name);
            if (entry.found_existing) return error.BadData;
            entry.value_ptr.* = @intCast(cx.out.items.len);
        },
        .call => try emitCall(cx, node_index),
        else => return error.BadData,
    }
}

fn emitCall(cx: *Cx, node_index: u32) !void {
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
    var name_buf: [24]u8 = undefined;
    const name = switch (expr.*) {
        .identifier => |id| id,
        .field => |f| blk: {
            const lhs = &cx.ast.nodes.items[f.lhs];
            if (lhs.* != .identifier) return error.BadData;
            const lhs_str = lhs.identifier;
            break :blk std.fmt.bufPrint(&name_buf, "{s}.{s}", .{ lhs_str, f.field }) catch
                return error.BadData;
        },
        else => return error.BadData,
    };
    return lang.lookup(cx.language, cx.ins_map, name) orelse error.BadData;
}

fn pushExpr(cx: *Cx, node_index: u32) error{ OutOfMemory, BadData }!void {
    const expr = &cx.ast.nodes.items[node_index];
    switch (expr.*) {
        .integer => |int| try pushInt(cx, int),
        .identifier => |id| {
            const variable = parseVariable(id) orelse return error.BadData;
            try emitOpcodeByName(cx, "push-var");
            try emitVariable(cx, variable);
        },
        .call => try emitCall(cx, node_index),
        .list => try pushList(cx, node_index),
        else => return error.BadData,
    }
}

fn pushInt(cx: *const Cx, integer: i32) !void {
    if (std.math.cast(u8, integer)) |i| {
        try emitOpcodeByName(cx, "push-u8");
        try cx.out.append(cx.gpa, i);
    } else if (std.math.cast(i16, integer)) |i| {
        try emitOpcodeByName(cx, "push-i16");
        try cx.out.writer(cx.gpa).writeInt(i16, i, .little);
    } else {
        return error.BadData;
    }
}

fn pushList(cx: *Cx, node_index: u32) !void {
    const list = &cx.ast.nodes.items[node_index].list;
    for (cx.ast.getExtra(list.items)) |ei|
        try pushExpr(cx, ei);
    try pushInt(cx, @intCast(list.items.len));
}

// TODO: this is why this is slow! don't use strings!
fn emitOpcodeByName(cx: *const Cx, name: []const u8) !void {
    const opcode = cx.ins_map.get(name) orelse return error.BadData;
    try cx.out.appendSlice(cx.gpa, opcode.slice());
}

fn emitOperand(cx: *Cx, op: lang.LangOperand, node_index: u32) !void {
    switch (op) {
        .relative_offset => {
            const label_expr = &cx.ast.nodes.items[node_index];
            if (label_expr.* != .identifier) return error.BadData;
            const label_name = label_expr.identifier;

            const offset: u16 = @intCast(cx.out.items.len);
            // undefined bytes will be filled in at the end
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);
            try cx.label_fixups.append(cx.gpa, .{ .offset = offset, .label_name = label_name });
        },
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
