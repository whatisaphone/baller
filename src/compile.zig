const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const lang = @import("lang.zig");
const lexer = @import("lexer.zig");

pub fn compile(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    project_scope: *const std.StringHashMapUnmanaged(lang.Variable),
    file: *const Project.SourceFile,
    root_node: Ast.NodeIndex,
    statements: Ast.ExtraSlice,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    var cx: Cx = .{
        .gpa = gpa,
        .diag = diag,
        .language = language,
        .ins_map = ins_map,
        .project_scope = project_scope,
        .lex = &file.lex,
        .ast = &file.ast,
        .out = out,
        .label_offsets = .empty,
        .label_fixups = .empty,
    };
    defer cx.label_fixups.deinit(gpa);
    defer cx.label_offsets.deinit(gpa);

    compileInner(&cx, statements) catch |err| {
        if (err != error.AddedToDiagnostic) {
            const token_index = file.ast.node_tokens.items[root_node];
            const loc = file.lex.tokens.items[token_index].span.start;
            diag.zigErr(loc, "unexpected error: {s}", .{}, err);
        }
    };
}

pub fn compileInner(cx: *Cx, statements: Ast.ExtraSlice) !void {
    try emitBlock(cx, statements);

    for (cx.label_fixups.items) |fixup| {
        const label_offset = cx.label_offsets.get(fixup.label_name) orelse return error.BadData;
        const rel_wide = @as(i32, label_offset) - @as(i32, fixup.offset) - 2;
        const rel = std.math.cast(i16, rel_wide) orelse return error.BadData;
        std.mem.writeInt(i16, cx.out.items[fixup.offset..][0..2], rel, .little);
    }
}

const Cx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    project_scope: *const std.StringHashMapUnmanaged(lang.Variable),
    lex: *const lexer.Lex,
    ast: *const Ast,

    out: *std.ArrayListUnmanaged(u8),
    label_offsets: std.StringHashMapUnmanaged(u16),
    label_fixups: std.ArrayListUnmanaged(struct { offset: u16, label_name: []const u8 }),
};

fn emitBlock(cx: *Cx, slice: Ast.ExtraSlice) error{ OutOfMemory, AddedToDiagnostic, BadData }!void {
    for (cx.ast.getExtra(slice)) |i|
        try emitStatement(cx, i);
}

fn emitStatement(cx: *Cx, node_index: u32) !void {
    const node = &cx.ast.nodes.items[node_index];
    switch (node.*) {
        .label => |name| {
            const entry = try cx.label_offsets.getOrPut(cx.gpa, name);
            if (entry.found_existing) return error.BadData;
            entry.value_ptr.* = @intCast(cx.out.items.len);
        },
        .call => try emitCall(cx, node_index),
        .@"if" => |s| {
            try pushExpr(cx, s.condition);
            try emitOpcodeByName(cx, "jump-unless");
            const cond_fixup = try cx.out.addManyAsArray(cx.gpa, 2);
            try emitBlock(cx, s.true);
            if (s.false.len != 0) {
                try emitOpcodeByName(cx, "jump");
                const true_end_fixup = try cx.out.addManyAsArray(cx.gpa, 2);
                try fixupJumpToHere(cx, cond_fixup);
                try emitBlock(cx, s.false);
                try fixupJumpToHere(cx, true_end_fixup);
            } else {
                try fixupJumpToHere(cx, cond_fixup);
            }
        },
        // TODO: handle all errors during parsing and make this unreachable
        else => return error.BadData,
    }
}

fn emitCall(cx: *Cx, node_index: u32) !void {
    const call = &cx.ast.nodes.items[node_index].call;

    const opcode, const ins = findIns(cx, call.callee) orelse {
        const token_index = cx.ast.node_tokens.items[node_index];
        const loc = cx.lex.tokens.items[token_index].span.start;
        cx.diag.err(loc, "instruction not found", .{});
        return error.AddedToDiagnostic;
    };

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
) ?struct { std.BoundedArray(u8, 2), *const lang.LangIns } {
    const expr = &cx.ast.nodes.items[node_index];
    var name_buf: [24]u8 = undefined;
    const name = switch (expr.*) {
        .identifier => |id| id,
        .field => |f| blk: {
            const lhs = &cx.ast.nodes.items[f.lhs];
            if (lhs.* != .identifier) return null;
            const lhs_str = lhs.identifier;
            break :blk std.fmt.bufPrint(&name_buf, "{s}.{s}", .{ lhs_str, f.field }) catch
                return null;
        },
        else => return null,
    };
    return lang.lookup(cx.language, cx.ins_map, name);
}

fn pushExpr(cx: *Cx, node_index: u32) error{ OutOfMemory, AddedToDiagnostic, BadData }!void {
    const expr = &cx.ast.nodes.items[node_index];
    switch (expr.*) {
        .integer => |int| try pushInt(cx, int),
        .identifier => {
            try emitOpcodeByName(cx, "push-var");
            try emitVariable(cx, node_index);
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
        .variable => try emitVariable(cx, node_index),
        else => unreachable,
    }
}

fn emitVariable(cx: *const Cx, node_index: u32) !void {
    const expr = &cx.ast.nodes.items[node_index];
    if (expr.* != .identifier) return error.BadData;
    const id = expr.identifier;
    const variable = parseVariable(cx, id) orelse {
        const token_index = cx.ast.node_tokens.items[node_index];
        const loc = cx.lex.tokens.items[token_index].span.start;
        cx.diag.err(loc, "variable not found", .{});
        return error.AddedToDiagnostic;
    };
    try cx.out.writer(cx.gpa).writeInt(u16, variable.raw, .little);
}

fn parseVariable(cx: *const Cx, str: []const u8) ?lang.Variable {
    if (cx.project_scope.get(str)) |v|
        return v;

    // TODO: get rid of this fallback eventually
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

fn fixupJumpToHere(cx: *Cx, dest: *[2]u8) !void {
    const target = cx.out.unusedCapacitySlice().ptr;
    const rel_wide = target - (dest.ptr + 2);
    const rel = std.math.cast(i16, rel_wide) orelse return error.BadData;
    std.mem.writeInt(i16, dest, rel, .little);
}
