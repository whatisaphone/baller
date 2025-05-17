const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const ops = @import("decompile.zig").ops;
const lang = @import("lang.zig");
const lexer = @import("lexer.zig");
const script = @import("script.zig");

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
        .set => |*s| {
            const lhs = &cx.ast.nodes.items[s.lhs];
            switch (lhs.*) {
                .identifier => {
                    try pushExpr(cx, s.rhs);
                    try emitOpcodeByName(cx, "set");
                    try emitVariable(cx, s.lhs);
                },
                .array_get => |a| {
                    try pushExpr(cx, a.index);
                    try pushExpr(cx, s.rhs);
                    try emitOpcodeByName(cx, "set-array-item");
                    try emitVariable(cx, a.lhs);
                },
                .array_get2 => |a| {
                    try pushExpr(cx, a.index1);
                    try pushExpr(cx, a.index2);
                    try pushExpr(cx, s.rhs);
                    try emitOpcodeByName(cx, "set-array-item-2d");
                    try emitVariable(cx, a.lhs);
                },
                else => return error.BadData,
            }
        },
        .call => try emitCall(cx, node_index),
        .@"if" => |*s| {
            try pushExpr(cx, s.condition);
            try emitOpcodeByName(cx, "jump-unless");
            const cond_fixup: u32 = @intCast(cx.out.items.len);
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);
            try emitBlock(cx, s.true);
            if (s.false.len != 0) {
                try emitOpcodeByName(cx, "jump");
                const true_end_fixup: u32 = @intCast(cx.out.items.len);
                _ = try cx.out.addManyAsSlice(cx.gpa, 2);
                try fixupJumpToHere(cx, cond_fixup);
                try emitBlock(cx, s.false);
                try fixupJumpToHere(cx, true_end_fixup);
            } else {
                try fixupJumpToHere(cx, cond_fixup);
            }
        },
        .@"while" => |*s| {
            const loop_target: u32 = @intCast(cx.out.items.len);
            try pushExpr(cx, s.condition);
            try emitOpcodeByName(cx, "jump-unless");
            const cond_fixup: u32 = @intCast(cx.out.items.len);
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);
            try emitBlock(cx, s.body);
            try emitOpcodeByName(cx, "jump");
            try writeJumpTargetBackwards(cx, loop_target);
            try fixupJumpToHere(cx, cond_fixup);
        },
        .do => |*s| {
            const loop_target: u32 = @intCast(cx.out.items.len);
            try emitBlock(cx, s.body);
            if (s.condition == Ast.null_node) {
                try emitOpcodeByName(cx, "jump");
                try writeJumpTargetBackwards(cx, loop_target);
            } else {
                try pushExpr(cx, s.condition);
                try emitOpcodeByName(cx, "jump-unless");
                try writeJumpTargetBackwards(cx, loop_target);
            }
        },
        .case => |*s| {
            var end_fixups: std.BoundedArray(u32, Ast.max_case_branches) = .{};
            try pushExpr(cx, s.value);
            var cond_fixup: ?u32 = null;
            for (cx.ast.getExtra(s.branches)) |ni_branch| {
                const branch = &cx.ast.nodes.items[ni_branch].case_branch;
                if (cond_fixup) |fixup|
                    try fixupJumpToHere(cx, fixup);
                if (branch.value != Ast.null_node) {
                    try emitOpcodeByName(cx, "dup");
                    try pushExpr(cx, branch.value);
                    try emitOpcodeByName(cx, "eq");
                    try emitOpcodeByName(cx, "jump-unless");
                    cond_fixup = @intCast(cx.out.items.len);
                    _ = try cx.out.addManyAsSlice(cx.gpa, 2);
                }
                try emitOpcodeByName(cx, "pop");
                try emitBlock(cx, branch.body);
                if (branch.value != Ast.null_node) {
                    try emitOpcodeByName(cx, "jump");
                    end_fixups.append(@intCast(cx.out.items.len)) catch unreachable;
                    _ = try cx.out.addManyAsSlice(cx.gpa, 2);
                }
            }
            for (end_fixups.slice()) |fixup|
                try fixupJumpToHere(cx, fixup);
        },
        // TODO: handle all errors during parsing and make this unreachable
        else => return error.BadData,
    }
}

fn emitCall(cx: *Cx, node_index: u32) !void {
    const call = &cx.ast.nodes.items[node_index].call;

    const callee = findCallee(cx, call.callee) orelse {
        const token_index = cx.ast.node_tokens.items[node_index];
        const loc = cx.lex.tokens.items[token_index].span.start;
        cx.diag.err(loc, "instruction not found", .{});
        return error.AddedToDiagnostic;
    };
    switch (callee) {
        .ins => |*ins| try emitCallIns(cx, ins, call.args),
        .compound => |c| try emitCallCompound(cx, c, call.args),
    }
}

fn emitCallIns(cx: *Cx, ins: *const InsData, args_slice: Ast.ExtraSlice) !void {
    const args = cx.ast.getExtra(args_slice);
    const required = ins.operands.len + ins.normal_params;
    if (args.len < required) return error.BadData;
    const args_operands = args[0..ins.operands.len];
    const args_stack = args[ins.operands.len..required];
    const args_variadic = args[required..];
    if (!ins.variadic and args_variadic.len != 0) return error.BadData;

    for (args_stack) |ei|
        try pushExpr(cx, ei);
    if (ins.variadic)
        try pushList(cx, args_variadic);

    try cx.out.appendSlice(cx.gpa, ins.opcode.slice());

    for (ins.operands.slice(), args_operands) |op, ei|
        try emitOperand(cx, op, ei);
}

const InsData = struct {
    opcode: std.BoundedArray(u8, 2),
    name: lang.Op,
    operands: std.BoundedArray(lang.LangOperand, lang.max_operands),
    normal_params: usize,
    variadic: bool,
};

fn findCallee(cx: *const Cx, node_index: u32) ?union(enum) {
    ins: InsData,
    compound: script.Compound,
} {
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
    return if (lang.lookup(cx.language, cx.ins_map, name)) |i|
        .{ .ins = makeInsData(i[0], i[1]) orelse return null }
    else if (std.meta.stringToEnum(script.Compound, name)) |c|
        .{ .compound = c }
    else
        null;
}

fn makeInsData(opcode: std.BoundedArray(u8, 2), ins: *const lang.LangIns) ?InsData {
    if (ins.name != .op) return null;
    const params: []const script.Param = switch (ops.getPtrConst(ins.name.op).*) {
        .jump_if, .jump_unless => &.{.int},
        .jump => &.{},
        .generic => |*g| g.params.slice(),
        else => return null,
    };
    var param_exprs = params.len;
    var variadic = false;
    if (params.len != 0) {
        // Only support lists if they're in the last position
        for (params[0 .. params.len - 1]) |param|
            if (param == .list) return null;
        if (params[params.len - 1] == .list) {
            variadic = true;
            param_exprs -= 1;
        }
    }
    return .{
        .opcode = opcode,
        .name = ins.name.op,
        .operands = ins.operands,
        .normal_params = param_exprs,
        .variadic = variadic,
    };
}

fn emitCallCompound(cx: *Cx, compound: script.Compound, args_slice: Ast.ExtraSlice) !void {
    const args = cx.ast.getExtra(args_slice);
    switch (compound) {
        .@"sprite-select" => {
            if (args.len != 1) return error.BadData;
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "dup");
            try emitOpcodeByName(cx, "sprite-select-range");
        },
        .@"palette-set-slot-color" => {
            if (args.len != 2) return error.BadData;
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "dup");
            try pushExpr(cx, args[1]);
            try emitOpcodeByName(cx, "palette-set-color");
        },
    }
}

fn pushExpr(cx: *Cx, node_index: u32) error{ OutOfMemory, AddedToDiagnostic, BadData }!void {
    const expr = &cx.ast.nodes.items[node_index];
    switch (expr.*) {
        .integer => |int| try pushInt(cx, int),
        .string => try pushStr(cx, node_index),
        .identifier => {
            try emitOpcodeByName(cx, "push-var");
            try emitVariable(cx, node_index);
        },
        .call => try emitCall(cx, node_index),
        .array_get => |e| {
            try pushExpr(cx, e.index);
            try emitOpcodeByName(cx, "get-array-item");
            try emitVariable(cx, e.lhs);
        },
        .array_get2 => |e| {
            try pushExpr(cx, e.index1);
            try pushExpr(cx, e.index2);
            try emitOpcodeByName(cx, "get-array-item-2d");
            try emitVariable(cx, e.lhs);
        },
        .binop => |e| {
            try pushExpr(cx, e.lhs);
            try pushExpr(cx, e.rhs);
            try emitOpcodeByName(cx, @tagName(e.op));
        },
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
        try emitOpcodeByName(cx, "push-i32");
        try cx.out.writer(cx.gpa).writeInt(i32, integer, .little);
    }
}

fn pushStr(cx: *Cx, node_index: u32) !void {
    try emitOpcodeByName(cx, "push-str");
    try emitString(cx, node_index);
    try pushInt(cx, -1);
}

fn pushList(cx: *Cx, items: []const Ast.NodeIndex) !void {
    for (items) |ei|
        try pushExpr(cx, ei);
    try pushInt(cx, @intCast(items.len));
}

// TODO: this is why this is slow! don't use strings!
fn emitOpcodeByName(cx: *const Cx, name: []const u8) !void {
    const opcode = cx.ins_map.get(name) orelse return error.BadData;
    try cx.out.appendSlice(cx.gpa, opcode.slice());
}

fn emitOperand(cx: *Cx, op: lang.LangOperand, node_index: u32) !void {
    switch (op) {
        // TODO: try to get rid of u8 here. all occurrences are probably better
        // represented as subopcodes
        .u8 => {
            const node = &cx.ast.nodes.items[node_index];
            if (node.* != .integer) return error.BadData;
            const i = std.math.cast(u8, node.integer) orelse return error.BadData;
            try cx.out.append(cx.gpa, i);
        },
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
        .string => try emitString(cx, node_index),
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

fn emitString(cx: *const Cx, node_index: u32) !void {
    const expr = &cx.ast.nodes.items[node_index];
    if (expr.* != .string) return error.BadData;
    const str = expr.string;
    try cx.out.appendSlice(cx.gpa, str);
    try cx.out.append(cx.gpa, 0);
}

fn fixupJumpToHere(cx: *Cx, dest: u32) !void {
    const rel_wide = @as(u32, @intCast(cx.out.items.len)) - (dest + 2);
    const rel = std.math.cast(i16, rel_wide) orelse return error.BadData;
    std.mem.writeInt(i16, cx.out.items[dest..][0..2], rel, .little);
}

fn writeJumpTargetBackwards(cx: *Cx, target: u32) !void {
    const here: i32 = @intCast(cx.out.items.len);
    const target_signed: i32 = @intCast(target);
    const rel_wide = target_signed - (here + 2);
    const rel = std.math.cast(i16, rel_wide) orelse return error.BadData;
    try cx.out.writer(cx.gpa).writeInt(i16, rel, .little);
}
