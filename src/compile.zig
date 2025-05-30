const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const UsageTracker = @import("UsageTracker.zig");
const decompile = @import("decompile.zig");
const lang = @import("lang.zig");
const lexer = @import("lexer.zig");
const script = @import("script.zig");

pub fn compile(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    op_map: *const std.EnumArray(lang.Op, decompile.Op),
    project_scope: *const std.StringHashMapUnmanaged(script.Symbol),
    room_scope: *const std.StringHashMapUnmanaged(script.Symbol),
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
        .op_map = op_map,
        .project_scope = project_scope,
        .room_scope = room_scope,
        .lex = &file.lex,
        .ast = &file.ast,
        .out = out,
        .local_vars = .{},
        .label_offsets = .empty,
        .label_fixups = .empty,
    };
    defer cx.label_fixups.deinit(gpa);
    defer cx.label_offsets.deinit(gpa);

    compileInner(&cx, root_node, statements) catch |err| {
        if (err != error.AddedToDiagnostic) {
            const token_index = file.ast.node_tokens.items[root_node];
            const loc = file.lex.tokens.items[token_index].span.start;
            diag.zigErr(loc, "unexpected error: {s}", .{}, err);
        }
    };
}

pub fn compileInner(cx: *Cx, root_node: Ast.NodeIndex, statements: Ast.ExtraSlice) !void {
    try emitBody(cx, statements);

    const end = switch (cx.ast.nodes.items[root_node]) {
        .script, .local_script => "end",
        .enter, .exit => "end2",
        else => unreachable,
    };
    try emitOpcodeByName(cx, end);

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
    op_map: *const std.EnumArray(lang.Op, decompile.Op),
    project_scope: *const std.StringHashMapUnmanaged(script.Symbol),
    room_scope: *const std.StringHashMapUnmanaged(script.Symbol),
    lex: *const lexer.Lex,
    ast: *const Ast,

    out: *std.ArrayListUnmanaged(u8),
    local_vars: std.BoundedArray(?[]const u8, UsageTracker.max_local_vars),
    label_offsets: std.StringHashMapUnmanaged(u16),
    label_fixups: std.ArrayListUnmanaged(struct { offset: u16, label_name: []const u8 }),
};

fn emitBody(cx: *Cx, slice: Ast.ExtraSlice) !void {
    // Vars must come first
    const stmts = cx.ast.getExtra(slice);
    const first_non_var_stmt = for (stmts, 0..) |si, i| {
        const stmt = &cx.ast.nodes.items[si];
        if (stmt.* != .local_vars) break i;
        for (cx.ast.getExtra(stmt.local_vars.children)) |ni| {
            const var_node = &cx.ast.nodes.items[ni].local_var;
            try cx.local_vars.append(var_node.name);
        }
    } else return;

    for (stmts[first_non_var_stmt..]) |i|
        try emitStatement(cx, i);
}

fn emitBlock(cx: *Cx, slice: Ast.ExtraSlice) error{ OutOfMemory, AddedToDiagnostic, BadData }!void {
    for (cx.ast.getExtra(slice)) |i|
        try emitStatement(cx, i);
}

fn emitStatement(cx: *Cx, node_index: u32) !void {
    const node = &cx.ast.nodes.items[node_index];
    switch (node.*) {
        .label => |name| {
            const offset: u16 = @intCast(cx.out.items.len);
            const entry = try cx.label_offsets.getOrPut(cx.gpa, name);
            // TODO: forbid duplicate labels, this would mean fixing the decompiler first
            if (entry.found_existing) {
                if (entry.value_ptr.* != offset) return error.BadData;
            } else {
                entry.value_ptr.* = offset;
            }
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
        .binop_assign => |e| {
            const lhs = &cx.ast.nodes.items[e.lhs];
            if (lhs.* == .identifier) {
                try pushExpr(cx, e.lhs);
                try pushExpr(cx, e.rhs);
                try emitOpcodeByName(cx, @tagName(e.op));
                try emitOpcodeByName(cx, "set");
                try emitVariable(cx, e.lhs);
            } else if (lhs.* == .array_get) {
                try pushExpr(cx, lhs.array_get.index);
                try emitOpcodeByName(cx, "dup");
                try emitOpcodeByName(cx, "get-array-item");
                try emitVariable(cx, lhs.array_get.lhs);
                try pushExpr(cx, e.rhs);
                try emitOpcodeByName(cx, @tagName(e.op));
                try emitOpcodeByName(cx, "set-array-item");
                try emitVariable(cx, lhs.array_get.lhs);
            } else if (lhs.* == .array_get2) {
                try pushExpr(cx, lhs.array_get2.index1);
                try pushExpr(cx, lhs.array_get2.index2);
                try emitOpcodeByName(cx, "dup-multi");
                try cx.out.writer(cx.gpa).writeInt(i16, 2, .little);
                try emitOpcodeByName(cx, "get-array-item-2d");
                try emitVariable(cx, lhs.array_get2.lhs);
                try pushExpr(cx, e.rhs);
                try emitOpcodeByName(cx, @tagName(e.op));
                try emitOpcodeByName(cx, "set-array-item-2d");
                try emitVariable(cx, lhs.array_get2.lhs);
            } else return error.BadData;
        },
        .call => try emitCall(cx, node_index),
        .@"if" => |*s| {
            try pushExpr(cx, s.condition);
            try emitOpcodeByName(cx, "jump-unless");
            const cond_fixup: u32 = @intCast(cx.out.items.len);
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);
            try emitBlock(cx, s.true);
            // Mild hack here. We can tell the difference between the parser
            // parsing no else block at all (start=0, len=0), and parsing an
            // empty else block (start=???, len=0). We need to know the
            // difference for a byte-identical roundtrip.
            if (s.false.start != 0) {
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
        .@"for" => |*s| {
            try pushExpr(cx, s.start);
            try emitOpcodeByName(cx, "set");
            try emitVariable(cx, s.accumulator);

            const loop_target: u32 = @intCast(cx.out.items.len);

            try pushExpr(cx, s.accumulator);
            try pushExpr(cx, s.end);
            try emitOpcodeByName(cx, if (s.direction == .up) "le" else "ge");
            try emitOpcodeByName(cx, "jump-unless");
            const end_fixup: u32 = @intCast(cx.out.items.len);
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);

            try emitBlock(cx, s.body);

            try emitOpcodeByName(cx, if (s.direction == .up) "inc" else "dec");
            try emitVariable(cx, s.accumulator);

            try emitOpcodeByName(cx, "jump");
            try writeJumpTargetBackwards(cx, loop_target);

            try fixupJumpToHere(cx, end_fixup);
        },
        .for_in => |*s| {
            try pushExpr(cx, s.list);
            try pushInt(cx, 1);
            try emitOpcodeByName(cx, "array-assign");
            try emitVariable(cx, s.backing);

            try pushExpr(cx, s.backing);
            try emitOpcodeByName(cx, "localize");

            try pushInt(cx, 0);
            try pushInt(cx, 0);
            try emitOpcodeByName(cx, "set-array-item");
            try emitVariable(cx, s.backing);

            const loop_target: u32 = @intCast(cx.out.items.len);

            try pushInt(cx, 0);
            try emitOpcodeByName(cx, "inc-array-item");
            try emitVariable(cx, s.backing);

            try pushInt(cx, 0);
            try emitOpcodeByName(cx, "get-array-item");
            try emitVariable(cx, s.backing);
            try pushInt(cx, @intCast(cx.ast.nodes.items[s.list].list.items.len));
            try emitOpcodeByName(cx, "le");
            try emitOpcodeByName(cx, "jump-unless");
            const end_fixup: u32 = @intCast(cx.out.items.len);
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);

            try pushInt(cx, 0);
            try emitOpcodeByName(cx, "get-array-item");
            try emitVariable(cx, s.backing);
            try emitOpcodeByName(cx, "get-array-item");
            try emitVariable(cx, s.backing);
            try emitOpcodeByName(cx, "set");
            try emitVariable(cx, s.target);

            try emitBlock(cx, s.body);

            try emitOpcodeByName(cx, "jump");
            try writeJumpTargetBackwards(cx, loop_target);

            try fixupJumpToHere(cx, end_fixup);

            try emitOpcodeByName(cx, "undim");
            try emitVariable(cx, s.backing);
        },
        .case => |*s| {
            var end_fixups: std.BoundedArray(u32, Ast.max_case_branches) = .{};
            try pushExpr(cx, s.value);
            var cond_fixup: ?u32 = null;
            for (cx.ast.getExtra(s.branches)) |ni_branch| {
                const branch = &cx.ast.nodes.items[ni_branch].case_branch;
                if (cond_fixup) |fixup|
                    try fixupJumpToHere(cx, fixup);
                switch (branch.condition) {
                    .eq => |ei| {
                        try emitOpcodeByName(cx, "dup");
                        try pushExpr(cx, ei);
                        try emitOpcodeByName(cx, "eq");
                        try emitOpcodeByName(cx, "jump-unless");
                        cond_fixup = @intCast(cx.out.items.len);
                        _ = try cx.out.addManyAsSlice(cx.gpa, 2);
                    },
                    .in => |slice| {
                        try emitOpcodeByName(cx, "dup");
                        try pushList(cx, cx.ast.getExtra(slice));
                        try emitOpcodeByName(cx, "in-list");
                        try emitOpcodeByName(cx, "jump-unless");
                        cond_fixup = @intCast(cx.out.items.len);
                        _ = try cx.out.addManyAsSlice(cx.gpa, 2);
                    },
                    .default => {},
                }
                try emitOpcodeByName(cx, "pop");
                try emitBlock(cx, branch.body);
                if (branch.condition != .default) {
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
        .{ .ins = makeInsData(cx, i[0], i[1]) orelse return null }
    else if (std.meta.stringToEnum(script.Compound, name)) |c|
        .{ .compound = c }
    else
        null;
}

fn makeInsData(cx: *const Cx, opcode: std.BoundedArray(u8, 2), ins: *const lang.LangIns) ?InsData {
    if (ins.name != .op) return null;
    const params: []const script.Param = switch (cx.op_map.getPtrConst(ins.name.op).*) {
        .jump_if, .jump_unless => &.{.int},
        .jump, .override => &.{},
        .generic => |*g| g.params.slice(),
        else => return null,
    };
    var param_exprs = params.len;
    var variadic = false;
    if (params.len != 0 and params[params.len - 1] == .variadic) {
        variadic = true;
        param_exprs -= 1;
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
        .@"array-sort-row" => {
            if (args.len != 5) return error.BadData;
            try pushExpr(cx, args[1]);
            try pushExpr(cx, args[2]);
            try pushExpr(cx, args[3]);
            try emitOpcodeByName(cx, "dup");
            try pushExpr(cx, args[4]);
            try emitOpcodeByName(cx, "array-sort");
            try emitVariable(cx, args[0]);
        },
        .@"lock-and-load-script" => {
            if (args.len != 1) return error.BadData;
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "dup");
            try emitOpcodeByName(cx, "lock-script");
            try emitOpcodeByName(cx, "load-script");
        },
        .@"palette-set-slot-rgb" => {
            if (args.len != 4) return error.BadData;
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "dup");
            try pushExpr(cx, args[1]);
            try pushExpr(cx, args[2]);
            try pushExpr(cx, args[3]);
            try emitOpcodeByName(cx, "palette-set-rgb");
        },
        .@"palette-set-slot-color" => {
            if (args.len != 2) return error.BadData;
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "dup");
            try pushExpr(cx, args[1]);
            try emitOpcodeByName(cx, "palette-set-color");
        },
        .@"delete-one-polygon" => {
            if (args.len != 1) return error.BadData;
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "dup");
            try emitOpcodeByName(cx, "delete-polygon");
        },
        .@"break-until" => {
            if (args.len != 1) return error.BadData;
            const start: u32 = @intCast(cx.out.items.len);
            try pushExpr(cx, args[0]);
            try emitOpcodeByName(cx, "jump-if");
            const fixup: u32 = @intCast(cx.out.items.len);
            _ = try cx.out.addManyAsSlice(cx.gpa, 2);
            try emitOpcodeByName(cx, "break-here");
            try emitOpcodeByName(cx, "jump");
            try writeJumpTargetBackwards(cx, start);
            try fixupJumpToHere(cx, fixup);
        },
    }
}

fn pushExpr(cx: *Cx, node_index: u32) error{ OutOfMemory, AddedToDiagnostic, BadData }!void {
    const expr = &cx.ast.nodes.items[node_index];
    switch (expr.*) {
        .integer => |int| try pushInt(cx, int),
        .string => try pushStr(cx, node_index),
        .identifier => try pushSymbol(cx, node_index),
        .call => try emitCall(cx, node_index),
        .list => |list| try pushList(cx, cx.ast.getExtra(list.items)),
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

fn pushSymbol(cx: *const Cx, node_index: u32) !void {
    switch (try lookupSymbol(cx, node_index)) {
        .variable => |v| try pushVar(cx, v),
        .constant => |i| try pushInt(cx, i),
    }
}

fn pushVar(cx: *const Cx, variable: lang.Variable) !void {
    try emitOpcodeByName(cx, "push-var");
    try emitVarNumber(cx, variable);
}

fn emitVariable(cx: *const Cx, node_index: u32) !void {
    const symbol = try lookupSymbol(cx, node_index);
    if (symbol != .variable) return error.BadData;
    try emitVarNumber(cx, symbol.variable);
}

fn emitVarNumber(cx: *const Cx, variable: lang.Variable) !void {
    try cx.out.writer(cx.gpa).writeInt(u16, variable.raw, .little);
}

fn lookupSymbol(cx: *const Cx, node_index: Ast.NodeIndex) !script.Symbol {
    const expr = &cx.ast.nodes.items[node_index];
    if (expr.* != .identifier) return error.BadData;
    const name = expr.identifier;

    for (cx.local_vars.slice(), 0..) |local_name, num_usize| {
        const num: u14 = @intCast(num_usize);
        if (local_name) |n|
            if (std.mem.eql(u8, name, n))
                return .{ .variable = .init2(.local, num) };
    }
    if (cx.room_scope.get(name)) |sym| return sym;
    if (cx.project_scope.get(name)) |sym| return sym;

    // Not found, return an error
    const token_index = cx.ast.node_tokens.items[node_index];
    const loc = cx.lex.tokens.items[token_index].span.start;
    cx.diag.err(loc, "name not found", .{});
    return error.AddedToDiagnostic;
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
