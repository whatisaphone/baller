const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const games = @import("games.zig");
const lang = @import("lang.zig");
const Precedence = @import("parser.zig").Precedence;
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    game: games.Game,
    bytecode: []const u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    // This code uses u16 all over the place. Make sure everything will fit.
    // Leave an extra byte so the end of a slice is representable as 0xffff.
    if (bytecode.len > 0xfffe) return error.BadData;

    const language = lang.buildLanguage(game);

    var basic_blocks = try scanBasicBlocks(gpa, &language, bytecode);
    defer basic_blocks.deinit(gpa);

    var dcx: DecompileCx = .{
        .gpa = gpa,
        .diag = diag,
        .language = &language,
        .stack = .{},
        .stmts = .empty,
        .exprs = .empty,
        .extra = .empty,
    };
    defer dcx.extra.deinit(gpa);
    defer dcx.exprs.deinit(gpa);
    defer dcx.stmts.deinit(gpa);

    for (basic_blocks.items, 0..) |*bb, i| {
        const bb_start = if (i == 0) 0 else basic_blocks.items[i - 1].end;
        try decompile(&dcx, bytecode, bb, bb_start);
    }

    var scx: StructuringCx = .{
        .gpa = gpa,
        .stmts = .init(dcx.stmts.items),
        .nodes = .empty,
    };
    defer scx.nodes.deinit(gpa);
    try structure(&scx, basic_blocks.items);

    var jump_targets = try findJumpTargets(gpa, .init(scx.nodes.items));
    defer jump_targets.deinit(gpa);

    var ecx: EmitCx = .{
        .gpa = gpa,
        .nodes = .init(scx.nodes.items),
        .stmts = .init(dcx.stmts.items),
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .jump_targets = jump_targets.items,
        .out = out,
    };
    try emitNodeList(&ecx, root_node_index);
}

const DecompileCx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    language: *const lang.Language,

    stack: std.BoundedArray(ExprIndex, 16),
    stmts: std.ArrayListUnmanaged(Stmt),
    exprs: std.ArrayListUnmanaged(Expr),
    extra: std.ArrayListUnmanaged(ExprIndex),
};

fn scanBasicBlocks(
    gpa: std.mem.Allocator,
    language: *const lang.Language,
    bytecode: []const u8,
) !std.ArrayListUnmanaged(BasicBlock) {
    var result: std.ArrayListUnmanaged(BasicBlock) = .empty;
    errdefer result.deinit(gpa);

    var disasm: lang.Disasm = .init(language, bytecode);
    while (try disasm.next()) |ins| {
        if (ins.name == .op) switch (ins.name.op) {
            .@"jump-unless" => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{
                    .jump_unless = .{ .target = target },
                });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
            .jump => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{
                    .jump = .{ .target = target },
                });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
            else => {},
        };
    }
    try result.append(gpa, .{
        .end = @intCast(bytecode.len),
        .exit = .no_jump,
        .statements = .undef,
    });
    return result;
}

fn jumpTarget(ins: *const lang.Ins, bytecode_len: u16) !u16 {
    const rel = ins.operands.get(0).relative_offset;
    const target = utils.addUnsignedSigned(ins.end, rel) orelse return error.BadData;
    if (target >= bytecode_len) return error.BadData;
    return target;
}

fn insertBasicBlock(
    gpa: std.mem.Allocator,
    basic_blocks: *std.ArrayListUnmanaged(BasicBlock),
    end: u16,
    exit: BasicBlockExit,
) !void {
    const index = std.sort.lowerBound(BasicBlock, basic_blocks.items, end, BasicBlock.order);
    if (index != basic_blocks.items.len and basic_blocks.items[index].end == end) {
        // If there's a dup, update the exit
        if (exit != .no_jump) {
            std.debug.assert(basic_blocks.items[index].exit == .no_jump);
            basic_blocks.items[index].exit = exit;
        }
    } else {
        // Otherwise insert the new block
        try basic_blocks.insert(gpa, index, .{
            .end = end,
            .exit = exit,
            .statements = .undef,
        });
    }
}

const BasicBlock = struct {
    end: u16,
    exit: BasicBlockExit,
    statements: utils.SafeUndefined(ExtraSlice),

    fn order(end: u16, item: BasicBlock) std.math.Order {
        return std.math.order(end, item.end);
    }
};

const BasicBlockExit = union(enum) {
    no_jump,
    jump: struct { target: u16 },
    jump_unless: struct { target: u16 },
};

const Stmt = union(enum) {
    jump_unless: struct { target: u16, condition: ExprIndex },
    jump: struct { target: u16 },
    call: struct { op: lang.Op, args: ExtraSlice },
};

const ExprIndex = u16;

const Expr = union(enum) {
    int: i32,
    variable: lang.Variable,
    call: struct { op: lang.Op, args: ExtraSlice },
    list: struct { items: ExtraSlice },
};

const ExtraSlice = struct {
    start: u16,
    len: u16,
};

const Op = union(enum) {
    push8,
    push16,
    push_var,
    jump_unless,
    jump,
    generic: struct {
        call: bool,
        params: std.BoundedArray(Param, max_params),
    },

    fn gen(params: []const Param) Op {
        return .{ .generic = .{
            .call = false,
            .params = std.BoundedArray(Param, max_params).fromSlice(params) catch unreachable,
        } };
    }

    fn genCall(params: []const Param) Op {
        return .{ .generic = .{
            .call = true,
            .params = std.BoundedArray(Param, max_params).fromSlice(params) catch unreachable,
        } };
    }
};

const max_params = 8;

const Param = union(enum) {
    int,
    list,
};

const ops: std.EnumArray(lang.Op, Op) = .init(.{
    .@"push-u8" = .push8,
    .@"push-i16" = .push16,
    .@"push-var" = .push_var,
    .@"get-array-item" = .genCall(&.{.int}),
    .@"get-array-item-2d" = .genCall(&.{ .int, .int }),
    .add = .genCall(&.{ .int, .int }),
    .sub = .genCall(&.{ .int, .int }),
    .mul = .genCall(&.{ .int, .int }),
    .@"dim-array-range.int16" = .gen(&.{ .int, .int, .int, .int, .int }),
    .set = .gen(&.{.int}),
    .@"set-array-item" = .gen(&.{ .int, .int }),
    .@"set-array-item-2d" = .gen(&.{ .int, .int, .int }),
    .inc = .gen(&.{}),
    .dec = .gen(&.{}),
    .@"jump-unless" = .jump_unless,
    .@"start-script" = .gen(&.{ .int, .list }),
    .@"start-script-rec" = .gen(&.{ .int, .list }),
    .@"start-object" = .gen(&.{ .int, .int, .list }),
    .@"array-get-height" = .genCall(&.{}),
    .@"array-get-width" = .genCall(&.{}),
    .end2 = .gen(&.{}),
    .end = .gen(&.{}),
    .@"window-select" = .gen(&.{.int}),
    .@"window-set-image" = .gen(&.{.int}),
    .@"window-commit" = .gen(&.{}),
    .@"cursor-on" = .gen(&.{}),
    .@"break-here" = .gen(&.{}),
    .jump = .jump,
    .@"current-room" = .gen(&.{.int}),
    .@"stop-script" = .gen(&.{.int}),
    .random = .genCall(&.{.int}),
    .@"array-assign-slice" = .gen(&.{ .int, .int, .int, .int, .int, .int, .int, .int }),
    .debug = .gen(&.{.int}),
    .@"sleep-for-seconds" = .gen(&.{.int}),
    .@"stop-sentence" = .gen(&.{}),
    .@"dim-array.int8" = .gen(&.{.int}),
    .@"dim-array.int16" = .gen(&.{.int}),
    .undim = .gen(&.{}),
    .@"return" = .gen(&.{.int}),
    .@"call-script" = .genCall(&.{ .int, .list }),
    .@"dim-array-2d.int8" = .gen(&.{ .int, .int }),
    .@"dim-array-2d.int16" = .gen(&.{ .int, .int }),
    .@"kludge-call" = .genCall(&.{.list}),
    .kludge = .gen(&.{.list}),
    .@"break-here-multi" = .gen(&.{.int}),
    .@"actor-get-var" = .genCall(&.{ .int, .int }),
    .@"chain-script" = .gen(&.{ .int, .list }),
});

fn decompile(cx: *DecompileCx, bytecode: []const u8, bb: *BasicBlock, bb_start: u16) !void {
    const first_stmt: u16 = @intCast(cx.stmts.items.len);

    var disasm: lang.Disasm = .init(cx.language, bytecode[0..bb.end]);
    disasm.reader.pos = bb_start;
    while (try disasm.next()) |ins| {
        const op = switch (ins.name) {
            .op => |op| op,
            .str => |s| {
                cx.diag.err(ins.start, "unhandled opcode {s}", .{s});
                return error.AddedToDiagnostic;
            },
        };
        switch (ops.get(op)) {
            .push8 => {
                try push(cx, .{ .int = ins.operands.get(0).u8 });
            },
            .push16 => {
                try push(cx, .{ .int = ins.operands.get(0).i16 });
            },
            .push_var => {
                try push(cx, .{ .variable = ins.operands.get(0).variable });
            },
            .jump_unless => {
                const rel = ins.operands.get(0).relative_offset;
                const target = utils.addUnsignedSigned(ins.end, rel) orelse unreachable;
                const condition = try pop(cx);
                try cx.stmts.append(cx.gpa, .{ .jump_unless = .{
                    .target = target,
                    .condition = condition,
                } });
            },
            .jump => {
                const rel = ins.operands.get(0).relative_offset;
                const target = utils.addUnsignedSigned(ins.end, rel) orelse unreachable;
                try cx.stmts.append(cx.gpa, .{ .jump = .{ .target = target } });
            },
            .generic => |gen| {
                var args: std.BoundedArray(ExprIndex, lang.max_operands + max_params) = .{};
                for (ins.operands.slice()) |operand| {
                    const expr: Expr = switch (operand) {
                        .variable => |v| .{ .variable = v },
                        else => unreachable,
                    };
                    const ei = try storeExpr(cx, expr);
                    args.appendAssumeCapacity(ei);
                }

                // Pop args in reverse order
                var pi = gen.params.len;
                while (pi > 0) {
                    pi -= 1;
                    const param = gen.params.get(pi);
                    const ei = switch (param) {
                        .int => try pop(cx),
                        .list => try popList(cx),
                    };
                    args.buffer[args.len + pi] = ei;
                }
                args.len += gen.params.len;

                const args_extra = try storeExtra(cx, args.slice());
                if (gen.call) {
                    try push(cx, .{ .call = .{ .op = op, .args = args_extra } });
                } else {
                    try cx.stmts.append(cx.gpa, .{ .call = .{ .op = op, .args = args_extra } });
                }
            },
        }
    }

    const num_stmts = @as(u16, @intCast(cx.stmts.items.len)) - first_stmt;
    bb.statements.setOnce(.{ .start = first_stmt, .len = num_stmts });
}

fn push(cx: *DecompileCx, expr: Expr) !void {
    const ei = try storeExpr(cx, expr);
    try cx.stack.append(ei);
}

fn pop(cx: *DecompileCx) !ExprIndex {
    return cx.stack.pop() orelse return error.BadData;
}

fn popList(cx: *DecompileCx) !ExprIndex {
    const len_ei = try pop(cx);
    const len_expr = &cx.exprs.items[len_ei];
    if (len_expr.* != .int) return error.BadData;
    // TODO: think about the actual maximum here
    const len = std.math.cast(u8, len_expr.int) orelse return error.BadData;

    if (len > cx.stack.len) return error.BadData;
    const items = cx.stack.slice()[cx.stack.len - len ..];
    cx.stack.len -= len;

    return storeExpr(cx, .{ .list = .{
        .items = try storeExtra(cx, items),
    } });
}

fn storeExpr(cx: *DecompileCx, expr: Expr) !ExprIndex {
    const ei: ExprIndex = @intCast(cx.exprs.items.len);
    try cx.exprs.append(cx.gpa, expr);
    return ei;
}

fn storeExtra(cx: *DecompileCx, items: []const ExprIndex) !ExtraSlice {
    const start: u16 = @intCast(cx.extra.items.len);
    const len: u16 = @intCast(items.len);
    try cx.extra.appendSlice(cx.gpa, items);
    return .{ .start = start, .len = len };
}

const StructuringCx = struct {
    gpa: std.mem.Allocator,
    stmts: utils.SafeManyPointer([*]const Stmt),

    nodes: std.ArrayListUnmanaged(Node),
};

const NodeIndex = u16;

const null_node: NodeIndex = 0xffff;

const Node = struct {
    start: u16,
    end: u16,
    next: NodeIndex,
    kind: NodeKind,
};

const NodeKind = union(enum) {
    basic_block: struct {
        exit: BasicBlockExit,
        statements: ExtraSlice,
    },
    @"if": struct {
        condition: ExprIndex,
        true: NodeIndex,
    },
};

const root_node_index = 0;

fn structure(cx: *StructuringCx, basic_blocks: []const BasicBlock) !void {
    // Populate initial nodes, just every basic block one after the other
    const bb_nodes = try cx.nodes.addManyAsSlice(cx.gpa, basic_blocks.len);
    for (bb_nodes, basic_blocks, 0..) |*node, *bb, bbi_usize| {
        const bbi: u16 = @intCast(bbi_usize);
        const bb_start = if (bbi == 0) 0 else basic_blocks[bbi - 1].end;
        const next = if (bbi == basic_blocks.len - 1) null_node else bbi + 1;
        node.* = .{
            .start = bb_start,
            .end = bb.end,
            .next = next,
            .kind = .{ .basic_block = .{
                .exit = bb.exit,
                .statements = bb.statements.defined,
            } },
        };
    }

    try huntIf(cx, root_node_index);
}

fn huntIf(cx: *StructuringCx, ni_first: NodeIndex) !void {
    var ni = ni_first;
    const ni_true_end = while (ni != null_node) {
        const node = &cx.nodes.items[ni];
        if (node.kind == .basic_block and
            node.kind.basic_block.exit == .jump_unless and
            node.kind.basic_block.exit.jump_unless.target > node.end)
            if (findNodeWithEnd(cx, node.next, node.kind.basic_block.exit.jump_unless.target)) |n|
                break n;
        ni = node.next;
    } else return;

    try makeIf(cx, ni, ni_true_end);
}

fn makeIf(cx: *StructuringCx, ni_before: NodeIndex, ni_true_end: NodeIndex) !void {
    const condition = chopJumpUnless(cx, ni_before);
    const ni_if = try appendNode(cx, .{
        .start = cx.nodes.items[ni_before].end,
        .end = cx.nodes.items[ni_true_end].end,
        .next = cx.nodes.items[ni_true_end].next,
        .kind = .{ .@"if" = .{
            .condition = condition,
            .true = cx.nodes.items[ni_before].next,
        } },
    });
    cx.nodes.items[ni_before].next = ni_if;
    cx.nodes.items[ni_true_end].next = null_node;
}

fn findNodeWithEnd(cx: *StructuringCx, ni_first: NodeIndex, end: u16) ?NodeIndex {
    var ni = ni_first;
    while (ni != null_node) {
        const node = &cx.nodes.items[ni];
        if (node.end == end) return ni;
        ni = node.next;
    }
    return null;
}

fn appendNode(cx: *StructuringCx, node: Node) !NodeIndex {
    const ni: NodeIndex = @intCast(cx.nodes.items.len);
    try cx.nodes.append(cx.gpa, node);
    return ni;
}

fn chopJumpUnless(cx: *StructuringCx, ni: NodeIndex) ExprIndex {
    const jump_len = 3;

    const node = &cx.nodes.items[ni];

    const ss = node.kind.basic_block.statements;
    const condition = cx.stmts.getPtr(ss.start + ss.len - 1).jump_unless.condition;

    node.end -= jump_len;
    node.kind.basic_block.exit = .no_jump;
    node.kind.basic_block.statements.len -= 1;
    return condition;
}

const FindJumpTargetsCx = struct {
    gpa: std.mem.Allocator,
    nodes: utils.SafeManyPointer([*]const Node),
    result: std.ArrayListUnmanaged(u16),
};

fn findJumpTargets(
    gpa: std.mem.Allocator,
    nodes: utils.SafeManyPointer([*]const Node),
) !std.ArrayListUnmanaged(u16) {
    var cx: FindJumpTargetsCx = .{
        .gpa = gpa,
        .nodes = nodes,
        .result = .empty,
    };
    errdefer cx.result.deinit(cx.gpa);

    try findJumpTargetsInNodeList(&cx, root_node_index);

    return cx.result;
}

fn findJumpTargetsInNodeList(cx: *FindJumpTargetsCx, ni_start: NodeIndex) error{OutOfMemory}!void {
    var ni = ni_start;
    while (ni != null_node) {
        try findJumpTargetsInSingleNode(cx, ni);
        ni = cx.nodes.getPtr(ni).next;
    }
}

fn findJumpTargetsInSingleNode(cx: *FindJumpTargetsCx, ni: NodeIndex) !void {
    const node = cx.nodes.getPtr(ni);
    switch (node.kind) {
        .basic_block => |*bb| {
            const target = switch (bb.exit) {
                .no_jump => return,
                .jump => |j| j.target,
                .jump_unless => |j| j.target,
            };
            try insertSortedNoDup(cx.gpa, &cx.result, target);
        },
        .@"if" => |s| {
            try findJumpTargetsInNodeList(cx, s.true);
        },
    }
}

fn insertSortedNoDup(gpa: std.mem.Allocator, xs: *std.ArrayListUnmanaged(u16), item: u16) !void {
    const index = std.sort.lowerBound(u16, xs.items, item, orderU16);
    if (index != xs.items.len and xs.items[index] == item)
        return;
    try xs.insert(gpa, index, item);
}

fn orderU16(a: u16, b: u16) std.math.Order {
    return std.math.order(a, b);
}

const EmitCx = struct {
    gpa: std.mem.Allocator,
    nodes: utils.SafeManyPointer([*]const Node),
    stmts: utils.SafeManyPointer([*]const Stmt),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    jump_targets: []const u16,

    out: *std.ArrayListUnmanaged(u8),
};

fn emitNodeList(cx: *const EmitCx, ni_start: NodeIndex) error{ OutOfMemory, BadData }!void {
    var ni = ni_start;
    while (ni != null_node) {
        try emitSingleNode(cx, ni);
        ni = cx.nodes.getPtr(ni).next;
    }
}

fn emitSingleNode(cx: *const EmitCx, ni: NodeIndex) !void {
    const node = cx.nodes.getPtr(ni);
    switch (node.kind) {
        .basic_block => |bb| {
            // TODO: keep track of list position instead of searching every time
            if (std.sort.binarySearch(u16, cx.jump_targets, node.start, orderU16) != null) {
                try emitLabel(cx, node.start);
                try cx.out.appendSlice(cx.gpa, ":\n");
            }

            for (cx.stmts.use()[bb.statements.start..][0..bb.statements.len]) |*stmt|
                try emitStmt(cx, stmt);
        },
        .@"if" => |k| {
            try cx.out.appendSlice(cx.gpa, "    if (");
            try emitExpr(cx, k.condition, .all);
            try cx.out.appendSlice(cx.gpa, ") {\n");
            try emitNodeList(cx, k.true);
            try cx.out.appendSlice(cx.gpa, "    }\n");
        },
    }
}

fn emitStmt(cx: *const EmitCx, stmt: *const Stmt) !void {
    try cx.out.appendSlice(cx.gpa, "    ");
    switch (stmt.*) {
        .jump_unless => |j| {
            try cx.out.appendSlice(cx.gpa, @tagName(lang.Op.@"jump-unless"));
            try cx.out.append(cx.gpa, ' ');
            try emitLabel(cx, j.target);
            try cx.out.append(cx.gpa, ' ');
            try emitExpr(cx, j.condition, .space);
        },
        .jump => |j| {
            try cx.out.appendSlice(cx.gpa, @tagName(lang.Op.jump));
            try cx.out.append(cx.gpa, ' ');
            try emitLabel(cx, j.target);
        },
        .call => |call| try emitCall(cx, call.op, call.args),
    }
    try cx.out.append(cx.gpa, '\n');
}

fn emitExpr(
    cx: *const EmitCx,
    ei: ExprIndex,
    prec: Precedence,
) error{ OutOfMemory, BadData }!void {
    switch (cx.exprs.get(ei)) {
        .int => |int| try cx.out.writer(cx.gpa).print("{}", .{int}),
        .variable => |v| try emitVariable(cx, v),
        .call => |c| {
            if (@intFromEnum(prec) >= @intFromEnum(Precedence.space))
                try cx.out.append(cx.gpa, '(');
            try emitCall(cx, c.op, c.args);
            if (@intFromEnum(prec) >= @intFromEnum(Precedence.space))
                try cx.out.append(cx.gpa, ')');
        },
        .list => |list| {
            try cx.out.append(cx.gpa, '[');
            for (getExtra(cx, list.items), 0..) |e, i| {
                if (i != 0)
                    try cx.out.append(cx.gpa, ' ');
                try emitExpr(cx, e, .space);
            }
            try cx.out.append(cx.gpa, ']');
        },
    }
}

fn emitCall(cx: *const EmitCx, op: lang.Op, args: ExtraSlice) !void {
    try cx.out.appendSlice(cx.gpa, @tagName(op));
    for (getExtra(cx, args)) |ei| {
        try cx.out.append(cx.gpa, ' ');
        try emitExpr(cx, ei, .space);
    }
}

fn emitVariable(cx: *const EmitCx, variable: lang.Variable) !void {
    const kind, const number = try variable.decode2();
    try cx.out.writer(cx.gpa).print("{s}{}", .{ @tagName(kind), number });
}

fn emitLabel(cx: *const EmitCx, pc: u16) !void {
    try cx.out.writer(cx.gpa).print("L{x:0>4}", .{pc});
}

fn getExtra(cx: *const EmitCx, slice: ExtraSlice) []const ExprIndex {
    return cx.extra.use()[slice.start..][0..slice.len];
}
