const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const games = @import("games.zig");
const lang = @import("lang.zig");
const Precedence = @import("parser.zig").Precedence;
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    symbols: *const Symbols,
    bytecode: []const u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    // This code uses u16 all over the place. Make sure everything will fit.
    // Leave an extra byte so the end of a slice is representable as 0xffff.
    if (bytecode.len > 0xfffe) return error.BadData;

    const language = lang.buildLanguage(symbols.game);

    var basic_blocks = try scanBasicBlocks(gpa, &language, bytecode);
    defer basic_blocks.deinit(gpa);

    var dcx: DecompileCx = .{
        .gpa = gpa,
        .diag = diag,
        .language = &language,
        .basic_blocks = basic_blocks.items,

        .pending_basic_blocks = .empty,
        .stack = .{},
        .str_stack = .{},
        .stmts = .empty,
        .exprs = .empty,
        .extra = .empty,
    };
    defer dcx.extra.deinit(gpa);
    defer dcx.exprs.deinit(gpa);
    defer dcx.stmts.deinit(gpa);
    defer dcx.pending_basic_blocks.deinit(gpa);

    try decompileBasicBlocks(&dcx, bytecode);

    var scx: StructuringCx = .{
        .gpa = gpa,
        .stmts = .init(dcx.stmts.items),
        .queue = .empty,
        .nodes = .empty,
    };
    defer scx.nodes.deinit(gpa);
    defer scx.queue.deinit(gpa);
    try structure(&scx, basic_blocks.items);

    var jump_targets = try findJumpTargets(gpa, .init(scx.nodes.items));
    defer jump_targets.deinit(gpa);

    var ecx: EmitCx = .{
        .gpa = gpa,
        .symbols = symbols,
        .nodes = .init(scx.nodes.items),
        .stmts = .init(dcx.stmts.items),
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .jump_targets = jump_targets.items,
        .out = out,
        .indent = indent_size * 1,
    };
    try emitNodeList(&ecx, root_node_index);
}

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
            .@"jump-if" => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{
                    .jump_if = .{ .target = target },
                });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
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
            else => {
                std.debug.assert(ins.operands.len == 0 or ins.operands.get(0) != .relative_offset);
            },
        };
    }
    try result.append(gpa, .{
        .end = @intCast(bytecode.len),
        .exit = .no_jump,
        .state = .new,
        .stack_on_enter = .undef,
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
    // A basic block ending at 0 would have been a zero-length block
    if (end == 0) return;

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
            .state = .new,
            .stack_on_enter = .undef,
            .statements = .undef,
        });
    }
}

const BasicBlockIndex = u16;

const BasicBlock = struct {
    end: u16,
    exit: BasicBlockExit,
    state: enum { new, pending, decompiled },
    /// valid in {pending,decompiled}
    stack_on_enter: utils.SafeUndefined(std.BoundedArray(ExprIndex, 1)),
    /// valid in {decompiled}
    statements: utils.SafeUndefined(ExtraSlice),

    fn order(end: u16, item: BasicBlock) std.math.Order {
        return std.math.order(end, item.end);
    }
};

const BasicBlockExit = union(enum) {
    no_jump,
    jump: struct { target: u16 },
    jump_if: JumpTarget,
    jump_unless: JumpTarget,
};

const JumpTarget = struct {
    target: u16,
};

const DecompileCx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    language: *const lang.Language,
    basic_blocks: []BasicBlock,

    pending_basic_blocks: std.ArrayListUnmanaged(u16),
    stack: std.BoundedArray(ExprIndex, 16),
    str_stack: std.BoundedArray(ExprIndex, 2),
    stmts: std.ArrayListUnmanaged(Stmt),
    exprs: std.ArrayListUnmanaged(Expr),
    extra: std.ArrayListUnmanaged(ExprIndex),
};

const Stmt = union(enum) {
    jump_if: JumpTargetAndCondition,
    jump_unless: JumpTargetAndCondition,
    jump: struct { target: u16 },
    call: struct { op: lang.Op, args: ExtraSlice },
};

const JumpTargetAndCondition = struct {
    target: u16,
    condition: ExprIndex,
};

const ExprIndex = u16;

const Expr = union(enum) {
    int: i32,
    string: []const u8,
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
    push32,
    push_var,
    push_str,
    dup,
    pop,
    jump_if,
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
    string,
    list,
};

const ops: std.EnumArray(lang.Op, Op) = initEnumArrayFixed(lang.Op, Op, .{
    .@"push-u8" = .push8,
    .@"push-i16" = .push16,
    .@"push-i32" = .push32,
    .@"push-var" = .push_var,
    .@"push-str" = .push_str,
    .@"get-array-item" = .genCall(&.{.int}),
    .@"get-array-item-2d" = .genCall(&.{ .int, .int }),
    .dup = .dup,
    .not = .genCall(&.{.int}),
    .eq = .genCall(&.{ .int, .int }),
    .ne = .genCall(&.{ .int, .int }),
    .gt = .genCall(&.{ .int, .int }),
    .lt = .genCall(&.{ .int, .int }),
    .le = .genCall(&.{ .int, .int }),
    .ge = .genCall(&.{ .int, .int }),
    .add = .genCall(&.{ .int, .int }),
    .sub = .genCall(&.{ .int, .int }),
    .mul = .genCall(&.{ .int, .int }),
    .div = .genCall(&.{ .int, .int }),
    .land = .genCall(&.{ .int, .int }),
    .lor = .genCall(&.{ .int, .int }),
    .pop = .pop,
    .@"image-set-width" = .gen(&.{.int}),
    .@"image-set-height" = .gen(&.{.int}),
    .@"image-draw" = .gen(&.{}),
    .@"image-select" = .gen(&.{.int}),
    .@"image-set-pos" = .gen(&.{ .int, .int }),
    .@"image-set-palette" = .gen(&.{.int}),
    .@"image-set-shadow" = .gen(&.{.int}),
    .@"image-set-draw-box" = .gen(&.{ .int, .int, .int, .int, .int }),
    .@"image-set-render-image" = .gen(&.{.int}),
    .@"image-new" = .gen(&.{}),
    .@"image-commit" = .gen(&.{}),
    .max = .genCall(&.{ .int, .int }),
    .@"line-length-2d" = .genCall(&.{ .int, .int, .int, .int }),
    .@"sprite-get-state" = .genCall(&.{.int}),
    .@"sprite-get-variable" = .genCall(&.{ .int, .int }),
    .@"sprite-set-state" = .gen(&.{.int}),
    .@"sprite-select-range" = .gen(&.{ .int, .int }),
    .@"sprite-set-image" = .gen(&.{.int}),
    .@"sprite-new" = .gen(&.{}),
    .@"sprite-group-select" = .gen(&.{.int}),
    .@"sprite-group-new" = .gen(&.{}),
    .@"image-get-width" = .genCall(&.{ .int, .int }),
    .@"image-get-height" = .genCall(&.{ .int, .int }),
    .@"actor-get-property" = .genCall(&.{ .int, .int, .int }),
    .@"start-script-order" = .gen(&.{ .int, .int, .list }),
    .mod = .genCall(&.{ .int, .int }),
    .shl = .genCall(&.{ .int, .int }),
    .shr = .genCall(&.{ .int, .int }),
    .iif = .genCall(&.{ .int, .int, .int }),
    .@"dim-array-range.int16" = .gen(&.{ .int, .int, .int, .int, .int }),
    .set = .gen(&.{.int}),
    .@"set-array-item" = .gen(&.{ .int, .int }),
    .@"set-array-item-2d" = .gen(&.{ .int, .int, .int }),
    .@"read-ini-int" = .genCall(&.{ .int, .string, .string }),
    .inc = .gen(&.{}),
    .@"inc-array-item" = .gen(&.{.int}),
    .dec = .gen(&.{}),
    .@"dec-array-item" = .gen(&.{.int}),
    .@"jump-if" = .jump_if,
    .@"jump-unless" = .jump_unless,
    .@"start-script" = .gen(&.{ .int, .list }),
    .@"start-script-rec" = .gen(&.{ .int, .list }),
    .@"start-object" = .gen(&.{ .int, .int, .list }),
    .@"array-get-dim" = .genCall(&.{}),
    .@"array-get-height" = .genCall(&.{}),
    .@"array-get-width" = .genCall(&.{}),
    .end2 = .gen(&.{}),
    .end = .gen(&.{}),
    .@"window-select" = .gen(&.{.int}),
    .@"window-set-image" = .gen(&.{.int}),
    .@"window-new" = .gen(&.{}),
    .@"window-commit" = .gen(&.{}),
    .@"cursor-on" = .gen(&.{}),
    .@"break-here" = .gen(&.{}),
    .jump = .jump,
    .@"sound-channel" = .gen(&.{.int}),
    .@"sound-select" = .gen(&.{.int}),
    .@"sound-start" = .gen(&.{}),
    .@"stop-sound" = .gen(&.{.int}),
    .@"current-room" = .gen(&.{.int}),
    .@"stop-script" = .gen(&.{.int}),
    .@"put-actor" = .gen(&.{ .int, .int, .int, .int }),
    .@"do-animation" = .gen(&.{ .int, .int }),
    .random = .genCall(&.{.int}),
    .@"random-between" = .genCall(&.{ .int, .int }),
    .@"script-running" = .genCall(&.{.int}),
    .@"palette-color" = .genCall(&.{ .int, .int }),
    .@"sound-running" = .genCall(&.{.int}),
    .@"unlock-costume" = .gen(&.{.int}),
    .@"nuke-image" = .gen(&.{.int}),
    .@"palette-select" = .gen(&.{.int}),
    .@"palette-from-image" = .gen(&.{ .int, .int }),
    .@"palette-new" = .gen(&.{}),
    .@"palette-commit" = .gen(&.{}),
    .@"assign-string" = .gen(&.{.string}),
    .@"array-assign-list" = .gen(&.{ .int, .int, .int, .int, .list }),
    .@"array-assign-slice" = .gen(&.{ .int, .int, .int, .int, .int, .int, .int, .int }),
    .@"array-assign-range" = .gen(&.{ .int, .int, .int, .int, .int, .int }),
    .sprintf = .gen(&.{ .string, .int, .list }),
    .debug = .gen(&.{.int}),
    .in = .genCall(&.{ .int, .list }),
    .@"sleep-for-seconds" = .gen(&.{.int}),
    .@"stop-sentence" = .gen(&.{}),
    .@"print-debug-string" = .gen(&.{}),
    .@"print-debug-printf" = .gen(&.{ .int, .list }),
    .@"print-debug-start" = .gen(&.{}),
    .@"print-system-string" = .gen(&.{}),
    .@"print-system-start" = .gen(&.{}),
    .@"dim-array.int8" = .gen(&.{.int}),
    .@"dim-array.int16" = .gen(&.{.int}),
    .@"dim-array.int32" = .gen(&.{.int}),
    .undim = .gen(&.{}),
    .@"return" = .gen(&.{.int}),
    .@"call-script" = .genCall(&.{ .int, .list }),
    .@"dim-array-2d.int8" = .gen(&.{ .int, .int }),
    .@"dim-array-2d.int16" = .gen(&.{ .int, .int }),
    .abs = .genCall(&.{.int}),
    .@"kludge-call" = .genCall(&.{.list}),
    .kludge = .gen(&.{.list}),
    .@"break-here-multi" = .gen(&.{.int}),
    .pick = .genCall(&.{ .int, .list }),
    .@"actor-get-var" = .genCall(&.{ .int, .int }),
    .@"chain-script" = .gen(&.{ .int, .list }),
    .band = .genCall(&.{ .int, .int }),
    .bor = .genCall(&.{ .int, .int }),
    .@"delete-file" = .gen(&.{.string}),
    .localize = .gen(&.{.int}),
    .@"string-length" = .genCall(&.{.int}),
    .@"string-substr" = .genCall(&.{ .int, .int, .int }),
    .@"read-system-ini-int" = .genCall(&.{.string}),
    .@"delete-polygon" = .gen(&.{ .int, .int }),
});

fn initEnumArrayFixed(E: type, V: type, values: std.enums.EnumFieldStruct(E, V, null)) std.EnumArray(E, V) {
    @setEvalBranchQuota(2000);
    return .init(values);
}

fn decompileBasicBlocks(cx: *DecompileCx, bytecode: []const u8) !void {
    try scheduleBasicBlock(cx, 0);
    while (cx.pending_basic_blocks.pop()) |bbi| {
        const bb = &cx.basic_blocks[bbi];
        const bb_start = if (bbi == 0) 0 else cx.basic_blocks[bbi - 1].end;

        std.debug.assert(cx.stack.len == 0);
        cx.stack.appendSlice(bb.stack_on_enter.defined.slice()) catch unreachable;

        const first_stmt: u16 = @intCast(cx.stmts.items.len);

        var disasm: lang.Disasm = .init(cx.language, bytecode[0..bb.end]);
        disasm.reader.pos = bb_start;
        while (try disasm.next()) |ins|
            try decompileIns(cx, ins);

        const num_stmts = @as(u16, @intCast(cx.stmts.items.len)) - first_stmt;
        bb.statements.setOnce(.{ .start = first_stmt, .len = num_stmts });

        switch (bb.exit) {
            .no_jump => {
                if (bbi != cx.basic_blocks.len - 1) {
                    // middle blocks fall through to the next block
                    try scheduleBasicBlock(cx, bbi + 1);
                } else {
                    // the last block should finish with an empty stack
                    if (cx.stack.len != 0) return error.BadData;
                }
            },
            .jump => |j| {
                const target_bbi = findBasicBlockWithStart(cx, j.target);
                try scheduleBasicBlock(cx, target_bbi);
            },
            .jump_if, .jump_unless => |j| {
                if (bbi == cx.basic_blocks.len - 1) // should never happen, given well-formed input
                    return error.BadData;
                try scheduleBasicBlock(cx, bbi + 1);
                const target_bbi = findBasicBlockWithStart(cx, j.target);
                try scheduleBasicBlock(cx, target_bbi);
            },
        }

        // Above, the stack was dealt with, so we no longer need it. Clear it for the next block.
        cx.stack.clear();
        // However the string stack wasn't, so make sure it ended up empty.
        if (cx.str_stack.len != 0) return error.BadData;
    }

    // Bail if any basic blocks were unreachable and not decompiled. If this comes up I'll fix it
    for (cx.basic_blocks) |*bb|
        if (bb.state == .new)
            return error.BadData;
}

fn findBasicBlockWithStart(cx: *const DecompileCx, start: u16) BasicBlockIndex {
    if (start == 0) return 0;
    // We're storing the end, not the start, so search for the end of one block
    // which will also be the beginning of the next.
    const bbi_usize = std.sort.binarySearch(BasicBlock, cx.basic_blocks, start, BasicBlock.order).?;
    const bbi: BasicBlockIndex = @intCast(bbi_usize);
    return bbi + 1;
}

fn scheduleBasicBlock(cx: *DecompileCx, bbi: u16) !void {
    const bb = &cx.basic_blocks[bbi];
    if (bb.state != .new) {
        // Make sure the stack is identical on all entrances to each basic block
        if (!std.mem.eql(ExprIndex, bb.stack_on_enter.defined.slice(), cx.stack.slice()))
            return error.BadData;
        return;
    }
    bb.state = .pending;
    bb.stack_on_enter.setOnce(try .fromSlice(cx.stack.slice()));
    try cx.pending_basic_blocks.append(cx.gpa, bbi);
}

fn decompileIns(cx: *DecompileCx, ins: lang.Ins) !void {
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
        .push32 => {
            try push(cx, .{ .int = ins.operands.get(0).i32 });
        },
        .push_var => {
            try push(cx, .{ .variable = ins.operands.get(0).variable });
        },
        .push_str => {
            const ei = try storeExpr(cx, .{ .string = ins.operands.get(0).string });
            try cx.str_stack.append(ei);
        },
        .dup, .pop => {
            return error.BadData; // TODO
        },
        .jump_if => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.addUnsignedSigned(ins.end, rel).?;
            const condition = try pop(cx);
            try cx.stmts.append(cx.gpa, .{ .jump_if = .{
                .target = target,
                .condition = condition,
            } });
        },
        .jump_unless => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.addUnsignedSigned(ins.end, rel).?;
            const condition = try pop(cx);
            try cx.stmts.append(cx.gpa, .{ .jump_unless = .{
                .target = target,
                .condition = condition,
            } });
        },
        .jump => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.addUnsignedSigned(ins.end, rel).?;
            try cx.stmts.append(cx.gpa, .{ .jump = .{ .target = target } });
        },
        .generic => |gen| {
            var args: std.BoundedArray(ExprIndex, lang.max_operands + max_params) = .{};
            for (ins.operands.slice()) |operand| {
                const expr: Expr = switch (operand) {
                    .variable => |v| .{ .variable = v },
                    .string => |s| .{ .string = s },
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
                    .string => try popString(cx),
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

fn push(cx: *DecompileCx, expr: Expr) !void {
    const ei = try storeExpr(cx, expr);
    try cx.stack.append(ei);
}

fn pop(cx: *DecompileCx) !ExprIndex {
    return cx.stack.pop() orelse return error.BadData;
}

fn popString(cx: *DecompileCx) !ExprIndex {
    const ei = try pop(cx);
    const expr = &cx.exprs.items[ei];
    if (expr.* == .int and expr.int == -1)
        return cx.str_stack.pop() orelse return error.BadData;
    if (expr.* == .variable)
        return ei;
    return error.BadData;
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

    queue: std.ArrayListUnmanaged(NodeIndex),
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
        false: NodeIndex,
    },
};

const root_node_index = 0;

fn structure(cx: *StructuringCx, basic_blocks: []const BasicBlock) !void {
    // Populate initial nodes, just every basic block one after the other
    const bb_nodes = try cx.nodes.addManyAsSlice(cx.gpa, basic_blocks.len);
    for (bb_nodes, basic_blocks, 0..) |*node, *bb, bbi_usize| {
        const bbi: BasicBlockIndex = @intCast(bbi_usize);
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

    try cx.queue.append(cx.gpa, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntIf(cx, ni);
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

    var ni_false_end = null_node;
    const true_end = &cx.nodes.items[ni_true_end];
    if (true_end.kind == .basic_block and
        true_end.kind.basic_block.exit == .jump and
        true_end.kind.basic_block.exit.jump.target > true_end.end)
    {
        if (findNodeWithEnd(cx, true_end.next, true_end.kind.basic_block.exit.jump.target)) |n|
            ni_false_end = n;
    }

    if (ni_false_end == null_node)
        try makeIf(cx, ni, ni_true_end)
    else
        try makeIfElse(cx, ni, ni_true_end, ni_false_end);
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
            .false = null_node,
        } },
    });
    cx.nodes.items[ni_before].next = ni_if;
    cx.nodes.items[ni_true_end].next = null_node;

    try cx.queue.append(cx.gpa, cx.nodes.items[ni_if].next);
    try cx.queue.append(cx.gpa, cx.nodes.items[ni_if].kind.@"if".true);
}

fn makeIfElse(
    cx: *StructuringCx,
    ni_before: NodeIndex,
    ni_true_end: NodeIndex,
    ni_false_end: NodeIndex,
) !void {
    const condition = chopJumpUnless(cx, ni_before);
    chopJump(cx, ni_true_end);
    const ni_if = try appendNode(cx, .{
        .start = cx.nodes.items[ni_before].end,
        .end = cx.nodes.items[ni_false_end].end,
        .next = cx.nodes.items[ni_false_end].next,
        .kind = .{ .@"if" = .{
            .condition = condition,
            .true = cx.nodes.items[ni_before].next,
            .false = cx.nodes.items[ni_true_end].next,
        } },
    });
    cx.nodes.items[ni_before].next = ni_if;
    cx.nodes.items[ni_true_end].next = null_node;
    cx.nodes.items[ni_false_end].next = null_node;

    try cx.queue.append(cx.gpa, cx.nodes.items[ni_if].next);
    try cx.queue.append(cx.gpa, cx.nodes.items[ni_if].kind.@"if".true);
    try cx.queue.append(cx.gpa, cx.nodes.items[ni_if].kind.@"if".false);
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

const jump_len = 3;

fn chopJump(cx: *StructuringCx, ni: NodeIndex) void {
    const node = &cx.nodes.items[ni];
    node.end -= jump_len;
    node.kind.basic_block.exit = .no_jump;
    node.kind.basic_block.statements.len -= 1;
}

fn chopJumpUnless(cx: *StructuringCx, ni: NodeIndex) ExprIndex {
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
                .jump_if => |j| j.target,
                .jump_unless => |j| j.target,
            };
            try insertSortedNoDup(cx.gpa, &cx.result, target);
        },
        .@"if" => |s| {
            try findJumpTargetsInNodeList(cx, s.true);
            try findJumpTargetsInNodeList(cx, s.false);
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

const indent_size = 4;

const EmitCx = struct {
    gpa: std.mem.Allocator,
    symbols: *const Symbols,
    nodes: utils.SafeManyPointer([*]const Node),
    stmts: utils.SafeManyPointer([*]const Stmt),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    jump_targets: []const u16,

    out: *std.ArrayListUnmanaged(u8),
    indent: u16,
};

fn emitNodeList(cx: *EmitCx, ni_start: NodeIndex) error{ OutOfMemory, BadData }!void {
    var ni = ni_start;
    while (ni != null_node) {
        try emitSingleNode(cx, ni);
        ni = cx.nodes.getPtr(ni).next;
    }
}

fn emitSingleNode(cx: *EmitCx, ni: NodeIndex) !void {
    const node = cx.nodes.getPtr(ni);
    switch (node.kind) {
        .basic_block => |bb| {
            // TODO: keep track of list position instead of searching every time
            if (std.sort.binarySearch(u16, cx.jump_targets, node.start, orderU16) != null) {
                try writeIndent(cx);
                try emitLabel(cx, node.start);
                try cx.out.appendSlice(cx.gpa, ":\n");
            }

            for (cx.stmts.use()[bb.statements.start..][0..bb.statements.len]) |*stmt|
                try emitStmt(cx, stmt);
        },
        .@"if" => |k| {
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "if (");
            try emitExpr(cx, k.condition, .all);
            try cx.out.appendSlice(cx.gpa, ") {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, k.true);
            cx.indent -= indent_size;
            if (k.false != null_node) {
                try writeIndent(cx);
                try cx.out.appendSlice(cx.gpa, "} else {\n");
                cx.indent += indent_size;
                try emitNodeList(cx, k.false);
                cx.indent -= indent_size;
            }
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
    }
}

fn emitStmt(cx: *const EmitCx, stmt: *const Stmt) !void {
    try writeIndent(cx);
    switch (stmt.*) {
        .jump_if, .jump_unless => |j| {
            const op = switch (stmt.*) {
                .jump_if => @tagName(lang.Op.@"jump-if"),
                .jump_unless => @tagName(lang.Op.@"jump-unless"),
                else => unreachable,
            };
            try cx.out.appendSlice(cx.gpa, op);
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
        .string => |s| try cx.out.writer(cx.gpa).print("\"{s}\"", .{s}),
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
    switch (kind) {
        .global => {
            if (cx.symbols.globals.get(number)) |name|
                return try cx.out.appendSlice(cx.gpa, name);
        },
        .local => {}, // TODO
        .room => {}, // TODO
    }
    try cx.out.writer(cx.gpa).print("{s}{}", .{ @tagName(kind), number });
}

fn emitLabel(cx: *const EmitCx, pc: u16) !void {
    try cx.out.writer(cx.gpa).print("L{x:0>4}", .{pc});
}

fn writeIndent(cx: *const EmitCx) !void {
    const bytes = try cx.out.addManyAsSlice(cx.gpa, cx.indent);
    @memset(bytes, ' ');
}

fn getExtra(cx: *const EmitCx, slice: ExtraSlice) []const ExprIndex {
    return cx.extra.use()[slice.start..][0..slice.len];
}
