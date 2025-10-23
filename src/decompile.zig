const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const UsageTracker = @import("UsageTracker.zig");
const ArrayMap = @import("array_map.zig").ArrayMap;
const BoundedArray = @import("bounded_array.zig").BoundedArray;
const Directory = @import("extract.zig").Directory;
const Index = @import("extract.zig").Index;
const games = @import("games.zig");
const lang = @import("lang.zig");
const langdef = @import("langdef.zig");
const Precedence = @import("parser.zig").Precedence;
const script = @import("script.zig");
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    vm: *const lang.Vm,
    op_map: *const std.EnumArray(lang.Op, Op),
    symbols: *const Symbols,
    annotate: bool,
    room_number: u8,
    id: Symbols.ScriptId,
    bytecode: []const u8,
    index: *const Index,
    /// Bitmask of which local scripts exist in the room. Any script in the mask
    /// is eligible to be emitted by name instead of by number.
    lsc_mask: *const UsageTracker.LocalScripts,
    out: *std.ArrayListUnmanaged(u8),
    indent: u16,
    usage: *UsageTracker,
) !void {
    // This code uses u16 all over the place. Make sure everything will fit.
    // Leave an extra byte so the end of a slice is representable as 0xffff.
    if (bytecode.len > 0xfffe) return error.BadData;

    var basic_blocks = try scanBasicBlocks(gpa, vm, bytecode);
    defer basic_blocks.deinit(gpa);

    var dcx: DecompileCx = .{
        .gpa = gpa,
        .diag = diag,
        .vm = vm,
        .op_map = op_map,
        .basic_blocks = basic_blocks.items,

        .pending_basic_blocks = .empty,
        .stack = .{},
        .str_stack = .{},
        .stmts = .empty,
        .stmt_ends = .empty,
        .exprs = .empty,
        .extra = .empty,
        .usage = usage,
    };
    defer dcx.extra.deinit(gpa);
    defer dcx.exprs.deinit(gpa);
    defer dcx.stmt_ends.deinit(gpa);
    defer dcx.stmts.deinit(gpa);
    defer dcx.pending_basic_blocks.deinit(gpa);
    try dcx.stmts.ensureTotalCapacity(gpa, bytecode.len / 8);
    try dcx.stmt_ends.ensureTotalCapacity(gpa, bytecode.len / 8);
    try dcx.exprs.ensureTotalCapacity(gpa, bytecode.len / 4);
    try dcx.extra.ensureTotalCapacity(gpa, bytecode.len / 4);

    try decompileBasicBlocks(&dcx, bytecode);

    var tcx: TypeCx = .{
        .op_map = op_map,
        .symbols = symbols,
        .room_number = room_number,
        .id = id,
        .basic_blocks = dcx.basic_blocks,
        .stmts = .init(dcx.stmts.items),
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .types = .empty,
    };
    defer tcx.types.deinit(gpa);
    try tcx.types.ensureTotalCapacityPrecise(gpa, dcx.exprs.items.len);
    recoverTypes(&tcx);

    peephole(&dcx);

    var scx: StructuringCx = .{
        .gpa = gpa,
        .stmts = .init(dcx.stmts.items),
        .stmt_ends = .init(dcx.stmt_ends.items),
        .exprs = .init(dcx.exprs.items),
        .extra = &dcx.extra,
        .queue = .empty,
        .nodes = .empty,
        .trail = if (builtin.mode == .Debug) .{},
    };
    defer if (builtin.mode == .Debug) scx.trail.deinit(gpa);
    defer scx.nodes.deinit(gpa);
    defer scx.queue.deinit(gpa);
    try scx.nodes.ensureTotalCapacity(gpa, bytecode.len / 16);
    try structure(&scx, basic_blocks.items);

    var jump_targets = try findJumpTargets(gpa, scx.nodes.items);
    defer jump_targets.deinit(gpa);

    var ecx: EmitCx = .{
        .gpa = gpa,
        .diag = diag,
        .symbols = symbols,
        .room_number = room_number,
        .id = id,
        .nodes = .init(scx.nodes.items),
        .stmts = .init(dcx.stmts.items),
        .stmt_ends = if (annotate) .init(dcx.stmt_ends.items) else null,
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .index = index,
        .lsc_mask = lsc_mask,
        .local_var_usage = &usage.local_vars,
        .types = &tcx.types,
        .jump_targets = jump_targets.items,
        .out = out,
        .indent = indent_size * indent,
    };
    try emitScript(&ecx);
}

fn scanBasicBlocks(
    gpa: std.mem.Allocator,
    vm: *const lang.Vm,
    bytecode: []const u8,
) !std.ArrayListUnmanaged(BasicBlock) {
    var result: std.ArrayListUnmanaged(BasicBlock) = .empty;
    errdefer result.deinit(gpa);
    try result.ensureTotalCapacity(gpa, bytecode.len / 16);

    var disasm: lang.Disasm = .init(vm, bytecode);
    while (try disasm.next()) |ins| {
        if (ins.op != .op) return error.BadData;
        switch (ins.op.op) {
            .@"jump-if" => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{ .jump_if = target });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
            .@"jump-unless" => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{ .jump_unless = target });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
            .jump => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{ .jump = target });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
            .override => {
                const target = try jumpTarget(&ins, @intCast(bytecode.len));
                try insertBasicBlock(gpa, &result, ins.end, .{ .override = target });
                try insertBasicBlock(gpa, &result, target, .no_jump);
            },
            else => {
                for (ins.operands.slice()) |*o|
                    std.debug.assert(o.* != .relative_offset);
            },
        }
    }
    try result.append(gpa, .{
        .end = @intCast(bytecode.len),
        .exit = .no_jump,
        .state = .new,
        .stack_on_enter = .undef,
        .stack_on_exit = .undef,
        .statements = .undef,
    });
    return result;
}

fn jumpTarget(ins: *const lang.Ins, bytecode_len: u16) !u16 {
    const rel = ins.operands.get(0).relative_offset;
    const target = utils.add(u16, ins.end, rel) orelse return error.BadData;
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
            .stack_on_exit = .undef,
            .statements = .undef,
        });
    }
}

const BasicBlockIndex = u16;

const BasicBlock = struct {
    end: u16,
    exit: BasicBlockExit,
    /// (`decompiled` is only used in asserts in debug builds)
    state: enum { new, pending, decompiled },
    /// valid in {pending,decompiled}
    stack_on_enter: utils.SafeUndefined(BoundedArray(ExprIndex, 1)),
    /// valid in {decompiled}
    stack_on_exit: utils.SafeUndefined(BoundedArray(ExprIndex, 1)),
    /// valid in {decompiled}
    statements: utils.SafeUndefined(ExtraSlice),

    fn order(end: u16, item: BasicBlock) std.math.Order {
        return std.math.order(end, item.end);
    }
};

const BasicBlockExit = union(enum) {
    no_jump,
    jump: u16,
    jump_if: u16,
    jump_unless: u16,
    override: u16,
};

const DecompileCx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    vm: *const lang.Vm,
    op_map: *const std.EnumArray(lang.Op, Op),
    basic_blocks: []BasicBlock,

    pending_basic_blocks: std.ArrayListUnmanaged(u16),
    stack: BoundedArray(ExprIndex, 80),
    str_stack: BoundedArray(ExprIndex, 3),
    stmts: std.ArrayListUnmanaged(Stmt),
    stmt_ends: std.ArrayListUnmanaged(u16),
    exprs: std.ArrayListUnmanaged(Expr),
    extra: std.ArrayListUnmanaged(ExprIndex),
    usage: *UsageTracker,
};

const Stmt = union(enum) {
    jump_if: JumpTargetAndCondition,
    jump_unless: JumpTargetAndCondition,
    jump: struct { target: u16 },
    override: struct { target: u16 },
    call: struct { op: lang.Op, args: ExtraSlice },
    compound: struct { op: script.Compound, args: ExtraSlice },
    binop_assign: struct { op: Ast.BinOp, args: ExtraSlice },
    tombstone,
};

const JumpTargetAndCondition = struct {
    target: u16,
    condition: ExprIndex,
};

const ExprIndex = u16;
const null_expr = 0xffff;

fn eiOpt(ei: ExprIndex) ?ExprIndex {
    if (ei == null_expr) return null;
    return ei;
}

const Expr = union(enum) {
    int: i32,
    string: []const u8,
    variable: lang.Variable,
    call: struct { op: lang.Op, args: ExtraSlice },
    list: ExtraSlice,
    variadic_list: ExtraSlice,
    dup: ExprIndex,
    stack_fault,
};

const ExtraSlice = struct {
    start: u16,
    len: u16,

    pub fn last(self: ExtraSlice) u16 {
        std.debug.assert(self.len != 0);
        return self.start + self.len - 1;
    }
};

pub const Op = union(enum) {
    push8,
    push16,
    push32,
    push_var,
    push_str,
    dup,
    dup_multi,
    jump_if,
    jump_unless,
    jump,
    override,
    generic: struct {
        call: bool,
        params: []const script.Param,
    },
    illegal,
};

pub fn buildOpMap(game: games.Game) std.EnumArray(lang.Op, Op) {
    var result: std.EnumArray(lang.Op, Op) = .initFill(.illegal);
    result.set(.@"push.u8", .push8);
    result.set(.@"push.i16", .push16);
    result.set(.@"push.i32", .push32);
    result.set(.@"push.var", .push_var);
    result.set(.@"push.string", .push_str);
    result.set(.@"dup.multi", .dup_multi);
    result.set(.dup, .dup);
    result.set(.@"jump-if", .jump_if);
    result.set(.@"jump-unless", .jump_unless);
    result.set(.jump, .jump);
    result.set(.override, .override);

    const target = game.target();
    for (langdef.calls) |c| {
        if (!(target.ge(c.target_min) and target.le(c.target_max))) continue;
        std.debug.assert(result.getPtrConst(c.op).* == .illegal);
        std.debug.assert(c.params.len <= script.max_params);
        result.set(c.op, .{ .generic = .{
            .call = c.call,
            .params = c.params,
        } });
    }

    return result;
}

fn decompileBasicBlocks(cx: *DecompileCx, bytecode: []const u8) !void {
    try scheduleBasicBlock(cx, 0, &.{});
    while (cx.pending_basic_blocks.pop()) |bbi|
        try decompileBasicBlock(cx, bytecode, bbi);

    // Check if any basic blocks were unreachable and not decompiled. If so,
    // handle the case where it's an infinite loop, otherwise fail.
    for (cx.basic_blocks, 0..) |*bb, bbi_usize| {
        const bbi: BasicBlockIndex = @intCast(bbi_usize);
        if (builtin.mode == .Debug)
            std.debug.assert(bb.state == .new or bb.state == .decompiled);
        if (bb.state != .new) continue;
        try scheduleBasicBlockWithAssumedStack(cx, bbi);
        // TODO: make this a tail call again once stage2 supports it
        return decompileBasicBlocks(cx, bytecode);
    }

    // Do some checks on the last basic block.
    const bb_last = &cx.basic_blocks[cx.basic_blocks.len - 1];
    // The stack should be empty by the end, otherwise some opcode params are
    // probably wrong.
    if (bb_last.stack_on_exit.defined.len != 0) return error.BadData;
    // It should end with `end` or `end2`. This is emitted implicitly by the
    // compiler, so don't output it explicitly here.
    const ss = bb_last.statements.defined;
    if (ss.len == 0) return error.BadData;
    const stmt = &cx.stmts.items[ss.last()];
    if (stmt.* != .call) return error.BadData;
    if (stmt.call.op != .end and stmt.call.op != .@"end-object") return error.BadData;
    bb_last.end -= 1;
    bb_last.statements.defined.len -= 1;
}

fn findBasicBlockWithStart(cx: *const DecompileCx, start: u16) BasicBlockIndex {
    if (start == 0) return 0;
    // We're storing the end, not the start, so search for the end of one block
    // which will also be the beginning of the next.
    const bbi_usize = std.sort.binarySearch(BasicBlock, cx.basic_blocks, start, BasicBlock.order).?;
    const bbi: BasicBlockIndex = @intCast(bbi_usize);
    return bbi + 1;
}

fn scheduleBasicBlock(cx: *DecompileCx, bbi: u16, stack: []const ExprIndex) !void {
    const bb = &cx.basic_blocks[bbi];
    if (bb.state != .new) {
        // Make sure the stack is identical on all entrances to each basic block
        if (!std.mem.eql(ExprIndex, bb.stack_on_enter.defined.slice(), stack))
            return error.BadData;
        return;
    }
    bb.state = .pending;
    bb.stack_on_enter.setOnce(try .fromSlice(stack));
    try cx.pending_basic_blocks.append(cx.gpa, bbi);
}

fn scheduleBasicBlockWithAssumedStack(cx: *DecompileCx, bbi: u16) !void {
    // If a basic block was never analyzed, it must be dead code, but analyze it
    // anyway and assume the stack is the same from the previous basic block.
    if (bbi == 0) return error.BadData;
    const bbi_prev = bbi - 1;
    const prev = &cx.basic_blocks[bbi_prev];
    try scheduleBasicBlock(cx, bbi, prev.stack_on_exit.defined.slice());
}

fn decompileBasicBlock(cx: *DecompileCx, bytecode: []const u8, bbi: u16) !void {
    const bb = &cx.basic_blocks[bbi];
    const bb_start = if (bbi == 0) 0 else cx.basic_blocks[bbi - 1].end;

    std.debug.assert(bb.state == .pending);

    std.debug.assert(cx.stack.len == 0);
    cx.stack.appendSlice(bb.stack_on_enter.defined.slice()) catch unreachable;

    const first_stmt: u16 = @intCast(cx.stmts.items.len);

    var disasm: lang.Disasm = .init(cx.vm, bytecode[0..bb.end]);
    disasm.reader.pos = bb_start;
    while (try disasm.next()) |ins|
        try decompileIns(cx, ins);

    const num_stmts = @as(u16, @intCast(cx.stmts.items.len)) - first_stmt;
    bb.statements.setOnce(.{ .start = first_stmt, .len = num_stmts });

    bb.stack_on_exit.setOnce(try .fromSlice(cx.stack.slice()));
    cx.stack.clear();
    const remaining_stack = bb.stack_on_exit.defined.slice();

    switch (bb.exit) {
        .no_jump => {
            if (bbi != cx.basic_blocks.len - 1) {
                // middle blocks fall through to the next block
                try scheduleBasicBlock(cx, bbi + 1, remaining_stack);
            } else {
                // the last block should finish with an empty stack
                if (cx.stack.len != 0) return error.BadData;
            }
        },
        .jump => |target| {
            const target_bbi = findBasicBlockWithStart(cx, target);
            try scheduleBasicBlock(cx, target_bbi, remaining_stack);
        },
        .jump_if, .jump_unless, .override => |target| {
            if (bbi == cx.basic_blocks.len - 1) // should never happen, given well-formed input
                return error.BadData;
            try scheduleBasicBlock(cx, bbi + 1, remaining_stack);
            const target_bbi = findBasicBlockWithStart(cx, target);
            try scheduleBasicBlock(cx, target_bbi, remaining_stack);
        },
    }

    // Strings should never persist between basic blocks.
    if (cx.str_stack.len != 0) return error.BadData;

    if (builtin.mode == .Debug)
        bb.state = .decompiled;
}

fn decompileIns(cx: *DecompileCx, ins: lang.Ins) !void {
    for (ins.operands.slice()) |o|
        if (o == .variable)
            try cx.usage.track(o.variable);
    const info = cx.op_map.getPtrConst(ins.op.op);
    switch (info.*) {
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
        .dup => {
            if (cx.stack.len == 0) return error.BadData;
            const top = cx.stack.get(cx.stack.len - 1);
            try push(cx, .{ .dup = top });
        },
        .dup_multi => {
            const count_i16 = ins.operands.get(0).i16;
            const count = std.math.cast(u8, count_i16) orelse return error.BadData;
            if (cx.stack.len < count) return error.BadData;
            for (cx.stack.slice()[cx.stack.len - count ..]) |ei|
                try push(cx, .{ .dup = ei });
        },
        .jump_if => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.add(u16, ins.end, rel).?;
            const condition = try pop(cx);
            try storeStmt(cx, ins.end, .{ .jump_if = .{
                .target = target,
                .condition = condition,
            } });
        },
        .jump_unless => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.add(u16, ins.end, rel).?;
            const condition = try pop(cx);
            try storeStmt(cx, ins.end, .{ .jump_unless = .{
                .target = target,
                .condition = condition,
            } });
        },
        .jump => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.add(u16, ins.end, rel).?;
            try storeStmt(cx, ins.end, .{ .jump = .{ .target = target } });
        },
        .override => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.add(u16, ins.end, rel).?;
            try storeStmt(cx, ins.end, .{ .override = .{ .target = target } });
        },
        .generic => |*gen| {
            var args: BoundedArray(ExprIndex, lang.max_operands + script.max_params) = .{};
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
                const param = gen.params[pi];
                const ei = switch (param) {
                    .string => try popString(cx),
                    .list => try popList(cx),
                    .variadic => try popVariadicList(cx),
                    else => try pop(cx),
                };
                args.buffer[args.len + pi] = ei;
            }
            args.len += gen.params.len;

            const args_extra = try storeExtra(cx, args.slice());
            if (gen.call) {
                try push(cx, .{ .call = .{ .op = ins.op.op, .args = args_extra } });
            } else {
                try storeStmt(cx, ins.end, .{ .call = .{ .op = ins.op.op, .args = args_extra } });
            }
        },
        .illegal => {
            cx.diag.err(ins.start, "unhandled opcode {s}", .{@tagName(ins.op.op)});
            return error.AddedToDiagnostic;
        },
    }
}

fn push(cx: *DecompileCx, expr: Expr) !void {
    const ei = try storeExpr(cx, expr);
    try cx.stack.append(ei);
}

fn pop(cx: *DecompileCx) !ExprIndex {
    return cx.stack.pop() orelse try storeExpr(cx, .stack_fault);
}

fn popString(cx: *DecompileCx) !ExprIndex {
    const ei = try pop(cx);
    const expr = &cx.exprs.items[ei];
    if (expr.* == .int and expr.int == -1)
        return cx.str_stack.pop() orelse return error.BadData;
    return ei;
}

fn popList(cx: *DecompileCx) !ExprIndex {
    const items = try popListItems(cx);
    return storeExpr(cx, .{ .list = items });
}

fn popVariadicList(cx: *DecompileCx) !ExprIndex {
    const items = try popListItems(cx);
    return storeExpr(cx, .{ .variadic_list = items });
}

fn popListItems(cx: *DecompileCx) !ExtraSlice {
    const len_ei = try pop(cx);
    const len_expr = &cx.exprs.items[len_ei];
    if (len_expr.* != .int) return error.BadData;
    // TODO: think about the actual maximum here
    const len = std.math.cast(u8, len_expr.int) orelse return error.BadData;

    if (len > cx.stack.len) return error.BadData;
    const items = cx.stack.slice()[cx.stack.len - len ..];
    cx.stack.len -= len;

    return storeExtra(cx, items);
}

fn storeStmt(cx: *DecompileCx, end: u16, stmt: Stmt) !void {
    try cx.stmts.append(cx.gpa, stmt);
    try cx.stmt_ends.append(cx.gpa, end);
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

const TypeCx = struct {
    op_map: *const std.EnumArray(lang.Op, Op),
    symbols: *const Symbols,
    room_number: u8,
    id: Symbols.ScriptId,
    basic_blocks: []const BasicBlock,
    stmts: utils.SafeManyPointer([*]const Stmt),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    types: ArrayMap(Symbols.TypeIndex),
};

fn recoverTypes(cx: *TypeCx) void {
    for (cx.basic_blocks) |*bb| {
        const ss = bb.statements.defined;
        const stmts = cx.stmts.use()[ss.start..][0..ss.len];
        for (stmts) |*stmt| switch (stmt.*) {
            .jump_if, .jump_unless => |j| recoverExpr(cx, j.condition),
            .jump, .override => {},
            .call => |c| recoverCall(cx, c.op, c.args, null_expr),
            .compound, .binop_assign, .tombstone => unreachable,
        };
    }
}

fn recoverExpr(cx: *TypeCx, ei: ExprIndex) void {
    switch (cx.exprs.getPtr(ei).*) {
        .int, .string => {},
        .variable => |v| {
            const sym = cx.symbols.getVariable(cx.room_number, cx.id, v) orelse return;
            giveType(cx, ei, sym.type);
        },
        .call => |call| recoverCall(cx, call.op, call.args, ei),
        .list, .variadic_list => |items| {
            for (getExtra3(cx.extra, items)) |i|
                recoverExpr(cx, i);
        },
        .dup => |child| {
            recoverExpr(cx, child);
            unify(cx, ei, child);
        },
        .stack_fault => {},
    }
}

fn recoverCall(cx: *TypeCx, op: lang.Op, arg_eis: ExtraSlice, result_ei: ExprIndex) void {
    const args = getExtra3(cx.extra, arg_eis);
    for (args) |ei|
        recoverExpr(cx, ei);

    const info = cx.op_map.getPtrConst(op);
    if (info.* == .generic) {
        const gen = &info.generic;
        for (args[0..gen.params.len], gen.params) |arg, param|
            if (getTypeFromParam(param)) |ty|
                setType(cx, arg, ty);
    }

    switch (op) {
        .eq, .ne, .gt, .lt, .le, .ge, .set => {
            unify(cx, args[0], args[1]);
        },
        .@"in-list", .in => {
            const list = cx.exprs.getPtr(args[1]).list;
            for (getExtra3(cx.extra, list)) |ei|
                unify(cx, args[0], ei);
        },
        .@"get-array-item" => {
            const lhsi = cx.types.get(args[0]) orelse return;
            const lhs = cx.symbols.types.items[lhsi];
            if (lhs != .array) return;
            giveType(cx, args[1], lhs.array.across);
            if (eiOpt(result_ei)) |ei| {
                giveType(cx, ei, lhs.array.value);
                recoverMappedIndexType(cx, args[1], lhs.array.across, ei);
            }
        },
        .@"set-array-item" => {
            const lhsi = cx.types.get(args[0]) orelse return;
            const lhs = cx.symbols.types.items[lhsi];
            if (lhs != .array) return;
            giveType(cx, args[1], lhs.array.across);
            giveType(cx, args[2], lhs.array.value);
            recoverMappedIndexType(cx, args[1], lhs.array.across, args[2]);
        },
        .@"get-array-item-2d" => {
            const lhsi = cx.types.get(args[0]) orelse return;
            const lhs = cx.symbols.types.items[lhsi];
            if (lhs != .array) return;
            giveType(cx, args[1], lhs.array.down);
            giveType(cx, args[2], lhs.array.across);
            if (eiOpt(result_ei)) |ei| {
                giveType(cx, ei, lhs.array.value);
                recoverMappedIndexType(cx, args[2], lhs.array.across, ei);
            }
        },
        .@"set-array-item-2d" => {
            const lhsi = cx.types.get(args[0]) orelse return;
            const lhs = cx.symbols.types.items[lhsi];
            if (lhs != .array) return;
            giveType(cx, args[1], lhs.array.down);
            giveType(cx, args[2], lhs.array.across);
            giveType(cx, args[3], lhs.array.value);
            recoverMappedIndexType(cx, args[2], lhs.array.across, args[3]);
        },
        .@"start-script" => {
            recoverScriptArgs(cx, args[0], args[1]);
        },
        .@"start-script.rec" => {
            recoverScriptArgs(cx, args[0], args[1]);
        },
        .@"call-script" => {
            recoverScriptArgs(cx, args[0], args[1]);
        },
        .@"chain-script" => {
            recoverScriptArgs(cx, args[0], args[1]);
        },
        .@"chain-script.rec" => {
            recoverScriptArgs(cx, args[0], args[1]);
        },
        else => {},
    }
}

fn recoverMappedIndexType(
    cx: *TypeCx,
    index_ei: ExprIndex,
    index_ti: Symbols.TypeIndex,
    result_ei: ExprIndex,
) void {
    if (index_ti == Symbols.null_type) return;
    const index_type = &cx.symbols.types.items[index_ti];
    if (index_type.* != .map) return;
    const map = cx.symbols.maps.at(index_type.map);
    const index_expr = cx.exprs.getPtr(index_ei);
    if (index_expr.* != .int) return;
    const entry_index = std.sort.binarySearch(
        Symbols.MapEntry,
        map.entries.items,
        index_expr.int,
        Symbols.MapEntry.orderByValue,
    ) orelse return;
    const entry = &map.entries.items[entry_index];
    giveType(cx, result_ei, entry.type);
}

fn recoverScriptArgs(cx: *TypeCx, script_ei: ExprIndex, args_ei: ExprIndex) void {
    const script_expr = cx.exprs.getPtr(script_ei);
    const script_number_i32 = switch (script_expr.*) {
        .int => |i| i,
        else => return,
    };
    const script_number = std.math.cast(u32, script_number_i32) orelse return;
    const first_lsc = games.firstLocalScript(cx.symbols.game);
    const script_id: Symbols.ScriptId = if (script_number < first_lsc)
        .{ .global = script_number }
    else
        .{ .local = .{ .room = cx.room_number, .number = script_number } };
    const script_symbol = cx.symbols.getScript(script_id) orelse return;

    const args_expr = cx.exprs.getPtr(args_ei);
    const arg_eis = switch (args_expr.*) {
        .variadic_list => |s| getExtra3(cx.extra, s),
        else => return,
    };

    for (arg_eis, 0..) |arg_ei, i| {
        const local = script_symbol.locals.getPtr(i) orelse continue;
        giveType(cx, arg_ei, local.type);
    }
}

fn setType(cx: *TypeCx, ei: ExprIndex, typ: Symbols.InternedType) void {
    const ti = Symbols.getInternedType(typ);
    cx.types.put(utils.null_allocator, ei, ti) catch unreachable;
}

fn getTypeFromParam(param: script.Param) ?Symbols.InternedType {
    return switch (param) {
        .int, .string, .list, .variadic => null,
        .room => .room,
        .script => .script,
        .sound => .sound,
        .costume => .costume,
        .charset => .charset,
        .image => .image,
        .talkie => .talkie,
        .FileMode => .FileMode,
        .SaveLoad => .SaveLoad,
    };
}

fn giveType(cx: *TypeCx, ei: ExprIndex, typ: Symbols.TypeIndex) void {
    if (typ == Symbols.null_type) return;
    cx.types.put(utils.null_allocator, ei, typ) catch unreachable;
}

fn unify(cx: *TypeCx, a: ExprIndex, b: ExprIndex) void {
    const a_type = cx.types.get(a);
    const b_type = cx.types.get(b);
    if (a_type == null) if (b_type) |typ|
        cx.types.put(utils.null_allocator, a, typ) catch unreachable;
    if (b_type == null) if (a_type) |typ|
        cx.types.put(utils.null_allocator, b, typ) catch unreachable;
}

fn getExtra3(
    extra: utils.SafeManyPointer([*]const ExprIndex),
    slice: ExtraSlice,
) []const ExprIndex {
    return extra.use()[slice.start..][0..slice.len];
}

fn peephole(cx: *DecompileCx) void {
    for (cx.basic_blocks) |*bb| {
        const ss = bb.statements.defined;
        const stmts = cx.stmts.items[ss.start..][0..ss.len];
        for (stmts, 0..) |*stmt, i| {
            peepBinOpEq(cx, stmt);
            peepBinOpArrayItem(cx, stmt);
            peepBinOpArrayItem2D(cx, stmt);
            peepSpriteInit(cx, stmt);
            peepArraySortRows(cx, stmt);
            peepArraySortCols(cx, stmt);
            peepLockAndLoad(cx, stmts, i);
            peepPaletteRgb(cx, stmt);
            peepPaletteColor(cx, stmt);
            peepDeleteOnePolygon(cx, stmt);
        }
    }
}

/// Replace e.g. `x = x + y` with `x += y`
fn peepBinOpEq(cx: *DecompileCx, stmt: *Stmt) void {
    const set_args = stmtCallArgs(cx, stmt, .set, 2) orelse return;
    const set_var = &cx.exprs.items[set_args[0]];
    const bin = &cx.exprs.items[set_args[1]];
    if (bin.* != .call) return;
    const op = binOp(bin.call.op) orelse return;
    if (!op.hasEqAssign()) return;
    std.debug.assert(bin.call.args.len == 2);
    const bin_args = getExtra4(cx, bin.call.args);
    const lhs = &cx.exprs.items[bin_args[0]];
    if (lhs.* != .variable) return;
    if (lhs.variable.raw != set_var.variable.raw) return;

    stmt.* = .{ .binop_assign = .{ .op = op, .args = bin.call.args } };
}

/// Replace e.g. `arr[i] = arr[dup{i}] + x` with `arr[i] += x`
fn peepBinOpArrayItem(cx: *DecompileCx, stmt: *Stmt) void {
    const set_args = stmtCallArgs(cx, stmt, .@"set-array-item", 3) orelse return;
    const set_array = &cx.exprs.items[set_args[0]];
    // arr[dup{i}] + x
    const bin = &cx.exprs.items[set_args[2]];
    if (bin.* != .call) return;
    const op = binOp(bin.call.op) orelse return;
    if (!op.hasEqAssign()) return;
    std.debug.assert(bin.call.args.len == 2);
    const bin_args = getExtra4(cx, bin.call.args);
    // arr[dup{i}]
    const get = &cx.exprs.items[bin_args[0]];
    const get_args = callArgs(cx, get, .@"get-array-item", 2) orelse return;
    const get_array = &cx.exprs.items[get_args[0]];
    if (get_array.variable.raw != set_array.variable.raw) return;
    const get_index = &cx.exprs.items[get_args[1]];
    if (get_index.* != .dup) return;
    if (get_index.dup != set_args[1]) return;

    // re-use the binop args array for the new stmt args
    get_args[1] = set_args[1]; // in arr[dup{i}], replace dup{i} with i
    stmt.* = .{ .binop_assign = .{ .op = op, .args = bin.call.args } };
}

/// Replace e.g. `arr[i][j] = arr[dup{i}][dup{j}] + x` with `arr[i][j] += x`
fn peepBinOpArrayItem2D(cx: *DecompileCx, stmt: *Stmt) void {
    const set_args = stmtCallArgs(cx, stmt, .@"set-array-item-2d", 4) orelse return;
    const set_array = &cx.exprs.items[set_args[0]];
    // arr[dup{i}][dup{j}] + x
    const bin = &cx.exprs.items[set_args[3]];
    if (bin.* != .call) return;
    const op = binOp(bin.call.op) orelse return;
    if (!op.hasEqAssign()) return;
    std.debug.assert(bin.call.args.len == 2);
    const bin_args = getExtra4(cx, bin.call.args);
    // arr[dup{i}][dup{j}]
    const get = &cx.exprs.items[bin_args[0]];
    const get_args = callArgs(cx, get, .@"get-array-item-2d", 3) orelse return;
    const get_array = &cx.exprs.items[get_args[0]];
    if (get_array.variable.raw != set_array.variable.raw) return;
    const get_row = &cx.exprs.items[get_args[1]];
    if (get_row.* != .dup) return;
    if (get_row.dup != set_args[1]) return;
    const get_col = &cx.exprs.items[get_args[2]];
    if (get_col.* != .dup) return;
    if (get_col.dup != set_args[2]) return;

    // re-use the binop args array for the new stmt args
    get_args[1] = set_args[1]; // in arr[dup{i}][dup{j}], replace dup{i} with i
    get_args[2] = set_args[2]; // in arr[dup{i}][dup{j}], replace dup{j} with j
    stmt.* = .{ .binop_assign = .{ .op = op, .args = bin.call.args } };
}

/// Replace `sprite.init-range x dup{x}` with `sprite.init x`
fn peepSpriteInit(cx: *DecompileCx, stmt: *Stmt) void {
    const args = stmtCallArgs(cx, stmt, .@"sprite.init-range", 2) orelse return;
    const second = &cx.exprs.items[args[1]];
    if (second.* != .dup) return;
    if (second.dup != args[0]) return;

    const new_args: ExtraSlice = .{ .start = stmt.call.args.start, .len = 1 };
    stmt.* = .{ .compound = .{
        .op = .@"sprite.init",
        .args = new_args,
    } };
}

/// Handle dup across in `array-sort`
fn peepArraySortRows(cx: *DecompileCx, stmt: *Stmt) void {
    const args = stmtCallArgs(cx, stmt, .@"array-sort", 6) orelse return;
    // args are [array, down_from, down_to, across_from, across_to, order]
    const across_to = &cx.exprs.items[args[4]];
    if (across_to.* != .dup) return;
    if (across_to.dup != args[3]) return;

    var new_args = stmt.call.args;
    new_args.len -= 1;
    args[4] = args[5];
    stmt.* = .{ .compound = .{ .op = .@"array-sort-rows", .args = new_args } };
}

/// Handle dup down in `array-sort`
fn peepArraySortCols(cx: *DecompileCx, stmt: *Stmt) void {
    const args = stmtCallArgs(cx, stmt, .@"array-sort", 6) orelse return;
    // args are [array, down_from, down_to, across_from, across_to, order]
    const down_to = &cx.exprs.items[args[2]];
    if (down_to.* != .dup) return;
    if (down_to.dup != args[1]) return;

    var new_args = stmt.call.args;
    new_args.len -= 1;
    std.mem.copyForwards(ExprIndex, args[2..][0..3], args[3..][0..3]);
    stmt.* = .{ .compound = .{ .op = .@"array-sort-cols", .args = new_args } };
}

fn peepLockAndLoad(cx: *DecompileCx, stmts: []Stmt, stmt_index: usize) void {
    const Group = struct {
        load_op: lang.Op,
        combined_op: script.Compound,

        fn init(load_op: lang.Op, combined_op: script.Compound) @This() {
            return .{ .load_op = load_op, .combined_op = combined_op };
        }
    };

    const groups: std.EnumMap(lang.Op, Group) = .init(.{
        .@"lock-script" = .init(.@"load-script", .@"lock-and-load-script"),
        .@"lock-costume" = .init(.@"load-costume", .@"lock-and-load-costume"),
        .@"lock-image" = .init(.@"load-image", .@"lock-and-load-image"),
    });

    if (stmt_index + 1 >= stmts.len) return;
    const lock = &stmts[stmt_index];
    const load = &stmts[stmt_index + 1];

    if (lock.* != .call) return;
    const group = groups.get(lock.call.op) orelse return;
    std.debug.assert(lock.call.args.len == 1);
    const lock_args = getExtra4(cx, lock.call.args);
    const lock_arg = &cx.exprs.items[lock_args[0]];
    if (lock_arg.* != .dup) return;
    const resource = lock_arg.dup;

    const load_args = stmtCallArgs(cx, load, group.load_op, 1) orelse return;
    if (load_args[0] != resource) return;

    stmts[stmt_index] = .{ .compound = .{
        .op = group.combined_op,
        .args = load.call.args,
    } };
    stmts[stmt_index + 1] = .tombstone;
}

/// Replace `palette.rgb s dup{s} r g b` with `palette.slot-rgb s r g b`
fn peepPaletteRgb(cx: *DecompileCx, stmt: *Stmt) void {
    const args = stmtCallArgs(cx, stmt, .@"palette.rgb", 5) orelse return;
    const second = &cx.exprs.items[args[1]];
    if (second.* != .dup) return;
    if (second.dup != args[0]) return;

    std.mem.copyForwards(ExprIndex, args[1..][0..3], args[2..][0..3]);
    const new_args: ExtraSlice = .{ .start = stmt.call.args.start, .len = 4 };
    stmt.* = .{ .compound = .{
        .op = .@"palette.slot-rgb",
        .args = new_args,
    } };
}

/// Replace `palette.color a dup{a} b` with `palette.slot-color a b`
fn peepPaletteColor(cx: *DecompileCx, stmt: *Stmt) void {
    const args = stmtCallArgs(cx, stmt, .@"palette.color", 3) orelse return;
    const second = &cx.exprs.items[args[1]];
    if (second.* != .dup) return;
    if (second.dup != args[0]) return;

    args[1] = args[2];
    const new_args: ExtraSlice = .{ .start = stmt.call.args.start, .len = 2 };
    stmt.* = .{ .compound = .{
        .op = .@"palette.slot-color",
        .args = new_args,
    } };
}

/// Replace `delete-polygon a dup{a}` with `delete-one-polygon a`
fn peepDeleteOnePolygon(cx: *DecompileCx, stmt: *Stmt) void {
    const args = stmtCallArgs(cx, stmt, .@"delete-polygon", 2) orelse return;
    const second = &cx.exprs.items[args[1]];
    if (second.* != .dup) return;
    if (second.dup != args[0]) return;

    const new_args: ExtraSlice = .{ .start = stmt.call.args.start, .len = 1 };
    stmt.* = .{ .compound = .{
        .op = .@"delete-one-polygon",
        .args = new_args,
    } };
}

fn stmtCallArgs(cx: *const DecompileCx, stmt: *const Stmt, op: lang.Op, len: usize) ?[]ExprIndex {
    if (stmt.* != .call) return null;
    if (stmt.call.op != op) return null;
    std.debug.assert(stmt.call.args.len == len);
    return getExtra4(cx, stmt.call.args);
}

fn callArgs(cx: *const DecompileCx, expr: *const Expr, op: lang.Op, len: usize) ?[]ExprIndex {
    if (expr.* != .call) return null;
    if (expr.call.op != op) return null;
    std.debug.assert(expr.call.args.len == len);
    return getExtra4(cx, expr.call.args);
}

fn getExtra4(cx: *const DecompileCx, slice: ExtraSlice) []ExprIndex {
    return cx.extra.items[slice.start..][0..slice.len];
}

const StructuringCx = struct {
    gpa: std.mem.Allocator,
    stmts: utils.SafeManyPointer([*]Stmt),
    stmt_ends: utils.SafeManyPointer([*]u16),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: *std.ArrayListUnmanaged(ExprIndex),

    queue: std.ArrayListUnmanaged(NodeIndex),
    nodes: std.ArrayListUnmanaged(Node),
    /// debug only, used to assert that all nodes are scanned once per phase
    trail: if (builtin.mode == .Debug) std.DynamicBitSetUnmanaged else void,
};

const NodeIndex = u16;
const null_node: NodeIndex = 0xffff;

fn niOpt(ni: NodeIndex) ?NodeIndex {
    return if (ni == null_node) null else ni;
}

const Node = struct {
    /// start pc
    start: u16,
    /// end pc
    end: u16,

    // doubly linked list of sibling nodes
    prev: NodeIndex,
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
    if_else: struct {
        condition: ExprIndex,
        true: NodeIndex,
        false: NodeIndex,
    },
    @"while": struct {
        condition: ExprIndex,
        body: NodeIndex,
    },
    @"for": struct {
        accumulator: lang.Variable,
        start: ExprIndex,
        end: ExprIndex,
        direction: Ast.ForDirection,
        body: NodeIndex,
    },
    for_in: struct {
        target: lang.Variable,
        list: ExprIndex,
        backing: lang.Variable,
        body: NodeIndex,
    },
    do: struct {
        body: NodeIndex,
        condition: ExprIndex,
    },
    case: struct {
        value: ExprIndex,
        first_branch: NodeIndex,
    },
    case_branch: struct {
        value: ExprIndex,
        body: NodeIndex,
    },
    /// debug only, used for invariant tracking
    orphan,
};

const root_node_index = 0;

fn structure(cx: *StructuringCx, basic_blocks: []const BasicBlock) !void {
    // Populate initial nodes, just every basic block one after the other
    const bb_nodes = try cx.nodes.addManyAsSlice(cx.gpa, basic_blocks.len);
    for (bb_nodes, basic_blocks, 0..) |*node, *bb, bbi_usize| {
        const bbi: BasicBlockIndex = @intCast(bbi_usize);
        const bb_start = if (bbi == 0) 0 else basic_blocks[bbi - 1].end;
        const prev = if (bbi == 0) null_node else bbi - 1;
        const next = if (bbi == basic_blocks.len - 1) null_node else bbi + 1;
        node.* = .{
            .start = bb_start,
            .end = bb.end,
            .prev = prev,
            .next = next,
            .kind = .{ .basic_block = .{
                .exit = bb.exit,
                .statements = bb.statements.defined,
            } },
        };
    }

    structureCheckpoint(cx, "initial", .{});

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntCase(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntIf(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntWhile(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntBreakUntil(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntDo(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntIfElse(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntFor(cx, ni);
    try endPhase(cx);

    try startPhase(cx);
    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntForIn(cx, ni);
    try endPhase(cx);
}

fn queueNode(cx: *StructuringCx, ni: NodeIndex) !void {
    if (ni == null_node) return;
    try cx.queue.append(cx.gpa, ni);
}

fn queueChildren(cx: *StructuringCx, ni: NodeIndex) !void {
    switch (cx.nodes.items[ni].kind) {
        .basic_block => {},
        .@"if" => |*n| {
            try queueNode(cx, n.true);
        },
        .if_else => |*n| {
            try queueNode(cx, n.true);
            try queueNode(cx, n.false);
        },
        .@"while" => |*n| {
            try queueNode(cx, n.body);
        },
        .do => |*n| {
            try queueNode(cx, n.body);
        },
        .@"for" => |*n| {
            try queueNode(cx, n.body);
        },
        .for_in => |*n| {
            try queueNode(cx, n.body);
        },
        .case => |*n| {
            try queueNode(cx, n.first_branch);
        },
        .case_branch => |*n| {
            try queueNode(cx, n.body);
        },
        .orphan => unreachable,
    }
}

fn startPhase(cx: *StructuringCx) !void {
    if (builtin.mode != .Debug) return;
    cx.trail.unsetAll();
}

fn endPhase(cx: *StructuringCx) !void {
    if (builtin.mode != .Debug) return;
    try updateTrailLen(cx);
    for (0..cx.nodes.items.len) |i| {
        const ni: NodeIndex = @intCast(i);
        const node = &cx.nodes.items[ni];
        if (cx.trail.isSet(ni)) continue;
        if (node.kind == .orphan) continue;
        std.debug.panic("skipped node {}", .{ni});
    }
}

fn trackStructured(cx: *StructuringCx, ni: NodeIndex) !void {
    if (builtin.mode != .Debug) return;
    try updateTrailLen(cx);
    if (cx.trail.isSet(ni))
        std.debug.panic("structuring node {} twice", .{ni});
    cx.trail.set(ni);
}

fn trackMoved(cx: *StructuringCx, ni_from: NodeIndex, ni_to: NodeIndex) !void {
    if (builtin.mode != .Debug) return;
    try updateTrailLen(cx);
    if (!cx.trail.isSet(ni_from)) return;
    cx.trail.unset(ni_from);
    cx.trail.set(ni_to);
}

fn updateTrailLen(cx: *StructuringCx) !void {
    // all the BitSet methods require the length to be set ahead of time, so
    // just do it once here to make things easy
    const expected = cx.nodes.items.len;
    if (cx.trail.bit_length > expected) @panic("oops");
    if (cx.trail.bit_length == expected) return;
    try cx.trail.resize(cx.gpa, expected, false);
}

fn huntIf(cx: *StructuringCx, ni_first: NodeIndex) !void {
    var ni = ni_first;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.kind != .basic_block) continue;
        if (node.kind.basic_block.exit != .jump_unless) continue;
        if (node.kind.basic_block.exit.jump_unless < node.end) continue;
        const ni_true_end = findNodeWithEnd(cx, ni, node.kind.basic_block.exit.jump_unless);
        if (ni_true_end == null_node) continue;

        const ni_t_e = if (ni_true_end != ni) ni_true_end else null_node;
        try makeIf(cx, ni, ni_t_e);
    }
}

fn makeIf(cx: *StructuringCx, ni_before: NodeIndex, ni_true_end: NodeIndex) !void {
    const empty = ni_true_end == null_node;
    const ni_last = niOpt(ni_true_end) orelse ni_before;
    const end = cx.nodes.items[ni_last].end;
    const ni_after = cx.nodes.items[ni_last].next;
    const ni_true_start = if (!empty) cx.nodes.items[ni_before].next else null_node;

    const condition = chopJumpCondition(cx, ni_before);
    const ni_if = try appendNode(cx, .{
        .start = cx.nodes.items[ni_before].end,
        .end = end,
        .prev = ni_before,
        .next = ni_after,
        .kind = .{ .@"if" = .{
            .condition = condition,
            .true = ni_true_start,
        } },
    });
    cx.nodes.items[ni_before].next = ni_if;
    if (ni_after != null_node)
        cx.nodes.items[ni_after].prev = ni_if;

    if (!empty) {
        cx.nodes.items[ni_true_start].prev = null_node;
        cx.nodes.items[ni_true_end].next = null_node;
    }

    structureCheckpoint(cx, "makeIf ni_before={} ni_true_end={}", .{ ni_before, ni_true_end });
}

fn huntIfElse(cx: *StructuringCx, ni_first: NodeIndex) !void {
    var ni = ni_first;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.kind != .@"if") continue;
        if (node.kind.@"if".true == null_node) continue;

        const ni_true_end = findLastNode(cx, node.kind.@"if".true);
        const true_end = &cx.nodes.items[ni_true_end];
        if (true_end.kind != .basic_block) continue;
        if (true_end.kind.basic_block.exit != .jump) continue;
        if (true_end.kind.basic_block.exit.jump < node.end) continue;
        const ni_false_end = findNodeWithEnd(cx, ni, true_end.kind.basic_block.exit.jump);
        if (ni_false_end == null_node) continue;

        const ni_f_e = if (ni_false_end != ni) ni_false_end else null_node;
        try makeIfElse(cx, ni, ni_true_end, ni_f_e);
    }
}

fn makeIfElse(
    cx: *StructuringCx,
    ni: NodeIndex,
    ni_true_end: NodeIndex,
    ni_false_end: NodeIndex,
) !void {
    const empty = ni_false_end == null_node;
    const node = &cx.nodes.items[ni];
    const condition = node.kind.@"if".condition;
    const ni_true_start = node.kind.@"if".true;
    const ni_false_start = if (!empty) node.next else null_node;
    const ni_last = niOpt(ni_false_end) orelse ni;
    const ni_after = cx.nodes.items[ni_last].next;

    chopJump(cx, ni_true_end);

    node.end = cx.nodes.items[ni_last].end;
    node.next = ni_after;
    node.kind = .{ .if_else = .{
        .condition = condition,
        .true = ni_true_start,
        .false = ni_false_start,
    } };
    if (ni_after != null_node)
        cx.nodes.items[ni_after].prev = ni;

    if (!empty) {
        cx.nodes.items[ni_false_start].prev = null_node;
        cx.nodes.items[ni_false_end].next = null_node;
    }

    structureCheckpoint(cx, "makeIfElse ni={} ni_false_end={}", .{ ni, ni_false_end });

    try queueNode(cx, node.kind.if_else.false);
}

fn huntWhile(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.kind != .@"if") continue;
        if (node.kind.@"if".true == null_node) continue;
        const ni_true_last = findLastNode(cx, node.kind.@"if".true);
        const true_last = &cx.nodes.items[ni_true_last];
        if (true_last.kind != .basic_block) continue;
        if (true_last.kind.basic_block.exit != .jump) continue;
        if (true_last.kind.basic_block.exit.jump != node.start) continue;

        makeWhile(cx, ni, ni_true_last);
    }
}

fn makeWhile(cx: *StructuringCx, ni_if: NodeIndex, ni_true_last: NodeIndex) void {
    const node = &cx.nodes.items[ni_if];
    const condition = node.kind.@"if".condition;
    const ni_body = node.kind.@"if".true;

    chopJump(cx, ni_true_last);

    node.kind = .{ .@"while" = .{
        .condition = condition,
        .body = ni_body,
    } };

    structureCheckpoint(cx, "makeWhile ni_if={} ni_true_last={}", .{ ni_if, ni_true_last });
}

fn huntFor(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        if (isFor(cx, ni)) |f|
            makeFor(cx, ni, f);
    }
}

const For = struct {
    accumulator: lang.Variable,
    start: ExprIndex,
    end: ExprIndex,
    direction: Ast.ForDirection,
    body_last: NodeIndex,
};

fn isFor(cx: *StructuringCx, ni: NodeIndex) ?For {
    const node = &cx.nodes.items[ni];
    if (node.kind != .@"while") return null;
    const cond = cx.exprs.getPtr(node.kind.@"while".condition);
    if (cond.* != .call) return null;
    const dir: Ast.ForDirection = switch (cond.call.op) {
        .le => .up,
        .ge => .down,
        else => return null,
    };
    std.debug.assert(cond.call.args.len == 2);
    const cond_args = getExtra2(cx.extra, cond.call.args);
    const accum_expr = cx.exprs.getPtr(cond_args[0]);
    if (accum_expr.* != .variable) return null;
    const accum = accum_expr.variable;

    // Due to a quirk of how if nodes are structured, there will always be an
    // empty basic block leftover before the one we're interested in here.
    if (node.prev == null_node) return null;
    const empty = &cx.nodes.items[node.prev];
    if (empty.kind != .basic_block) return null;
    if (empty.start != empty.end) return null;

    if (empty.prev == null_node) return null;
    const prev = &cx.nodes.items[empty.prev];
    if (prev.kind != .basic_block) return null;
    const init_ss = prev.kind.basic_block.statements;
    if (init_ss.len == 0) return null;
    const init = cx.stmts.getPtr(init_ss.last());
    if (init.* != .call) return null;
    if (init.call.op != .set) return null;
    std.debug.assert(init.call.args.len == 2);
    const init_args = getExtra2(cx.extra, init.call.args);
    const init_lhs = cx.exprs.getPtr(init_args[0]);
    if (init_lhs.* != .variable) return null;
    if (init_lhs.variable.raw != accum.raw) return null;

    const ni_last = findLastNode(cx, node.kind.@"while".body);
    const last = &cx.nodes.items[ni_last];
    if (last.kind != .basic_block) return null;
    const inc_ss = last.kind.basic_block.statements;
    if (inc_ss.len == 0) return null;
    const inc = cx.stmts.getPtr(inc_ss.last());
    if (inc.* != .call) return null;
    const expected_op: lang.Op = if (dir == .up) .inc else .dec;
    if (inc.call.op != expected_op) return null;
    std.debug.assert(inc.call.args.len == 1);
    const inc_args = getExtra2(cx.extra, inc.call.args);
    const inc_lhs = cx.exprs.getPtr(inc_args[0]);
    if (inc_lhs.* != .variable) return null;
    if (inc_lhs.variable.raw != accum.raw) return null;

    return .{
        .accumulator = accum,
        .start = init_args[1],
        .end = cond_args[1],
        .direction = dir,
        .body_last = ni_last,
    };
}

fn makeFor(cx: *StructuringCx, ni: NodeIndex, info: For) void {
    const node = &cx.nodes.items[ni];
    const ni_empty = node.prev;
    const empty = &cx.nodes.items[ni_empty];
    const ni_init = empty.prev;
    const ni_body = node.kind.@"while".body;

    chopEndStmts(cx, ni_init, 1); // chop off the init
    chopEndStmts(cx, info.body_last, 1); // chop off the increment

    node.start = cx.nodes.items[ni_init].end;
    node.prev = ni_init;
    node.kind = .{ .@"for" = .{
        .accumulator = info.accumulator,
        .start = info.start,
        .end = info.end,
        .direction = info.direction,
        .body = ni_body,
    } };

    cx.nodes.items[ni_init].next = ni;

    orphanNode(cx, ni_empty);

    structureCheckpoint(cx, "makeFor ni={} body_last={}", .{ ni, info.body_last });
}

fn huntForIn(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        if (isForIn(cx, ni)) |f|
            makeForIn(cx, ni, f);
    }
}

const ForIn = struct {
    ni_body_end: NodeIndex,
    target: lang.Variable,
    list: ExprIndex,
    backing: lang.Variable,
};

fn isForIn(cx: *StructuringCx, ni: NodeIndex) ?ForIn {
    // match this pattern:
    //
    // array-assign backing [a b c] 1
    // localize backing
    // backing[0] = 0
    // LABEL:
    // inc-array-item backing 0
    // if (backing[0] <= 3) {
    //     target = backing[backing[0]]
    //     {body}
    //     jump LABEL
    // }
    // undim backing
    //
    // and convert it to:
    //
    // for target = [a b c] {
    //     {body}
    // }

    // if (backing[0] <= len)
    const node = &cx.nodes.items[ni];
    if (node.kind != .@"if") return null;
    const cond = cx.exprs.getPtr(node.kind.@"if".condition);
    if (cond.* != .call) return null;
    if (cond.call.op != .le) return null;
    std.debug.assert(cond.call.args.len == 2);
    const cond_args = getExtra2(cx.extra, cond.call.args);
    const cond_lhs = cx.exprs.getPtr(cond_args[0]);
    if (cond_lhs.* != .call) return null;
    if (cond_lhs.call.op != .@"get-array-item") return null;
    std.debug.assert(cond_lhs.call.args.len == 2);
    const cond_lhs_args = getExtra2(cx.extra, cond_lhs.call.args);
    const backing_expr = cx.exprs.getPtr(cond_lhs_args[0]);
    if (backing_expr.* != .variable) return null;
    const backing = backing_expr.variable;
    const cond_rhs = cx.exprs.getPtr(cond_args[1]);
    if (cond_rhs.* != .int) return null;
    const len = cond_rhs.int;

    // inc-array-item backing 0
    if (node.prev == null_node) return null;
    const inc_node = &cx.nodes.items[node.prev];
    if (inc_node.kind != .basic_block) return null;
    if (inc_node.kind.basic_block.exit != .no_jump) return null;
    if (inc_node.kind.basic_block.statements.len != 1) return null;
    const inc = cx.stmts.getPtr(inc_node.kind.basic_block.statements.start);
    if (inc.* != .call) return null;
    if (inc.call.op != .@"inc-array-item") return null;
    std.debug.assert(inc.call.args.len == 2);
    const inc_args = getExtra2(cx.extra, inc.call.args);
    const inc_array = cx.exprs.getPtr(inc_args[0]);
    if (inc_array.* != .variable) return null;
    if (inc_array.variable.raw != backing.raw) return null;
    const inc_index = cx.exprs.getPtr(inc_args[1]);
    if (inc_index.* != .int) return null;
    if (inc_index.int != 0) return null;

    // the three init statements
    if (inc_node.prev == null_node) return null;
    const init = &cx.nodes.items[inc_node.prev];
    if (init.kind != .basic_block) return null;
    if (init.kind.basic_block.exit != .no_jump) return null;
    const init_ss = init.kind.basic_block.statements;
    if (init_ss.len < 3) return null;
    const init_stmts = cx.stmts.use()[init_ss.start + init_ss.len - 3 ..][0..3];
    // array-assign backing [list] 1
    const assign = &init_stmts[0];
    if (assign.* != .call) return null;
    if (assign.call.op != .@"array-assign") return null;
    std.debug.assert(assign.call.args.len == 3);
    const assign_args = getExtra2(cx.extra, assign.call.args);
    const assign_var = cx.exprs.getPtr(assign_args[0]);
    if (assign_var.* != .variable) return null;
    if (assign_var.variable.raw != backing.raw) return null;
    const assign_list = cx.exprs.getPtr(assign_args[1]);
    if (assign_list.* != .list) return null;
    if (assign_list.list.len != len) return null;
    const assign_index = cx.exprs.getPtr(assign_args[2]);
    if (assign_index.* != .int) return null;
    if (assign_index.int != 1) return null;
    // localize backing
    const localize = &init_stmts[1];
    if (localize.* != .call) return null;
    if (localize.call.op != .localize) return null;
    std.debug.assert(localize.call.args.len == 1);
    const localize_args = getExtra2(cx.extra, localize.call.args);
    const localize_var = cx.exprs.getPtr(localize_args[0]);
    if (localize_var.* != .variable) return null;
    if (localize_var.variable.raw != backing.raw) return null;
    // backing[0] = 0
    const zero = &init_stmts[2];
    if (zero.* != .call) return null;
    if (zero.call.op != .@"set-array-item") return null;
    std.debug.assert(zero.call.args.len == 3);
    const zero_args = getExtra2(cx.extra, zero.call.args);
    const zero_var = cx.exprs.getPtr(zero_args[0]);
    if (zero_var.* != .variable) return null;
    if (zero_var.variable.raw != backing.raw) return null;
    const zero_index = cx.exprs.getPtr(zero_args[1]);
    if (zero_index.* != .int) return null;
    if (zero_index.int != 0) return null;
    const zero_value = cx.exprs.getPtr(zero_args[2]);
    if (zero_value.* != .int) return null;
    if (zero_value.int != 0) return null;

    // undim backing
    if (node.next == null_node) return null;
    const node_undim = &cx.nodes.items[node.next];
    if (node_undim.kind != .basic_block) return null;
    if (node_undim.kind.basic_block.statements.len == 0) return null;
    const undim = cx.stmts.getPtr(node_undim.kind.basic_block.statements.start);
    if (undim.* != .call) return null;
    if (undim.call.op != .undim) return null;
    std.debug.assert(undim.call.args.len == 1);
    const undim_args = getExtra2(cx.extra, undim.call.args);
    const undim_var = cx.exprs.getPtr(undim_args[0]);
    if (undim_var.* != .variable) return null;
    if (undim_var.variable.raw != backing.raw) return null;

    // target = backing[backing[0]]
    if (node.kind.@"if".true == null_node) return null;
    const body_start = &cx.nodes.items[node.kind.@"if".true];
    if (body_start.kind != .basic_block) return null;
    if (body_start.kind.basic_block.statements.len == 0) return null;
    const pull = cx.stmts.getPtr(body_start.kind.basic_block.statements.start);
    if (pull.* != .call) return null;
    if (pull.call.op != .set) return null;
    std.debug.assert(pull.call.args.len == 2);
    const pull_args = getExtra2(cx.extra, pull.call.args);
    const pull_var = cx.exprs.getPtr(pull_args[0]);
    if (pull_var.* != .variable) return null;
    const target = pull_var.variable;
    // backing[backing[0]]
    const pull_rhs = cx.exprs.getPtr(pull_args[1]);
    if (pull_rhs.* != .call) return null;
    if (pull_rhs.call.op != .@"get-array-item") return null;
    std.debug.assert(pull_rhs.call.args.len == 2);
    const pull_rhs_args = getExtra2(cx.extra, pull_rhs.call.args);
    const pull_rhs_var = cx.exprs.getPtr(pull_rhs_args[0]);
    if (pull_rhs_var.* != .variable) return null;
    if (pull_rhs_var.variable.raw != backing.raw) return null;
    // backing[0]
    const pull_rhs_index = cx.exprs.getPtr(pull_rhs_args[1]);
    if (pull_rhs_index.* != .call) return null;
    if (pull_rhs_index.call.op != .@"get-array-item") return null;
    std.debug.assert(pull_rhs_index.call.args.len == 2);
    const pull_rhs_index_args = getExtra2(cx.extra, pull_rhs_index.call.args);
    const pull_rhs_index_var = cx.exprs.getPtr(pull_rhs_index_args[0]);
    if (pull_rhs_index_var.* != .variable) return null;
    if (pull_rhs_index_var.variable.raw != backing.raw) return null;
    const pull_rhs_index_int = cx.exprs.getPtr(pull_rhs_index_args[1]);
    if (pull_rhs_index_int.* != .int) return null;
    if (pull_rhs_index_int.int != 0) return null;

    // jump LABEL
    const ni_body_end = findLastNode(cx, node.kind.@"if".true);
    const body_end = &cx.nodes.items[ni_body_end];
    if (body_end.kind != .basic_block) return null;
    if (body_end.kind.basic_block.exit != .jump) return null;
    if (body_end.kind.basic_block.exit.jump != inc_node.start) return null;

    return .{
        .ni_body_end = ni_body_end,
        .target = target,
        .list = assign_args[1],
        .backing = backing,
    };
}

fn makeForIn(cx: *StructuringCx, ni: NodeIndex, info: ForIn) void {
    const node = &cx.nodes.items[ni];
    const ni_inc = node.prev;
    const node_inc = &cx.nodes.items[ni_inc];
    const ni_init = node_inc.prev;
    const ni_body = node.kind.@"if".true;
    const ni_after = node.next;

    cx.nodes.items[ni_init].next = ni;
    chopEndStmts(cx, ni_init, 3);

    chopStartStmts(cx, ni_body, 1); // chop off `target = backing[backing[0]]`
    chopJump(cx, info.ni_body_end);

    orphanNode(cx, ni_inc);

    chopStartStmts(cx, ni_after, 1); // chop off `undim backing`

    cx.nodes.items[ni] = .{
        .start = cx.nodes.items[ni_init].end,
        .end = cx.nodes.items[ni_after].start,
        .prev = ni_init,
        .next = ni_after,
        .kind = .{ .for_in = .{
            .target = info.target,
            .list = info.list,
            .backing = info.backing,
            .body = ni_body,
        } },
    };

    structureCheckpoint(cx, "makeForIn ni={}", .{ni});
}

fn huntDo(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.kind != .basic_block) continue;
        const target = switch (node.kind.basic_block.exit) {
            .jump, .jump_unless => |target| target,
            else => continue,
        };
        if (target >= node.end) continue;

        const ni_body_first = findBackwardsNodeWithStart(cx, ni, target) orelse continue;

        // makeDo replaces the node, so continue from the new node index, not
        // the stale current node index
        ni = try makeDo(cx, ni_body_first, ni);
    }
}

fn makeDo(
    cx: *StructuringCx,
    ni_body_first_orig: NodeIndex,
    ni_condition_orig: NodeIndex,
) !NodeIndex {
    const ni_body_first = try moveNode(cx, ni_body_first_orig);
    const ni_condition = if (ni_condition_orig == ni_body_first_orig)
        ni_body_first
    else
        ni_condition_orig;
    const ni_do = ni_body_first_orig;

    const end = cx.nodes.items[ni_condition].end;
    const ni_before = cx.nodes.items[ni_body_first].prev;
    const ni_after = cx.nodes.items[ni_condition].next;
    const condition = switch (cx.nodes.items[ni_condition].kind.basic_block.exit) {
        .jump => blk: {
            chopJump(cx, ni_condition);
            break :blk null_expr;
        },
        .jump_unless => chopJumpCondition(cx, ni_condition),
        else => unreachable,
    };

    cx.nodes.items[ni_do] = .{
        .start = cx.nodes.items[ni_body_first].start,
        .end = end,
        .prev = ni_before,
        .next = ni_after,
        .kind = .{ .do = .{
            .body = ni_body_first,
            .condition = condition,
        } },
    };
    if (ni_before != null_node)
        cx.nodes.items[ni_before].next = ni_do;
    if (ni_after != null_node)
        cx.nodes.items[ni_after].prev = ni_do;

    cx.nodes.items[ni_body_first].prev = null_node;
    cx.nodes.items[ni_condition].next = null_node;

    structureCheckpoint(
        cx,
        "makeDo ni_body_first_orig={} ni_condition_orig={}",
        .{ ni_body_first_orig, ni_condition_orig },
    );

    try trackStructured(cx, ni_do);
    return ni_do;
}

fn huntBreakUntil(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);
        try queueChildren(cx, ni);

        if (isBreakUntil(cx, ni))
            try makeBreakUntil(cx, ni);
    }
}

fn isBreakUntil(cx: *StructuringCx, ni: NodeIndex) bool {
    const node = &cx.nodes.items[ni];
    if (node.next == null_node) return false;
    const next = &cx.nodes.items[node.next];

    if (node.kind != .basic_block) return false;
    if (node.kind.basic_block.exit != .jump_if) return false;
    if (node.kind.basic_block.exit.jump_if != next.end) return false;
    if (node.kind.basic_block.statements.len != 1) return false;

    if (next.kind != .basic_block) return false;
    if (next.kind.basic_block.exit != .jump) return false;
    if (next.kind.basic_block.exit.jump != node.start) return false;
    const ss = next.kind.basic_block.statements;
    const stmts = cx.stmts.use()[ss.start..][0..ss.len];
    if (stmts.len != 2) return false;
    if (stmts[0] != .call) return false;
    if (stmts[0].call.op != .@"break-here") return false;

    return true;
}

fn makeBreakUntil(cx: *StructuringCx, ni: NodeIndex) !void {
    const node = &cx.nodes.items[ni];
    const ni_loop = cx.nodes.items[ni].next;
    const ni_after = cx.nodes.items[ni_loop].next;
    const stmt_index = node.kind.basic_block.statements.start;

    const stmt = cx.stmts.getPtr(stmt_index);
    const condition = stmt.jump_if.condition;
    stmt.* = .{ .compound = .{
        .op = .@"break-until",
        .args = try storeExtra2(cx.gpa, cx.extra, &.{condition}),
    } };
    cx.stmt_ends.set(stmt_index, cx.nodes.items[ni_loop].end);

    node.end = cx.nodes.items[ni_loop].end;
    node.next = ni_after;
    if (ni_after != null_node)
        cx.nodes.items[ni_after].prev = ni;
    node.kind.basic_block.exit = .no_jump;

    orphanNode(cx, ni_loop);

    structureCheckpoint(cx, "makeBreakUntil ni={}", .{ni});
}

fn huntCase(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try trackStructured(cx, ni);

        if (isCase(cx, ni)) |ni_last|
            try makeCase(cx, ni, ni_last);

        try queueChildren(cx, ni);
    }
}

fn isCase(cx: *StructuringCx, ni_start: NodeIndex) ?NodeIndex {
    const value, var ni, const end = checkCaseBranch(cx, ni_start) orelse return null;
    while (true) {
        if (checkCaseBranchAnother(cx, ni, value, end)) |i| {
            ni = i;
            continue;
        }
        // If it's not a continue, it must be the else branch. Check if it's
        // well-formed and if so return success.
        if (!nodeStartsWithPop(cx, ni, value)) return null;
        // Check that the end is within this sequence of basic blocks.
        if (findNodeWithEnd(cx, ni, end) == null_node) return null;
        return ni;
    }
}

fn checkCaseBranch(cx: *StructuringCx, ni: NodeIndex) ?struct { ExprIndex, NodeIndex, u16 } {
    const node = &cx.nodes.items[ni];
    if (node.kind != .basic_block) return null;
    if (node.kind.basic_block.exit != .jump_unless) return null;
    if (node.kind.basic_block.exit.jump_unless <= node.end) return null;

    const cond_stmt = cx.stmts.getPtr(node.kind.basic_block.statements.last());
    const cond = cx.exprs.getPtr(cond_stmt.jump_unless.condition);
    if (cond.* != .call) return null;
    if (cond.call.op != .eq and cond.call.op != .@"in-list") return null;
    // Both possible ops have args in the same place so just combine their logic
    std.debug.assert(cond.call.args.len == 2);
    const cond_args = getExtra2(cx.extra, cond.call.args);
    const cond_lhs = cx.exprs.getPtr(cond_args[0]);
    if (cond_lhs.* != .dup) return null;

    const ni_body_start = node.next;
    if (ni_body_start == null_node) return null;
    if (!nodeStartsWithPop(cx, ni_body_start, cond_lhs.dup)) return null;

    const ni_body_end = findNodeWithEnd(cx, ni_body_start, node.kind.basic_block.exit.jump_unless);
    if (ni_body_end == null_node) return null;
    const body_end = &cx.nodes.items[ni_body_end];
    if (body_end.kind != .basic_block) return null;
    if (body_end.kind.basic_block.exit != .jump) return null;
    if (body_end.kind.basic_block.exit.jump <= body_end.end) return null;

    const ni_next = body_end.next;
    if (ni_next == null_node) return null;

    return .{ cond_lhs.dup, ni_next, body_end.kind.basic_block.exit.jump };
}

fn checkCaseBranchAnother(
    cx: *StructuringCx,
    ni: NodeIndex,
    expected_value: ExprIndex,
    expected_end: u16,
) ?NodeIndex {
    const value, const ni_next, const end = checkCaseBranch(cx, ni) orelse return null;
    if (value != expected_value) return null;
    if (end != expected_end) return null;
    return ni_next;
}

fn nodeStartsWithPop(cx: *StructuringCx, ni: NodeIndex, expected_value: ExprIndex) bool {
    const node = &cx.nodes.items[ni];
    if (node.kind != .basic_block) return false;
    const node_stmts = node.kind.basic_block.statements;
    const stmts = cx.stmts.use()[node_stmts.start..][0..node_stmts.len];
    if (stmts.len == 0) return false;
    const pop_stmt = &stmts[0];
    if (pop_stmt.* != .call) return false;
    if (pop_stmt.call.op != .pop) return false;
    std.debug.assert(pop_stmt.call.args.len == 1);
    const pop_args = getExtra2(cx.extra, pop_stmt.call.args);
    if (pop_args[0] != expected_value) return false;
    return true;
}

fn makeCase(cx: *StructuringCx, ni: NodeIndex, ni_last: NodeIndex) !void {
    // convert node tree from this sequence of basic blocks:

    //    jump-unless a
    //    body w
    // a: jump-unless b
    //    body x
    // b: jump-unless c
    //    body y
    // c: body z

    // to this:

    // case
    //     branch a
    //         w
    //     branch b
    //         x
    //     branch c
    //         y
    //     branch else
    //         z

    // reusing nodes like so:

    // before after
    // -------------
    // j-u a  prior statements
    // (new)  case
    // (new)  branch a
    // w      w
    // j-u b  branch b
    // x      x
    // j-u c  branch c
    // y      y
    // (new)  branch else
    // z      z

    const node = &cx.nodes.items[ni];
    const ni_first_body_start = node.next;
    const ni_first_body_end = findNodeWithEnd(cx, ni, node.kind.basic_block.exit.jump_unless);

    const first_body_end = &cx.nodes.items[ni_first_body_end];
    const ni_second_branch = first_body_end.next;
    const case_end = first_body_end.kind.basic_block.exit.jump;

    const first_cond_index = chopJumpCondition(cx, ni);
    const first_cond = cx.exprs.getPtr(first_cond_index);
    std.debug.assert(first_cond.call.args.len == 2);
    const first_cond_args = getExtra2(cx.extra, first_cond.call.args);
    const case_value = cx.exprs.getPtr(first_cond_args[0]).dup;

    const ni_case = try appendNode(cx, .{
        .start = node.end,
        .end = case_end,
        .prev = ni,
        .next = undefined, // set below
        .kind = .{
            .case = .{
                .value = case_value,
                .first_branch = undefined, // set below
            },
        },
    });
    cx.nodes.items[ni].next = ni_case;

    const ni_first_branch = try appendNode(cx, .{
        .start = cx.nodes.items[ni].start,
        .end = cx.nodes.items[ni_first_body_end].end,
        .prev = null_node,
        .next = undefined, // set below
        .kind = .{ .case_branch = .{
            .value = first_cond_args[1],
            .body = ni_first_body_start,
        } },
    });
    cx.nodes.items[ni_case].kind.case.first_branch = ni_first_branch;

    cx.nodes.items[ni_first_body_start].prev = null_node;
    cx.nodes.items[ni_first_body_end].next = null_node;
    chopStartStmts(cx, ni_first_body_start, 1); // chop `pop`
    chopJump(cx, ni_first_body_end);

    var ni_prev_branch = ni_first_branch;
    var ni_cur = ni_second_branch;
    while (ni_cur != ni_last) {
        const cur = &cx.nodes.items[ni_cur];

        const ni_body_start = cur.next;
        const ni_body_end = findNodeWithEnd(cx, ni_cur, cur.kind.basic_block.exit.jump_unless);
        const ni_next = cx.nodes.items[ni_body_end].next;

        const cond_stmt = cx.stmts.getPtr(cur.kind.basic_block.statements.last());
        const cond = cx.exprs.getPtr(cond_stmt.jump_unless.condition);
        std.debug.assert(cond.call.args.len == 2);
        const cond_args = getExtra2(cx.extra, cond.call.args);

        cx.nodes.items[ni_cur] = .{
            .start = cur.start,
            .end = cx.nodes.items[ni_body_end].end,
            .prev = ni_prev_branch,
            .next = undefined, // set either in the next loop iteration or at the end
            .kind = .{ .case_branch = .{
                .value = cond_args[1],
                .body = ni_body_start,
            } },
        };
        cx.nodes.items[ni_prev_branch].next = ni_cur;

        cx.nodes.items[ni_body_start].prev = null_node;
        cx.nodes.items[ni_body_end].next = null_node;
        chopStartStmts(cx, cur.kind.case_branch.body, 1); // chop `pop`
        chopJump(cx, ni_body_end);

        ni_prev_branch = ni_cur;
        ni_cur = ni_next;
    }

    // Structure the final `else`

    const ni_body_start = ni_cur;
    const ni_body_end = findNodeWithEnd(cx, ni_cur, case_end);
    const ni_after = cx.nodes.items[ni_body_end].next;

    const else_start = cx.nodes.items[ni_body_start].start;
    const else_end = cx.nodes.items[ni_body_end].end;

    chopStartStmts(cx, ni_body_start, 1); // chop `pop`

    // Skip emitting the `else` block if it's empty
    if (cx.nodes.items[ni_body_start].start == cx.nodes.items[ni_body_end].end) {
        cx.nodes.items[ni_prev_branch].next = null_node;
        orphanNode(cx, ni_body_start);
    } else {
        const ni_else_branch = try appendNode(cx, .{
            .start = else_start,
            .end = else_end,
            .prev = ni_prev_branch,
            .next = null_node,
            .kind = .{ .case_branch = .{
                .value = null_expr,
                .body = ni_body_start,
            } },
        });
        cx.nodes.items[ni_prev_branch].next = ni_else_branch;

        cx.nodes.items[ni_body_start].prev = null_node;
        cx.nodes.items[ni_body_end].next = null_node;
    }

    cx.nodes.items[ni_case].next = ni_after;
    if (ni_after != null_node)
        cx.nodes.items[ni_after].prev = ni_case;

    structureCheckpoint(cx, "makeCase ni={} ni_last={}", .{ ni, ni_last });
}

fn findLastNode(cx: *StructuringCx, ni_initial: NodeIndex) NodeIndex {
    var ni = ni_initial;
    while (true) {
        const node = &cx.nodes.items[ni];
        if (node.next == null_node) return ni;
        ni = node.next;
    }
}

fn findNodeWithEnd(cx: *StructuringCx, ni_first: NodeIndex, end: u16) NodeIndex {
    var ni = ni_first;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        const node = &cx.nodes.items[ni];
        if (node.end == end) return ni;
        if (node.end > end) break;
    }
    return null_node;
}

fn findBackwardsNodeWithStart(cx: *StructuringCx, ni_initial: NodeIndex, start: u16) ?NodeIndex {
    var ni = ni_initial;
    while (ni != null_node) {
        const node = &cx.nodes.items[ni];
        if (node.start == start) return ni;
        if (node.start < start) break;
        ni = node.prev;
    }
    return null;
}

fn appendNode(cx: *StructuringCx, node: Node) !NodeIndex {
    const ni: NodeIndex = @intCast(cx.nodes.items.len);
    try cx.nodes.append(cx.gpa, (&node).*); // work around compiler bug
    return ni;
}

/// Move the given node to a new index, so its current index can be taken over
/// by a new node, and existing references will automatically point to the new
/// node without needing to update them manually.
fn moveNode(cx: *StructuringCx, ni_orig: NodeIndex) !NodeIndex {
    const ni_new = try appendNode(cx, cx.nodes.items[ni_orig]);
    cx.nodes.items[ni_orig] = undefined;

    if (niOpt(cx.nodes.items[ni_new].prev)) |ni_prev|
        cx.nodes.items[ni_prev].next = ni_new;
    if (niOpt(cx.nodes.items[ni_new].next)) |ni_next|
        cx.nodes.items[ni_next].prev = ni_new;

    try trackMoved(cx, ni_orig, ni_new);

    return ni_new;
}

fn orphanNode(cx: *StructuringCx, ni: NodeIndex) void {
    // In debug builds, when orphaning a node, update the links so it doesn't
    // trigger invariant violations.
    if (builtin.mode != .Debug) return;
    const node = &cx.nodes.items[ni];
    node.prev = null_node;
    node.next = null_node;
    node.kind = .orphan;
}

fn structureCheckpoint(cx: *StructuringCx, comptime fmt: []const u8, args: anytype) void {
    dumpNodes(cx, fmt, args) catch @panic("spew");
    checkInvariants(cx) catch @panic("invariant violation");
}

fn checkInvariants(cx: *StructuringCx) !void {
    if (builtin.mode != .Debug) return;

    for (cx.nodes.items, 0..) |*node, ni| {
        errdefer std.debug.print("broken node: {}\n", .{ni});

        if (node.prev != null_node) {
            const prev = &cx.nodes.items[node.prev];
            try std.testing.expect(prev.next == ni);
            try std.testing.expect(prev.end == node.start);
        }
        if (node.next != null_node) {
            const next = &cx.nodes.items[node.next];
            try std.testing.expect(next.prev == ni);
            try std.testing.expect(next.start == node.end);
        }
        if (node.kind == .basic_block) {
            const ss = node.kind.basic_block.statements;
            if (ss.len != 0)
                try std.testing.expect(node.end == cx.stmt_ends.get(ss.last()));
        }
    }
}

fn dumpNodes(cx: *StructuringCx, comptime fmt: []const u8, args: anytype) !void {
    if (true) return;

    std.Progress.lockStdErr();
    defer std.Progress.unlockStdErr();

    const out = std.io.getStdErr().writer();
    try out.writeAll("-------------------- ");
    try out.print(fmt, args);
    try out.writeByte('\n');
    try dumpNodesInner(cx, out);
}

fn dumpNodesInner(cx: *StructuringCx, out: anytype) !void {
    const Item = struct {
        index: usize,
        node: Node,

        fn lt(_: void, a: @This(), b: @This()) bool {
            if (std.math.order(a.node.start, b.node.start).differ()) |x| return x == .lt;
            if (std.math.order(a.node.end, b.node.end).differ()) |x| return x == .lt;
            return false;
        }
    };

    const FormatNodeIndex = struct {
        value: NodeIndex,

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            comptime std.debug.assert(fmt.len == 0);

            if (self.value != null_node)
                try std.fmt.formatIntValue(self.value, "", options, writer)
            else
                try std.fmt.formatText("-", "s", options, writer);
        }
    };

    const fni = struct {
        fn f(ni: NodeIndex) FormatNodeIndex {
            return .{ .value = ni };
        }
    }.f;

    const FormatExit = struct {
        value: BasicBlockExit,

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            comptime std.debug.assert(fmt.len == 0);
            _ = options; // XXX: should not ignore this
            switch (self.value) {
                .no_jump => try writer.writeAll("no_jump"),
                inline else => |target, tag| {
                    try writer.print("{s}:0x{x:0>4}", .{ @tagName(tag), target });
                },
            }
        }
    };

    const items = try cx.gpa.alloc(Item, cx.nodes.items.len);
    defer cx.gpa.free(items);
    for (items, cx.nodes.items, 0..) |*item, *node, ni|
        item.* = .{ .index = ni, .node = node.* };
    std.mem.sort(Item, items, {}, Item.lt);

    for (items) |item| {
        const ni = item.index;
        const node = item.node;
        try out.print(
            "{:>2}: {s:<11} {:>2}/{:>2} 0x{x:0>4}-0x{x:0>4} ",
            .{ ni, @tagName(node.kind), fni(node.prev), fni(node.next), node.start, node.end },
        );
        switch (node.kind) {
            .basic_block => |*n| try out.print("exit={}", .{FormatExit{ .value = n.exit }}),
            .@"if" => |*n| try out.print("true={}", .{fni(n.true)}),
            .if_else => |*n| try out.print("true={} false={}", .{ fni(n.true), fni(n.false) }),
            .@"while" => |*n| try out.print("body={}", .{fni(n.body)}),
            .@"for" => |*n| try out.print("body={}", .{fni(n.body)}),
            .for_in => |*n| try out.print("body={}", .{fni(n.body)}),
            .do => |*n| try out.print("body={}", .{fni(n.body)}),
            .case => |*n| try out.print("first={}", .{fni(n.first_branch)}),
            .case_branch => |*n| try out.print("body={}", .{fni(n.body)}),
            .orphan => {},
        }
        try out.writeByte('\n');
    }
}

const jump_len = 3;

fn chopJump(cx: *StructuringCx, ni: NodeIndex) void {
    const node = &cx.nodes.items[ni];

    if (builtin.mode == .Debug) {
        const ss = node.kind.basic_block.statements;
        const stmt = cx.stmts.getPtr(ss.last());
        std.debug.assert(stmt.* == .jump);
    }

    std.debug.assert(node.kind.basic_block.exit == .jump);

    node.kind.basic_block.exit = .no_jump;
    chopEndStmts(cx, ni, 1);
}

fn chopJumpCondition(cx: *StructuringCx, ni: NodeIndex) ExprIndex {
    const node = &cx.nodes.items[ni];

    const ss = node.kind.basic_block.statements;
    const stmt = cx.stmts.getPtr(ss.last());
    const condition = switch (stmt.*) {
        .jump_if, .jump_unless => |j| j.condition,
        else => unreachable,
    };

    std.debug.assert(node.kind.basic_block.exit == .jump_if or
        node.kind.basic_block.exit == .jump_unless);

    node.kind.basic_block.exit = .no_jump;
    chopEndStmts(cx, ni, 1);
    return condition;
}

fn chopStartStmts(cx: *StructuringCx, ni: NodeIndex, len: u16) void {
    const node = &cx.nodes.items[ni];
    const ss = &node.kind.basic_block.statements;

    std.debug.assert(len >= 1);

    node.start = cx.stmt_ends.get(ss.start + len - 1);
    ss.start += len;
    ss.len -= len;
}

fn chopEndStmts(cx: *StructuringCx, ni: NodeIndex, len: u16) void {
    const node = &cx.nodes.items[ni];
    const ss = &node.kind.basic_block.statements;

    std.debug.assert(len >= 1);
    std.debug.assert(node.kind.basic_block.exit == .no_jump);

    ss.len -= len;
    node.end = if (ss.len == 0)
        node.start
    else
        cx.stmt_ends.get(ss.last());
}

fn getExtra2(
    extra: *const std.ArrayListUnmanaged(ExprIndex),
    slice: ExtraSlice,
) []const ExprIndex {
    return extra.items[slice.start..][0..slice.len];
}

fn storeExtra2(
    gpa: std.mem.Allocator,
    extra: *std.ArrayListUnmanaged(ExprIndex),
    items: []const ExprIndex,
) !ExtraSlice {
    const start: u16 = @intCast(extra.items.len);
    const len: u16 = @intCast(items.len);
    try extra.appendSlice(gpa, items);
    return .{ .start = start, .len = len };
}

const FindJumpTargetsCx = struct {
    gpa: std.mem.Allocator,
    nodes: utils.SafeManyPointer([*]const Node),
    result: std.ArrayListUnmanaged(u16),
};

fn findJumpTargets(
    gpa: std.mem.Allocator,
    nodes: []const Node,
) !std.ArrayListUnmanaged(u16) {
    var cx: FindJumpTargetsCx = .{
        .gpa = gpa,
        .nodes = .init(nodes),
        .result = .empty,
    };
    errdefer cx.result.deinit(cx.gpa);
    try cx.result.ensureTotalCapacity(gpa, nodes.len / 32);

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
                .jump, .jump_if, .jump_unless, .override => |target| target,
            };
            try insertSortedNoDup(cx.gpa, &cx.result, target);
        },
        .@"if" => |*n| {
            try findJumpTargetsInNodeList(cx, n.true);
        },
        .if_else => |*n| {
            try findJumpTargetsInNodeList(cx, n.true);
            try findJumpTargetsInNodeList(cx, n.false);
        },
        .@"while" => |*n| {
            try findJumpTargetsInNodeList(cx, n.body);
        },
        .do => |*n| {
            try findJumpTargetsInNodeList(cx, n.body);
        },
        .@"for" => |*n| {
            try findJumpTargetsInNodeList(cx, n.body);
        },
        .for_in => |*n| {
            try findJumpTargetsInNodeList(cx, n.body);
        },
        .case => |*n| {
            try findJumpTargetsInNodeList(cx, n.first_branch);
        },
        .case_branch => |*n| {
            try findJumpTargetsInNodeList(cx, n.body);
        },
        .orphan => unreachable,
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
    diag: *const Diagnostic.ForBinaryFile,
    symbols: *const Symbols,
    room_number: u8,
    id: Symbols.ScriptId,
    nodes: utils.SafeManyPointer([*]const Node),
    stmts: utils.SafeManyPointer([*]const Stmt),
    stmt_ends: ?utils.SafeManyPointer([*]const u16),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    index: *const Index,
    lsc_mask: *const UsageTracker.LocalScripts,
    local_var_usage: *const UsageTracker.LocalVars,
    types: *const ArrayMap(Symbols.TypeIndex),
    jump_targets: []const u16,

    out: *std.ArrayListUnmanaged(u8),
    indent: u16,
};

fn emitScript(cx: *EmitCx) !void {
    try emitLocalVarsDecl(cx);
    try emitNodeList(cx, root_node_index);
}

fn emitLocalVarsDecl(cx: *EmitCx) !void {
    const num_used = num_used: {
        var i: usize = UsageTracker.max_local_vars;
        break :num_used while (i > 0) {
            i -= 1;
            const used = UsageTracker.get(cx.local_var_usage, i);
            if (used) break i + 1;
        } else 0;
    };

    const num_params = num_params: {
        // Only global scripts and local scripts can have params
        switch (cx.id) {
            .global, .local => {},
            .enter, .exit, .object => break :num_params 0,
        }
        const symbol = cx.symbols.getScript(cx.id) orelse break :num_params 0;
        break :num_params symbol.params orelse 0;
    };

    for (0..num_params) |num_usize|
        try emitLocalVarDecl(cx, @intCast(num_usize));

    try cx.out.appendSlice(cx.gpa, " {\n");

    if (num_used > num_params) {
        try writeIndent(cx, null);
        try cx.out.appendSlice(cx.gpa, "var");
        for (num_params..num_used) |num_usize|
            try emitLocalVarDecl(cx, @intCast(num_usize));
        try cx.out.appendSlice(cx.gpa, "\n\n");
    }
}

fn emitLocalVarDecl(cx: *EmitCx, num: u14) !void {
    try cx.out.append(cx.gpa, ' ');
    const used = UsageTracker.get(cx.local_var_usage, num);
    if (!used) {
        try cx.out.append(cx.gpa, '_');
        return;
    }
    try emitVariable(cx, .init(.local, num));
}

fn emitNodeList(cx: *EmitCx, ni_start: NodeIndex) error{ OutOfMemory, BadData }!void {
    var ni = ni_start;
    while (ni != null_node) {
        try emitSingleNode(cx, ni, false);
        ni = cx.nodes.getPtr(ni).next;
    }
}

fn emitSingleNode(cx: *EmitCx, ni: NodeIndex, skip_first_indent: bool) !void {
    const node = cx.nodes.getPtr(ni);

    // skip_first_indent is only ever used for those two
    std.debug.assert(!skip_first_indent or node.kind == .@"if" or node.kind == .if_else);

    switch (node.kind) {
        .basic_block => |bb| {
            // TODO: keep track of list position instead of searching every time
            if (std.sort.binarySearch(u16, cx.jump_targets, node.start, orderU16) != null) {
                try writeIndent(cx, null);
                try emitLabel(cx, node.start);
                try cx.out.appendSlice(cx.gpa, ":\n");
            }

            const stmts = cx.stmts.use()[bb.statements.start..][0..bb.statements.len];
            for (stmts, bb.statements.start..) |*stmt, i| {
                const start = if (cx.stmt_ends) |*stmt_ends|
                    if (i == bb.statements.start) node.start else stmt_ends.get(i - 1)
                else
                    null;
                try emitStmt(cx, start, stmt);
            }
        },
        .@"if" => |k| {
            if (!skip_first_indent)
                try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "if ");
            try emitExpr(cx, k.condition, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, k.true);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .if_else => |k| {
            if (!skip_first_indent)
                try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "if ");
            try emitExpr(cx, k.condition, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, k.true);
            cx.indent -= indent_size;
            try writeIndent(cx, cx.nodes.getPtr(k.true).end);
            try cx.out.appendSlice(cx.gpa, "} else ");

            if (shouldEmitElseIf(cx, k.false)) |child| {
                // TODO: make this a tail call again once stage2 supports it
                return emitSingleNode(cx, child, true);
            }

            try cx.out.appendSlice(cx.gpa, "{\n");
            cx.indent += indent_size;
            try emitNodeList(cx, k.false);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .@"while" => |n| {
            try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "while ");
            try emitExpr(cx, n.condition, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .@"for" => |*n| {
            try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "for ");
            try emitVariable(cx, n.accumulator);
            try cx.out.appendSlice(cx.gpa, " = ");
            try emitExpr(cx, n.start, .space);
            try cx.out.appendSlice(cx.gpa, " to ");
            try emitExpr(cx, n.end, .space);
            try cx.out.append(cx.gpa, ' ');
            try cx.out.append(cx.gpa, if (n.direction == .up) '+' else '-');
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .for_in => |*n| {
            try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "for ");
            try emitVariable(cx, n.target);
            try cx.out.appendSlice(cx.gpa, " = {");
            try emitVariable(cx, n.backing);
            try cx.out.append(cx.gpa, '}');
            try emitExpr(cx, n.list, .all);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .do => |n| {
            try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "do {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            if (n.condition == null_expr) {
                try cx.out.appendSlice(cx.gpa, "}\n");
            } else {
                try cx.out.appendSlice(cx.gpa, "} until ");
                try emitExpr(cx, n.condition, .space);
                try cx.out.appendSlice(cx.gpa, "\n");
            }
        },
        .case => |*n| {
            try writeIndent(cx, node.start);
            try cx.out.appendSlice(cx.gpa, "case ");
            try emitExpr(cx, n.value, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.first_branch);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .case_branch => |*n| {
            try writeIndent(cx, node.start);
            if (n.value == null_expr)
                try cx.out.appendSlice(cx.gpa, "else")
            else if (cx.exprs.getPtr(n.value).* == .list)
                try emitList(cx, cx.exprs.getPtr(n.value).list)
            else
                try emitExpr(cx, n.value, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx, node.end);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .orphan => unreachable,
    }
}

fn shouldEmitElseIf(cx: *EmitCx, ni: NodeIndex) ?NodeIndex {
    if (ni == null_node) return null;
    const node = cx.nodes.getPtr(ni);

    // Due to a quirk of how if nodes are structured, in this case there will be
    // an empty basic block before the one we want to check.
    if (node.start != node.end) return null;
    if (node.kind != .basic_block) return null;
    // Don't skip this basic block if it needs a label (e.g. Backyard Baseball 2001 scr201)
    if (std.sort.binarySearch(u16, cx.jump_targets, node.start, orderU16) != null) return null;

    // Check for exactly one node which must be if or if-else
    if (node.next == null_node) return null;
    const real = cx.nodes.getPtr(node.next);
    if (real.next != null_node) return null;
    if (real.kind != .@"if" and real.kind != .if_else) return null;

    return node.next;
}

fn emitStmt(cx: *const EmitCx, start: ?u16, stmt: *const Stmt) !void {
    if (stmt.* == .tombstone) return;
    try writeIndent(cx, start);
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
        .override => |j| {
            try cx.out.appendSlice(cx.gpa, @tagName(lang.Op.override));
            try cx.out.append(cx.gpa, ' ');
            try emitLabel(cx, j.target);
        },
        .call => |call| if (call.op == .set) {
            const args = getExtra(cx, call.args);
            try emitExpr(cx, args[0], .all);
            try cx.out.appendSlice(cx.gpa, " = ");
            try emitExpr(cx, args[1], .all);
        } else if (call.op == .@"set-array-item") {
            const args = getExtra(cx, call.args);
            try emitExpr(cx, args[0], .all);
            try cx.out.append(cx.gpa, '[');
            try emitExpr(cx, args[1], .all);
            try cx.out.appendSlice(cx.gpa, "] = ");
            try emitExpr(cx, args[2], .all);
        } else if (call.op == .@"set-array-item-2d") {
            const args = getExtra(cx, call.args);
            try emitExpr(cx, args[0], .all);
            try cx.out.append(cx.gpa, '[');
            try emitExpr(cx, args[1], .all);
            try cx.out.appendSlice(cx.gpa, "][");
            try emitExpr(cx, args[2], .all);
            try cx.out.appendSlice(cx.gpa, "] = ");
            try emitExpr(cx, args[3], .all);
        } else {
            try emitCall(cx, @tagName(call.op), call.args);
        },
        .compound => |c| {
            try emitCall(cx, @tagName(c.op), c.args);
        },
        .binop_assign => |s| {
            const args = getExtra(cx, s.args);
            try emitExpr(cx, args[0], .space);
            try cx.out.append(cx.gpa, ' ');
            try cx.out.appendSlice(cx.gpa, s.op.str());
            try cx.out.appendSlice(cx.gpa, "= ");
            try emitExpr(cx, args[1], .all);
        },
        .tombstone => unreachable,
    }
    try cx.out.append(cx.gpa, '\n');
}

fn emitExpr(
    cx: *const EmitCx,
    ei: ExprIndex,
    prec: Precedence,
) error{ OutOfMemory, BadData }!void {
    switch (cx.exprs.get(ei)) {
        .int => try emitInt(cx, ei),
        .variable => |v| try emitVariable(cx, v),
        .string => |s| {
            try cx.out.append(cx.gpa, '"');
            try emitStringContents(cx.gpa, cx.out, s);
            try cx.out.append(cx.gpa, '"');
        },
        .call => |call| {
            const args = getExtra(cx, call.args);
            if (call.op == .@"get-array-item") {
                try emitExpr(cx, args[0], .all);
                try cx.out.append(cx.gpa, '[');
                try emitExpr(cx, args[1], .all);
                try cx.out.append(cx.gpa, ']');
            } else if (call.op == .@"get-array-item-2d") {
                try emitExpr(cx, args[0], .all);
                try cx.out.append(cx.gpa, '[');
                try emitExpr(cx, args[1], .all);
                try cx.out.appendSlice(cx.gpa, "][");
                try emitExpr(cx, args[2], .all);
                try cx.out.append(cx.gpa, ']');
            } else if (binOp(call.op)) |op| {
                const op_prec = op.precedence();
                if (@intFromEnum(prec) >= @intFromEnum(op_prec))
                    try cx.out.append(cx.gpa, '(');
                try emitExpr(cx, args[0], op_prec.oneLower());
                try cx.out.writer(cx.gpa).print(" {s} ", .{op.str()});
                try emitExpr(cx, args[1], op_prec);
                if (@intFromEnum(prec) >= @intFromEnum(op_prec))
                    try cx.out.append(cx.gpa, ')');
            } else {
                const parens =
                    @intFromEnum(prec) >= @intFromEnum(Precedence.space) or
                    call.args.len == 0;
                if (parens)
                    try cx.out.append(cx.gpa, '(');
                try emitCall(cx, @tagName(call.op), call.args);
                if (parens)
                    try cx.out.append(cx.gpa, ')');
            }
        },
        .list => |items| try emitList(cx, items),
        .variadic_list => unreachable, // only appears in call args, handled elsewhere
        .dup => |child| {
            if (cx.stmt_ends == null) return error.BadData;
            // With --annotate, dump the target expr for help debugging
            try cx.out.appendSlice(cx.gpa, "#dup{");
            try emitExpr(cx, child, .all);
            try cx.out.append(cx.gpa, '}');
        },
        .stack_fault => {
            if (cx.stmt_ends == null) return error.BadData;
            // With --annotate, dump the target expr for help debugging
            try cx.out.appendSlice(cx.gpa, "#stackfault");
        },
    }
}

pub fn emitStringContents(
    gpa: std.mem.Allocator,
    out: *std.ArrayListUnmanaged(u8),
    str: []const u8,
) !void {
    for (str) |c| {
        if (c == '\\')
            try out.appendSlice(gpa, "\\\\")
        else if (32 <= c and c <= 126)
            try out.append(gpa, c)
        else
            try out.writer(gpa).print("\\x{x:0>2}", .{c});
    }
}

fn binOp(op: lang.Op) ?Ast.BinOp {
    return switch (op) {
        .eq => .eq,
        .ne => .ne,
        .gt => .gt,
        .lt => .lt,
        .le => .le,
        .ge => .ge,
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .land => .land,
        .lor => .lor,
        .mod => .mod,
        .shl => .shl,
        .shr => .shr,
        else => null,
    };
}

fn emitInt(cx: *const EmitCx, ei: ExprIndex) !void {
    const int = cx.exprs.getPtr(ei).int;
    if (cx.types.get(ei)) |ti| write_name: switch (cx.symbols.types.items[ti]) {
        .char => {
            if (!(32 <= int and int < 127)) break :write_name;
            if (int == '\'') break :write_name; // i gotta handle escaping first
            try cx.out.writer(cx.gpa).print("'{c}'", .{@as(u8, @intCast(int))});
            return;
        },
        .room => {
            const num = std.math.cast(u8, int) orelse break :write_name;
            const name = cx.index.room_names.get(num) orelse break :write_name;
            try cx.out.appendSlice(cx.gpa, name);
            return;
        },
        .script => {
            const num = std.math.cast(u32, int) orelse break :write_name;
            if (num < games.firstLocalScript(cx.symbols.game)) {
                // it's a global script
                const valid = num < cx.index.maxs.scripts and
                    cx.index.directories.scripts.rooms.get(num) != 0;
                if (!valid) {
                    if (num != 0)
                        cx.diag.info(0, "reference to missing script {}", .{num});
                    break :write_name;
                }
            } else {
                // it's a local script
                const index = num - games.firstLocalScript(cx.symbols.game);
                if (index >= UsageTracker.max_local_scripts) break :write_name;
                const valid = std.mem.readPackedInt(u1, std.mem.asBytes(cx.lsc_mask), index, .little) != 0;
                if (!valid) {
                    cx.diag.info(0, "reference to missing script {}", .{num});
                    break :write_name;
                }
            }
            try cx.out.print(cx.gpa, "{f}", .{cx.symbols.fmtScriptName(cx.room_number, num)});
            return;
        },
        .@"enum" => |enum_index| {
            const the_enum = &cx.symbols.enums.items[enum_index];
            const index = std.sort.binarySearch(
                Symbols.EnumEntry,
                the_enum.entries.items,
                int,
                Symbols.EnumEntry.orderByValue,
            ) orelse break :write_name;
            const entry = &the_enum.entries.items[index];
            try cx.out.appendSlice(cx.gpa, entry.name);
            return;
        },
        .map => |map_index| {
            const map = cx.symbols.maps.at(map_index);
            const index = std.sort.binarySearch(
                Symbols.MapEntry,
                map.entries.items,
                int,
                Symbols.MapEntry.orderByValue,
            ) orelse break :write_name;
            const entry = &map.entries.items[index];
            const name = entry.name orelse break :write_name;
            try cx.out.appendSlice(cx.gpa, name);
            return;
        },
        .array => {},
        .sound => if (try emitIntAsGlob(cx, .sound, int)) return,
        .costume => if (try emitIntAsGlob(cx, .costume, int)) return,
        .charset => if (try emitIntAsGlob(cx, .charset, int)) return,
        .image => if (try emitIntAsGlob(cx, .image, int)) return,
        .talkie => if (try emitIntAsGlob(cx, .talkie, int)) return,
    };
    try cx.out.writer(cx.gpa).print("{}", .{int});
}

fn emitIntAsGlob(cx: *const EmitCx, kind: Symbols.GlobKind, int: i32) !bool {
    const dir, const dir_len = cx.index.directory(kind);
    const num = std.math.cast(u32, int) orelse return false;
    if (num >= dir_len) return false;
    if (dir.rooms.get(num) == 0) return false;

    try cx.symbols.writeGlobName(kind, num, cx.out.writer(cx.gpa));
    return true;
}

fn emitCall(cx: *const EmitCx, op: []const u8, args: ExtraSlice) !void {
    try cx.out.appendSlice(cx.gpa, op);
    try emitArgsFlat(cx, args);
}

fn emitArgsFlat(cx: *const EmitCx, items: ExtraSlice) !void {
    for (getExtra(cx, items)) |ei| {
        const arg = cx.exprs.getPtr(ei);
        if (arg.* == .variadic_list) {
            try emitArgsFlat(cx, arg.variadic_list);
            continue;
        }
        try cx.out.append(cx.gpa, ' ');
        try emitExpr(cx, ei, .space);
    }
}

fn emitList(cx: *const EmitCx, items: ExtraSlice) !void {
    try cx.out.append(cx.gpa, '[');
    for (getExtra(cx, items), 0..) |ei, i| {
        if (i != 0)
            try cx.out.append(cx.gpa, ' ');
        try emitExpr(cx, ei, .space);
    }
    try cx.out.append(cx.gpa, ']');
}

fn emitVariable(cx: *const EmitCx, variable: lang.Variable) !void {
    try cx.out.print(cx.gpa, "{f}", .{cx.symbols.fmtVariableName(cx.room_number, cx.id, variable)});
}

fn emitLabel(cx: *const EmitCx, pc: u16) !void {
    try cx.out.writer(cx.gpa).print("L{x:0>4}", .{pc});
}

fn writeIndent(cx: *const EmitCx, annotation: ?u16) !void {
    if (cx.stmt_ends != null) {
        if (annotation) |ann| {
            try cx.out.writer(cx.gpa).print("0x{x:0>4}  ", .{ann});
        } else {
            const bytes = try cx.out.addManyAsSlice(cx.gpa, 8);
            @memset(bytes, ' ');
        }
    }

    const bytes = try cx.out.addManyAsSlice(cx.gpa, cx.indent);
    @memset(bytes, ' ');
}

fn getExtra(cx: *const EmitCx, slice: ExtraSlice) []const ExprIndex {
    return cx.extra.use()[slice.start..][0..slice.len];
}
