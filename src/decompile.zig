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

    const basic_blocks = try scanBasicBlocks(gpa, &language, bytecode);
    defer gpa.free(basic_blocks);

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

    for (basic_blocks, 0..) |*bb, i| {
        const bb_start = if (i == 0) 0 else basic_blocks[i - 1].end;
        try decompile(&dcx, bytecode, bb, bb_start);
    }

    var ecx: EmitCx = .{
        .gpa = gpa,
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .out = out,
    };
    for (basic_blocks, 0..) |*bb, i| {
        const bb_start = if (i == 0) 0 else basic_blocks[i - 1].end;

        try emitLabel(&ecx, bb_start);
        try out.appendSlice(gpa, ":\n");

        const ss = bb.statements.defined;
        for (dcx.stmts.items[ss.start..][0..ss.len]) |*stmt|
            try emitStmt(&ecx, stmt);
    }
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
) ![]BasicBlock {
    var ends: std.ArrayListUnmanaged(u16) = .empty;
    defer ends.deinit(gpa);

    var disasm: lang.Disasm = .init(language, bytecode);
    while (try disasm.next()) |ins| {
        if (ins.operands.len == 0) continue;
        if (ins.operands.get(0) != .relative_offset) continue;
        const rel = ins.operands.get(0).relative_offset;
        const target = utils.addUnsignedSigned(ins.end, rel) orelse return error.BadData;
        if (target >= bytecode.len) return error.BadData;
        try insertSortedNoDup(gpa, &ends, ins.end);
        try insertSortedNoDup(gpa, &ends, target);
    }
    try ends.append(gpa, @intCast(bytecode.len));

    const blocks = try gpa.alloc(BasicBlock, ends.items.len);
    errdefer gpa.free(blocks);

    for (ends.items, blocks) |end, *block|
        block.* = .{
            .end = end,
            .statements = .undef,
        };

    return blocks;
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

const BasicBlock = struct {
    end: u16,
    statements: utils.SafeUndefined(ExtraSlice),
};

const Stmt = union(enum) {
    jump_unless: struct { target: u16, condition: ExprIndex },
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

const max_params = 3;

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
    .set = .gen(&.{.int}),
    .@"set-array-item" = .gen(&.{ .int, .int }),
    .inc = .gen(&.{}),
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
    .@"break-here" = .gen(&.{}),
    .@"current-room" = .gen(&.{.int}),
    .random = .genCall(&.{.int}),
    .debug = .gen(&.{.int}),
    .@"sleep-for-seconds" = .gen(&.{.int}),
    .@"stop-sentence" = .gen(&.{}),
    .@"dim-array.int8" = .gen(&.{.int}),
    .@"dim-array.int16" = .gen(&.{.int}),
    .undim = .gen(&.{}),
    .@"return" = .gen(&.{.int}),
    .@"call-script" = .genCall(&.{ .int, .list }),
    .@"kludge-call" = .genCall(&.{.list}),
    .@"break-here-multi" = .gen(&.{.int}),
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
                const target = utils.addUnsignedSigned(ins.end, rel) orelse return error.BadData;
                const condition = try pop(cx);
                try cx.stmts.append(cx.gpa, .{ .jump_unless = .{
                    .target = target,
                    .condition = condition,
                } });
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

const EmitCx = struct {
    gpa: std.mem.Allocator,
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    out: *std.ArrayListUnmanaged(u8),
};

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
