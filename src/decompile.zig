const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const games = @import("games.zig");
const lang = @import("lang.zig");
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    game: games.Game,
    bytecode: []const u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    // This code uses u16 all over the place. Make sure everything will fit.
    if (bytecode.len > 0xffff) return error.BadData;

    const language = lang.buildLanguage(game);
    var dcx: DecompileCx = .{
        .gpa = gpa,
        .diag = diag,
        .language = &language,
        .bytecode = bytecode,
        .stack = .{},
        .stmts = .empty,
        .exprs = .empty,
        .extra = .empty,
    };
    defer dcx.extra.deinit(gpa);
    defer dcx.exprs.deinit(gpa);
    defer dcx.stmts.deinit(gpa);

    try decompile(&dcx);

    var ecx: EmitCx = .{
        .gpa = gpa,
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .out = out,
    };
    for (dcx.stmts.items) |*stmt|
        try emitStmt(&ecx, stmt);
}

const DecompileCx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    language: *const lang.Language,
    bytecode: []const u8,

    stack: std.BoundedArray(ExprIndex, 8),
    stmts: std.ArrayListUnmanaged(Stmt),
    exprs: std.ArrayListUnmanaged(Expr),
    extra: std.ArrayListUnmanaged(ExprIndex),
};

const Stmt = union(enum) {
    call: struct { op: lang.Op, args: ExtraSlice },
};

const ExprIndex = u16;

const Expr = union(enum) {
    int: i32,
    variable: lang.Variable,
};

const ExtraSlice = struct {
    start: u16,
    len: u16,
};

const Op = union(enum) {
    push8,
    push16,
    push_var,
    generic: struct {
        op: lang.Op,
        params: std.BoundedArray(Param, max_params),
    },

    fn gen(op: lang.Op, params: []const Param) Op {
        return .{ .generic = .{
            .op = op,
            .params = std.BoundedArray(Param, max_params).fromSlice(params) catch unreachable,
        } };
    }
};

const max_params = 1;

const Param = union(enum) {
    int,
};

const ops: std.EnumArray(lang.Op, Op) = .init(.{
    .@"push-u8" = .push8,
    .@"push-i16" = .push16,
    .@"push-var" = .push_var,
    .set = .gen(.set, &.{.int}),
    .end = .gen(.end, &.{}),
    .@"return" = .gen(.@"return", &.{.int}),
});

fn decompile(cx: *DecompileCx) !void {
    var disasm: lang.Disasm = .init(cx.language, cx.bytecode);
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
                const ei = try storeExpr(cx, .{ .int = ins.operands.get(0).u8 });
                try cx.stack.append(ei);
            },
            .push16 => {
                const ei = try storeExpr(cx, .{ .int = ins.operands.get(0).i16 });
                try cx.stack.append(ei);
            },
            .push_var => {
                const ei = try storeExpr(cx, .{ .variable = ins.operands.get(0).variable });
                try cx.stack.append(ei);
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
                for (gen.params.slice()) |param| {
                    comptime std.debug.assert(param == .int);
                    const ei = cx.stack.pop() orelse return error.BadData;
                    args.appendAssumeCapacity(ei);
                }
                try cx.stmts.append(cx.gpa, .{ .call = .{
                    .op = op,
                    .args = try storeExtra(cx, args.slice()),
                } });
            },
        }
    }
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
        .call => |call| {
            try cx.out.appendSlice(cx.gpa, @tagName(call.op));
            for (getExtra(cx, call.args)) |ei| {
                try cx.out.append(cx.gpa, ' ');
                try emitExpr(cx, ei);
            }
        },
    }
    try cx.out.append(cx.gpa, '\n');
}

fn emitExpr(cx: *const EmitCx, ei: ExprIndex) !void {
    switch (cx.exprs.get(ei)) {
        .int => |int| try cx.out.writer(cx.gpa).print("{}", .{int}),
        .variable => |v| try emitVariable(cx, v),
    }
}

fn emitVariable(cx: *const EmitCx, variable: lang.Variable) !void {
    const kind, const number = try variable.decode2();
    try cx.out.writer(cx.gpa).print("{s}{}", .{ @tagName(kind), number });
}

fn getExtra(cx: *const EmitCx, slice: ExtraSlice) []const ExprIndex {
    return cx.extra.use()[slice.start..][0..slice.len];
}
