const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const UsageTracker = @import("UsageTracker.zig");
const ArrayMap = @import("array_map.zig").ArrayMap;
const games = @import("games.zig");
const lang = @import("lang.zig");
const Precedence = @import("parser.zig").Precedence;
const script = @import("script.zig");
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    symbols: *const Symbols,
    room_number: u8,
    id: Symbols.ScriptId,
    bytecode: []const u8,
    out: *std.ArrayListUnmanaged(u8),
    usage: *UsageTracker,
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
        .usage = usage,
    };
    defer dcx.extra.deinit(gpa);
    defer dcx.exprs.deinit(gpa);
    defer dcx.stmts.deinit(gpa);
    defer dcx.pending_basic_blocks.deinit(gpa);

    try decompileBasicBlocks(&dcx, bytecode);

    var tcx: TypeCx = .{
        .basic_blocks = dcx.basic_blocks,
        .stmts = .init(dcx.stmts.items),
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .types = try .initCapacity(gpa, dcx.exprs.items.len),
    };
    defer tcx.types.deinit(gpa);
    recoverTypes(&tcx);

    peephole(&dcx);

    var scx: StructuringCx = .{
        .gpa = gpa,
        .stmts = .init(dcx.stmts.items),
        .exprs = .init(dcx.exprs.items),
        .extra = &dcx.extra,
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
        .room_number = room_number,
        .id = id,
        .nodes = .init(scx.nodes.items),
        .stmts = .init(dcx.stmts.items),
        .exprs = .init(dcx.exprs.items),
        .extra = .init(dcx.extra.items),
        .local_var_usage = &usage.local_vars,
        .types = &tcx.types,
        .jump_targets = jump_targets.items,
        .out = out,
        .indent = indent_size * 1,
    };
    try emitScript(&ecx);
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
    jump: u16,
    jump_if: u16,
    jump_unless: u16,
    override: u16,
};

const DecompileCx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    language: *const lang.Language,
    basic_blocks: []BasicBlock,

    pending_basic_blocks: std.ArrayListUnmanaged(u16),
    stack: std.BoundedArray(ExprIndex, 24),
    str_stack: std.BoundedArray(ExprIndex, 2),
    stmts: std.ArrayListUnmanaged(Stmt),
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
    tombstone,
};

const JumpTargetAndCondition = struct {
    target: u16,
    condition: ExprIndex,
};

const ExprIndex = u16;
const null_expr = 0xffff;

const Expr = union(enum) {
    int: i32,
    string: []const u8,
    variable: lang.Variable,
    call: struct { op: lang.Op, args: ExtraSlice },
    list: ExtraSlice,
    variadic_list: ExtraSlice,
    dup: ExprIndex,
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
    jump_if,
    jump_unless,
    jump,
    override,
    generic: struct {
        call: bool,
        params: std.BoundedArray(script.Param, script.max_params),
    },

    fn gen(params: []const script.Param) Op {
        return .{ .generic = .{
            .call = false,
            .params = std.BoundedArray(script.Param, script.max_params).fromSlice(params) catch unreachable,
        } };
    }

    fn genCall(params: []const script.Param) Op {
        @setEvalBranchQuota(2000);
        return .{ .generic = .{
            .call = true,
            .params = std.BoundedArray(script.Param, script.max_params).fromSlice(params) catch unreachable,
        } };
    }
};

pub const ops: std.EnumArray(lang.Op, Op) = initEnumArrayFixed(lang.Op, Op, .{
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
    .pop = .gen(&.{.int}),
    .@"in-list" = .genCall(&.{ .int, .list }),
    .@"image-set-width" = .gen(&.{.int}),
    .@"image-set-height" = .gen(&.{.int}),
    .@"image-draw" = .gen(&.{}),
    .@"image-capture" = .gen(&.{ .int, .int, .int, .int, .int }),
    .@"image-set-state" = .gen(&.{.int}),
    .@"image-select" = .gen(&.{.int}),
    .@"image-set-pos" = .gen(&.{ .int, .int }),
    .@"image-set-palette" = .gen(&.{.int}),
    .@"image-set-shadow" = .gen(&.{.int}),
    .@"image-set-draw-box" = .gen(&.{ .int, .int, .int, .int, .int }),
    .@"image-set-render-image" = .gen(&.{.int}),
    .@"image-new" = .gen(&.{}),
    .@"image-commit" = .gen(&.{}),
    .min = .genCall(&.{ .int, .int }),
    .max = .genCall(&.{ .int, .int }),
    .sin = .genCall(&.{.int}),
    .cos = .genCall(&.{.int}),
    .@"angle-from-delta" = .genCall(&.{ .int, .int }),
    .@"angle-from-line" = .genCall(&.{ .int, .int, .int, .int }),
    .@"line-length-2d" = .genCall(&.{ .int, .int, .int, .int }),
    .@"line-length-3d" = .genCall(&.{ .int, .int, .int, .int, .int, .int }),
    .@"sprite-get-object-y" = .genCall(&.{.int}),
    .@"sprite-get-state-count" = .genCall(&.{.int}),
    .@"sprite-get-group" = .genCall(&.{.int}),
    .@"find-sprite" = .genCall(&.{ .int, .int, .int, .int, .list }),
    .@"sprite-get-state" = .genCall(&.{.int}),
    .@"sprite-get-image" = .genCall(&.{.int}),
    .@"sprite-get-variable" = .genCall(&.{ .int, .int }),
    .@"sprite-set-group" = .gen(&.{.int}),
    .@"sprite-set-property" = .gen(&.{ .int, .int }),
    .@"sprite-set-order" = .gen(&.{.int}),
    .@"sprite-move" = .gen(&.{ .int, .int }),
    .@"sprite-set-state" = .gen(&.{.int}),
    .@"sprite-select-range" = .gen(&.{ .int, .int }),
    .@"sprite-set-image" = .gen(&.{.int}),
    .@"sprite-set-position" = .gen(&.{ .int, .int }),
    .@"sprite-set-animation-type" = .gen(&.{.int}),
    .@"sprite-set-palette" = .gen(&.{.int}),
    .@"sprite-set-animation-speed" = .gen(&.{.int}),
    .@"sprite-set-shadow" = .gen(&.{.int}),
    .@"sprite-set-update-type" = .gen(&.{.int}),
    .@"sprite-set-class" = .gen(&.{.list}),
    .@"sprite-restart" = .gen(&.{}),
    .@"sprite-variable-range" = .gen(&.{ .int, .int }),
    .@"sprite-new" = .gen(&.{}),
    .@"sprite-group-get" = .genCall(&.{.int}),
    .@"sprite-group-get-object-x" = .genCall(&.{.int}),
    .@"sprite-group-move" = .gen(&.{ .int, .int }),
    .@"sprite-group-select" = .gen(&.{.int}),
    .@"sprite-group-set-position" = .gen(&.{ .int, .int }),
    .@"sprite-group-set-clip" = .gen(&.{ .int, .int, .int, .int }),
    .@"sprite-group-new" = .gen(&.{}),
    .@"image-get-object-x" = .genCall(&.{ .int, .int }),
    .@"image-get-object-y" = .genCall(&.{ .int, .int }),
    .@"image-get-width" = .genCall(&.{ .int, .int }),
    .@"image-get-height" = .genCall(&.{ .int, .int }),
    .@"image-get-color-at" = .genCall(&.{ .int, .int, .int, .int }),
    .@"actor-get-property" = .genCall(&.{ .int, .int, .int }),
    .@"start-script-order" = .gen(&.{ .int, .int, .variadic }),
    .mod = .genCall(&.{ .int, .int }),
    .shl = .genCall(&.{ .int, .int }),
    .shr = .genCall(&.{ .int, .int }),
    .iif = .genCall(&.{ .int, .int, .int }),
    .@"dim-array-range.int8" = .gen(&.{ .int, .int, .int, .int, .int }),
    .@"dim-array-range.int16" = .gen(&.{ .int, .int, .int, .int, .int }),
    .set = .gen(&.{.int}),
    .@"file-size" = .genCall(&.{.string}),
    .@"set-array-item" = .gen(&.{ .int, .int }),
    .@"string-number" = .genCall(&.{.int}),
    .@"set-array-item-2d" = .gen(&.{ .int, .int, .int }),
    .@"read-ini-int" = .genCall(&.{ .int, .string, .string }),
    .@"read-ini-string" = .genCall(&.{ .int, .string, .string }),
    .@"write-ini-int" = .gen(&.{ .string, .string, .string, .int }),
    .@"write-ini-string" = .gen(&.{ .string, .string, .string, .string }),
    .inc = .gen(&.{}),
    .@"override-off-off" = .gen(&.{}),
    .@"inc-array-item" = .gen(&.{.int}),
    .dec = .gen(&.{}),
    .@"sound-position" = .genCall(&.{.int}),
    .@"dec-array-item" = .gen(&.{.int}),
    .@"jump-if" = .jump_if,
    .@"jump-unless" = .jump_unless,
    .@"start-script" = .gen(&.{ .int, .variadic }),
    .@"start-script-rec" = .gen(&.{ .int, .variadic }),
    .@"start-object" = .gen(&.{ .int, .int, .variadic }),
    .@"draw-object" = .gen(&.{ .int, .int }),
    .@"print-image" = .gen(&.{.int}),
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
    .@"cursor-off" = .gen(&.{}),
    .@"userput-on" = .gen(&.{}),
    .@"userput-off" = .gen(&.{}),
    .charset = .gen(&.{.int}),
    .@"break-here" = .gen(&.{}),
    .@"object-set-class" = .gen(&.{ .int, .list }),
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
    .@"actor-room" = .genCall(&.{.int}),
    .@"palette-color" = .genCall(&.{ .int, .int }),
    .rgb = .genCall(&.{ .int, .int, .int }),
    .override = .override,
    .@"override-off" = .gen(&.{}),
    .@"sound-running" = .genCall(&.{.int}),
    .@"load-script" = .gen(&.{.int}),
    .@"nuke-sound" = .gen(&.{.int}),
    .@"lock-script" = .gen(&.{.int}),
    .@"unlock-costume" = .gen(&.{.int}),
    .@"load-charset" = .gen(&.{.int}),
    .@"preload-sound" = .gen(&.{.int}),
    .@"nuke-image" = .gen(&.{.int}),
    .@"preload-image" = .gen(&.{.int}),
    .fades = .gen(&.{.int}),
    .@"actor-set-order" = .gen(&.{.int}),
    .@"actor-set-clipped" = .gen(&.{ .int, .int, .int, .int }),
    .@"actor-set-costume" = .gen(&.{.int}),
    .@"actor-set-talk-animation" = .gen(&.{ .int, .int }),
    .@"actor-set-elevation" = .gen(&.{.int}),
    .@"actor-set-color" = .gen(&.{ .int, .int }),
    .@"actor-set-scale" = .gen(&.{.int}),
    .@"actor-ignore-boxes" = .gen(&.{}),
    .@"actor-set-animation-speed" = .gen(&.{.int}),
    .@"actor-set-shadow" = .gen(&.{.int}),
    .@"actor-select" = .gen(&.{.int}),
    .@"actor-set-var" = .gen(&.{ .int, .int }),
    .@"actor-new" = .gen(&.{}),
    .@"palette-select" = .gen(&.{.int}),
    .@"palette-from-image" = .gen(&.{ .int, .int }),
    .@"palette-set-color" = .gen(&.{ .int, .int, .int }),
    .@"palette-new" = .gen(&.{}),
    .@"palette-commit" = .gen(&.{}),
    .@"assign-string" = .gen(&.{.string}),
    .@"find-object" = .genCall(&.{ .int, .int }),
    .@"valid-verb" = .genCall(&.{ .int, .int }),
    .@"array-assign-list" = .gen(&.{ .int, .int, .int, .int, .list }),
    .@"array-assign-slice" = .gen(&.{ .int, .int, .int, .int, .int, .int, .int, .int }),
    .@"array-assign-range" = .gen(&.{ .int, .int, .int, .int, .int, .int }),
    .sprintf = .gen(&.{ .string, .int, .variadic }),
    .@"array-assign" = .gen(&.{ .list, .int }),
    .@"array-set-row" = .gen(&.{ .int, .list }),
    .@"draw-box" = .gen(&.{ .int, .int, .int, .int, .int }),
    .debug = .gen(&.{.int}),
    .@"wait-for-message" = .gen(&.{}),
    .in = .genCall(&.{ .int, .list }),
    .@"quit-quit" = .gen(&.{}),
    .@"sleep-for" = .gen(&.{.int}),
    .@"sleep-for-seconds" = .gen(&.{.int}),
    .@"stop-sentence" = .gen(&.{}),
    .@"print-text-position" = .gen(&.{ .int, .int }),
    .@"print-text-center" = .gen(&.{}),
    .@"print-text-printf" = .gen(&.{ .int, .variadic }),
    .@"print-text-start" = .gen(&.{}),
    .@"print-debug-string" = .gen(&.{}),
    .@"print-debug-printf" = .gen(&.{ .int, .variadic }),
    .@"print-debug-start" = .gen(&.{}),
    .@"print-system-string" = .gen(&.{}),
    .@"print-system-printf" = .gen(&.{ .int, .variadic }),
    .@"print-system-start" = .gen(&.{}),
    .@"say-line-position" = .gen(&.{ .int, .int }),
    .@"say-line-string" = .gen(&.{}),
    .@"say-line-talkie" = .gen(&.{.int}),
    .@"say-line-color" = .gen(&.{.list}),
    .@"say-line-start" = .gen(&.{.int}),
    .@"say-line-actor" = .gen(&.{.int}),
    .@"dim-array.int8" = .gen(&.{.int}),
    .@"dim-array.int16" = .gen(&.{.int}),
    .@"dim-array.int32" = .gen(&.{.int}),
    .undim = .gen(&.{}),
    .@"return" = .gen(&.{.int}),
    .@"call-script" = .genCall(&.{ .int, .variadic }),
    .@"dim-array-2d.int8" = .gen(&.{ .int, .int }),
    .@"dim-array-2d.int16" = .gen(&.{ .int, .int }),
    .@"dim-array-2d.int32" = .gen(&.{ .int, .int }),
    .abs = .genCall(&.{.int}),
    .@"kludge-call" = .genCall(&.{.variadic}),
    .kludge = .gen(&.{.variadic}),
    .@"break-here-multi" = .gen(&.{.int}),
    .pick = .genCall(&.{ .int, .list }),
    .@"debug-input" = .genCall(&.{.string}),
    .@"get-time-date" = .gen(&.{}),
    .@"stop-line" = .gen(&.{}),
    .@"actor-get-var" = .genCall(&.{ .int, .int }),
    .shuffle = .gen(&.{ .int, .int }),
    .@"chain-script" = .gen(&.{ .int, .variadic }),
    .band = .genCall(&.{ .int, .int }),
    .bor = .genCall(&.{ .int, .int }),
    .@"close-file" = .gen(&.{.int}),
    .@"open-file" = .genCall(&.{ .string, .int }),
    .@"read-file-int8" = .genCall(&.{ .int, .int }),
    .@"write-file-int8" = .gen(&.{ .int, .int }),
    .@"delete-file" = .gen(&.{.string}),
    .@"array-line-draw" = .gen(&.{ .int, .int, .int, .int, .int, .int }),
    .localize = .gen(&.{.int}),
    .@"pick-random" = .genCall(&.{.list}),
    .@"redim-array.int8" = .gen(&.{ .int, .int }),
    .@"redim-array.int16" = .gen(&.{ .int, .int }),
    .@"string-length" = .genCall(&.{.int}),
    .@"string-substr" = .genCall(&.{ .int, .int, .int }),
    .@"string-compare" = .genCall(&.{ .int, .int }),
    .@"read-system-ini-int" = .genCall(&.{.string}),
    .@"read-system-ini-string" = .genCall(&.{.string}),
    .@"write-system-ini-int" = .gen(&.{ .string, .int }),
    .@"write-system-ini-string" = .gen(&.{ .string, .string }),
    .@"sound-size" = .genCall(&.{.int}),
    .@"title-bar" = .gen(&.{.string}),
    .@"delete-polygon" = .gen(&.{ .int, .int }),
});

fn initEnumArrayFixed(E: type, V: type, values: std.enums.EnumFieldStruct(E, V, null)) std.EnumArray(E, V) {
    @setEvalBranchQuota(3000);
    return .init(values);
}

fn decompileBasicBlocks(cx: *DecompileCx, bytecode: []const u8) !void {
    try scheduleBasicBlock(cx, 0, &.{});
    while (cx.pending_basic_blocks.pop()) |bbi|
        try decompileBasicBlock(cx, bytecode, bbi);

    // Check if any basic blocks were unreachable and not decompiled. If so,
    // handle the case where it's an infinite loop, otherwise fail.
    for (cx.basic_blocks, 0..) |*bb, bbi_usize| {
        const bbi: BasicBlockIndex = @intCast(bbi_usize);
        if (bb.state != .new) continue;
        try scheduleAfterInfiniteLoop(cx, bbi);
        return @call(.always_tail, decompileBasicBlocks, .{ cx, bytecode });
    }

    // The last basic block should end with `end` or `end2`. This is
    // emitted implicitly by the compiler, so don't output it explicitly here.
    const bb_last = &cx.basic_blocks[cx.basic_blocks.len - 1];
    const ss = bb_last.statements.defined;
    if (ss.len == 0) return error.BadData;
    const stmt = &cx.stmts.items[ss.start + ss.len - 1];
    if (stmt.* != .call) return error.BadData;
    if (stmt.call.op != .end and stmt.call.op != .end2) return error.BadData;
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
    bb.stack_on_enter.setOnce(try .fromSlice(cx.stack.slice()));
    try cx.pending_basic_blocks.append(cx.gpa, bbi);
}

fn scheduleAfterInfiniteLoop(cx: *DecompileCx, bbi: u16) !void {
    // Check if the previous block forms a loop (presumably infinite loop). If
    // so, schedule the current block using the stack from the loop entry point.
    if (bbi == 0) return error.BadData;
    const bbi_prev = bbi - 1;
    const prev = &cx.basic_blocks[bbi_prev];
    if (prev.exit != .jump) return error.BadData;
    if (prev.exit.jump >= prev.end) return error.BadData;
    const bbi_start = findBasicBlockWithStart(cx, prev.exit.jump);
    const start = &cx.basic_blocks[bbi_start];
    if (start.state == .new) return error.BadData;
    try scheduleBasicBlock(cx, bbi, start.stack_on_enter.defined.slice());
}

fn decompileBasicBlock(cx: *DecompileCx, bytecode: []const u8, bbi: u16) !void {
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
                try scheduleBasicBlock(cx, bbi + 1, cx.stack.slice());
            } else {
                // the last block should finish with an empty stack
                if (cx.stack.len != 0) return error.BadData;
            }
        },
        .jump => |target| {
            const target_bbi = findBasicBlockWithStart(cx, target);
            try scheduleBasicBlock(cx, target_bbi, cx.stack.slice());
        },
        .jump_if, .jump_unless, .override => |target| {
            if (bbi == cx.basic_blocks.len - 1) // should never happen, given well-formed input
                return error.BadData;
            try scheduleBasicBlock(cx, bbi + 1, cx.stack.slice());
            const target_bbi = findBasicBlockWithStart(cx, target);
            try scheduleBasicBlock(cx, target_bbi, cx.stack.slice());
        },
    }

    // Above, the stack was dealt with, so we no longer need it. Clear it for the next block.
    cx.stack.clear();
    // However the string stack wasn't, so make sure it ended up empty.
    if (cx.str_stack.len != 0) return error.BadData;
}

fn decompileIns(cx: *DecompileCx, ins: lang.Ins) !void {
    const op = switch (ins.name) {
        .op => |op| op,
        .str => |s| {
            cx.diag.err(ins.start, "unhandled opcode {s}", .{s});
            return error.AddedToDiagnostic;
        },
    };
    for (ins.operands.slice()) |o|
        if (o == .variable)
            try cx.usage.track(o.variable);
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
        .dup => {
            if (cx.stack.len == 0) return error.BadData;
            const top = cx.stack.get(cx.stack.len - 1);
            try push(cx, .{ .dup = top });
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
        .override => {
            const rel = ins.operands.get(0).relative_offset;
            const target = utils.addUnsignedSigned(ins.end, rel).?;
            try cx.stmts.append(cx.gpa, .{ .override = .{ .target = target } });
        },
        .generic => |gen| {
            var args: std.BoundedArray(ExprIndex, lang.max_operands + script.max_params) = .{};
            for (ins.operands.slice()) |operand| {
                const expr: Expr = switch (operand) {
                    // TODO: try to get rid of u8 here. all occurrences are
                    // probably better represented as subopcodes
                    .u8 => |i| .{ .int = i },
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
                    .variadic => try popVariadicList(cx),
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
    basic_blocks: []const BasicBlock,
    stmts: utils.SafeManyPointer([*]const Stmt),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    types: ArrayMap(Type),
};

const Type = union(enum) {
    script,
};

fn recoverTypes(cx: *TypeCx) void {
    for (cx.basic_blocks) |*bb| {
        const ss = bb.statements.defined;
        const stmts = cx.stmts.use()[ss.start..][0..ss.len];
        for (stmts) |*stmt| switch (stmt.*) {
            .jump_if, .jump_unless => |j| recoverExpr(cx, j.condition),
            .jump, .override => {},
            .call => |c| recoverCall(cx, c.op, c.args),
            .compound, .tombstone => unreachable,
        };
    }
}

fn recoverExpr(cx: *TypeCx, ei: ExprIndex) void {
    switch (cx.exprs.getPtr(ei).*) {
        .int, .string, .variable, .dup => {},
        .call => |call| recoverCall(cx, call.op, call.args),
        .list, .variadic_list => |items| {
            for (getExtra3(cx.extra, items)) |i|
                recoverExpr(cx, i);
        },
    }
}

fn recoverCall(cx: *TypeCx, op: lang.Op, arg_eis: ExtraSlice) void {
    const args = getExtra3(cx.extra, arg_eis);
    for (args) |ei|
        recoverExpr(cx, ei);
    switch (op) {
        .@"start-script" => {
            setType(cx, args[0], .script);
        },
        .@"start-script-rec" => {
            setType(cx, args[0], .script);
        },
        .@"script-running" => {
            setType(cx, args[0], .script);
        },
        .@"call-script" => {
            setType(cx, args[0], .script);
        },
        .@"load-script" => {
            setType(cx, args[0], .script);
        },
        else => {},
    }
}

fn setType(cx: *TypeCx, ei: ExprIndex, typ: Type) void {
    cx.types.put(utils.null_allocator, ei, typ) catch unreachable;
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
            peepholeSpriteSelect(cx, stmt);
            peepLockAndLoadScript(cx, stmts, i);
            peepholePaletteSetColor(cx, stmt);
        }
    }
}

/// Replace `sprite-select-range x dup{x}` with `sprite-select x`
fn peepholeSpriteSelect(cx: *DecompileCx, stmt: *Stmt) void {
    if (stmt.* != .call) return;
    if (stmt.call.op != .@"sprite-select-range") return;
    std.debug.assert(stmt.call.args.len == 2);
    const args = cx.extra.items[stmt.call.args.start..][0..stmt.call.args.len];
    const second = &cx.exprs.items[args[1]];
    if (second.* != .dup) return;
    if (second.dup != args[0]) return;
    stmt.* = .{ .compound = .{
        .op = .@"sprite-select",
        .args = .{ .start = stmt.call.args.start, .len = 1 },
    } };
}

fn peepLockAndLoadScript(cx: *DecompileCx, stmts: []Stmt, stmt_index: usize) void {
    if (stmt_index + 1 >= stmts.len) return;
    const lock = &stmts[stmt_index];
    const load = &stmts[stmt_index + 1];

    if (lock.* != .call) return;
    if (lock.call.op != .@"lock-script") return;
    std.debug.assert(lock.call.args.len == 1);
    const lock_args = getExtra2(&cx.extra, lock.call.args);
    const lock_arg = &cx.exprs.items[lock_args[0]];
    if (lock_arg.* != .dup) return;
    const resource = lock_arg.dup;

    if (load.* != .call) return;
    if (load.call.op != .@"load-script") return;
    std.debug.assert(load.call.args.len == 1);
    const load_args = getExtra2(&cx.extra, load.call.args);
    if (load_args[0] != resource) return;

    stmts[stmt_index] = .{ .compound = .{
        .op = .@"lock-and-load-script",
        .args = load.call.args,
    } };
    stmts[stmt_index + 1] = .tombstone;
}

/// Replace `palette-set-color a dup{a} b` with `palette-set-slot-color a b`
fn peepholePaletteSetColor(cx: *DecompileCx, stmt: *Stmt) void {
    if (stmt.* != .call) return;
    if (stmt.call.op != .@"palette-set-color") return;
    std.debug.assert(stmt.call.args.len == 3);
    const args = cx.extra.items[stmt.call.args.start..][0..stmt.call.args.len];
    const second = &cx.exprs.items[args[1]];
    if (second.* != .dup) return;
    if (second.dup != args[0]) return;
    args[1] = args[2];
    stmt.* = .{ .compound = .{
        .op = .@"palette-set-slot-color",
        .args = .{ .start = stmt.call.args.start, .len = 2 },
    } };
}

const StructuringCx = struct {
    gpa: std.mem.Allocator,
    stmts: utils.SafeManyPointer([*]Stmt),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: *std.ArrayListUnmanaged(ExprIndex),

    queue: std.ArrayListUnmanaged(NodeIndex),
    nodes: std.ArrayListUnmanaged(Node),
};

const NodeIndex = u16;
const null_node: NodeIndex = 0xffff;

fn niOpt(ni: NodeIndex) ?NodeIndex {
    return if (ni == null_node) null else ni;
}

const pc_unknown = 0xffff;

fn pcOpt(pc: u16) ?u16 {
    return if (pc == pc_unknown) null else pc;
}

const Node = struct {
    /// start pc
    start: u16,
    /// end pc, or `pc_unknown` if we didn't bother to keep track exactly
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

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntIf(cx, ni);

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntIfElse(cx, ni);

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntWhile(cx, ni);

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntFor(cx, ni);

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntBreakUntil(cx, ni);

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntDo(cx, ni);

    try queueNode(cx, root_node_index);
    while (cx.queue.pop()) |ni|
        try huntCase(cx, ni);
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
        .case => |*n| {
            try queueNode(cx, n.first_branch);
        },
        .case_branch => |*n| {
            try queueNode(cx, n.body);
        },
    }
}

fn huntIf(cx: *StructuringCx, ni_first: NodeIndex) !void {
    var ni = ni_first;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
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
            .false = null_node,
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

    try queueNode(cx, ni_true_start);
}

fn huntIfElse(cx: *StructuringCx, ni_first: NodeIndex) !void {
    var ni = ni_first;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.kind != .@"if") continue;
        if (node.kind.@"if".true == null_node) continue;

        const ni_true_end = findLastNode(cx, node.kind.@"if".true);
        const true_end = &cx.nodes.items[ni_true_end];
        if (true_end.kind != .basic_block) continue;
        if (true_end.kind.basic_block.exit != .jump) continue;
        if (true_end.kind.basic_block.exit.jump <= node.end) continue;
        const ni_false_end = findNodeWithEnd(cx, node.next, true_end.kind.basic_block.exit.jump);
        if (ni_false_end == null_node) continue;

        try makeIfElse(cx, ni, ni_false_end);
    }
}

fn makeIfElse(cx: *StructuringCx, ni: NodeIndex, ni_false_end: NodeIndex) !void {
    const node = &cx.nodes.items[ni];
    const ni_true_end = findLastNode(cx, node.kind.@"if".true);
    const ni_false_start = node.next;
    const ni_after = cx.nodes.items[ni_false_end].next;

    chopJump(cx, ni_true_end);
    node.end = cx.nodes.items[ni_false_end].end;
    node.next = ni_after;
    node.kind.@"if".false = ni_false_start;
    if (ni_after != null_node)
        cx.nodes.items[ni_after].prev = ni;

    cx.nodes.items[ni_false_start].prev = null_node;
    cx.nodes.items[ni_false_end].next = null_node;

    structureCheckpoint(cx, "makeIfElse ni={} ni_false_end={}", .{ ni, ni_false_end });

    try queueNode(cx, node.kind.@"if".true);
    try queueNode(cx, node.kind.@"if".false);
}

fn huntWhile(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.kind != .@"if") continue;
        if (node.kind.@"if".false != null_node) continue;

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
    chopJump(cx, ni_true_last);
    cx.nodes.items[ni_if].kind = .{ .@"while" = .{
        .condition = cx.nodes.items[ni_if].kind.@"if".condition,
        .body = cx.nodes.items[ni_if].kind.@"if".true,
    } };

    structureCheckpoint(cx, "makeWhile ni_if={} ni_true_last={}", .{ ni_if, ni_true_last });
}

fn huntFor(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
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
    const init = cx.stmts.getPtr(init_ss.start + init_ss.len - 1);
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
    const inc = cx.stmts.getPtr(inc_ss.start + inc_ss.len - 1);
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
    const ni_body = cx.nodes.items[ni].kind.@"while".body;

    chopLastStmt(cx, ni_init); // chop off the init
    chopLastStmt(cx, info.body_last); // chop off the increment

    cx.nodes.items[ni].start = cx.nodes.items[ni_init].end;
    cx.nodes.items[ni].prev = ni_init;
    cx.nodes.items[ni].kind = .{ .@"for" = .{
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

fn huntDo(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        try queueChildren(cx, ni);

        const node = &cx.nodes.items[ni];
        if (node.end == pc_unknown) continue;
        if (node.kind != .basic_block) continue;
        const target = switch (node.kind.basic_block.exit) {
            .jump, .jump_unless => |target| target,
            else => continue,
        };
        if (target >= node.end) continue;

        const ni_body_first = findBackwardsNodeWithStart(cx, ni, target) orelse continue;

        try makeDo(cx, ni_body_first, ni);
    }
}

fn makeDo(cx: *StructuringCx, ni_body_first_orig: NodeIndex, ni_condition_orig: NodeIndex) !void {
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
}

fn huntBreakUntil(cx: *StructuringCx, ni_initial: NodeIndex) !void {
    var ni = ni_initial;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
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

    const stmt = cx.stmts.getPtr(node.kind.basic_block.statements.start);
    const condition = stmt.jump_if.condition;
    stmt.* = .{ .compound = .{
        .op = .@"break-until",
        .args = try storeExtra2(cx.gpa, cx.extra, &.{condition}),
    } };

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
        try queueChildren(cx, ni);

        if (isCase(cx, ni)) |ni_last|
            try makeCase(cx, ni, ni_last);
    }
}

fn isCase(cx: *StructuringCx, ni_start: NodeIndex) ?NodeIndex {
    var ni = ni_start;
    const value = checkCaseBranch(cx, ni) orelse return null;
    ni = cx.nodes.items[ni].kind.@"if".false;
    while (true) {
        if (checkCaseBranchAnother(cx, ni, value)) |i| {
            ni = i;
            continue;
        }
        // If it's not a continue, it must be the else branch. Check if it's
        // well-formed and if so return success.
        if (!nodeStartsWithPop(cx, ni, value)) return null;
        return ni;
    }
}

fn checkCaseBranch(cx: *StructuringCx, ni: NodeIndex) ?ExprIndex {
    const node = &cx.nodes.items[ni];
    if (node.kind != .@"if") return null;
    if (node.kind.@"if".false == null_node) return null;

    const cond = cx.exprs.getPtr(node.kind.@"if".condition);
    if (cond.* != .call) return null;
    if (cond.call.op != .eq and cond.call.op != .@"in-list") return null;
    // Both possible ops have args in the same place so just combine their logic
    std.debug.assert(cond.call.args.len == 2);
    const cond_args = getExtra2(cx.extra, cond.call.args);
    const cond_lhs = cx.exprs.getPtr(cond_args[0]);
    if (cond_lhs.* != .dup) return null;

    if (node.kind.@"if".true == null_node) return null;
    if (!nodeStartsWithPop(cx, node.kind.@"if".true, cond_lhs.dup)) return null;

    return cond_lhs.dup;
}

fn checkCaseBranchAnother(
    cx: *StructuringCx,
    ni_leading: NodeIndex,
    expected_value: ExprIndex,
) ?NodeIndex {
    // Due to a quirk of how if statements are structured, the false branch will
    // start with a zero-length basic block before the if statement we're
    // actually interested in. Check for it and skip over it.
    const leading = &cx.nodes.items[ni_leading];
    if (leading.start != leading.end) return null;
    if (leading.kind != .basic_block) return null;
    if (leading.next == null_node) return null;

    const ni = leading.next;
    const node = &cx.nodes.items[ni];
    if (node.next != null_node) return null;
    const cond_lhs = checkCaseBranch(cx, ni) orelse return null;
    if (cond_lhs != expected_value) return null;
    return node.kind.@"if".false;
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
    // convert node tree from this:

    // if a
    //     w
    // else
    //     if b
    //         x
    //     else
    //         if c
    //             y
    //         else
    //             z

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
    // if a   case
    // (new)  branch a
    // w      w
    // if b   branch b
    // x      x
    // if c   branch c
    // y      y
    // (new)  branch else
    // z      z

    const node = &cx.nodes.items[ni];
    const ni_first_true = node.kind.@"if".true;
    const ni_first_false = node.kind.@"if".false;
    const cond = cx.exprs.getPtr(node.kind.@"if".condition);
    std.debug.assert(cond.call.args.len == 2);
    const cond_args = getExtra2(cx.extra, cond.call.args);
    const case_value = cx.exprs.getPtr(cond_args[0]).dup;

    node.kind = .{
        .case = .{
            .value = case_value,
            .first_branch = undefined, // set below
        },
    };
    const ni_first_branch = try appendNode(cx, .{
        .start = node.start,
        .end = pc_unknown,
        .prev = null_node,
        .next = undefined, // set below
        .kind = .{ .case_branch = .{
            .value = cond_args[1],
            .body = ni_first_true,
        } },
    });
    cx.nodes.items[ni].kind.case.first_branch = ni_first_branch;
    chopFirstPop(cx, ni_first_true);

    var ni_prev_branch = ni_first_branch;
    var ni_cur = ni_first_false;
    while (ni_cur != ni_last) {
        // As above, skip over the quirky empty node.
        const ni_real_cur = cx.nodes.items[ni_cur].next;
        orphanNode(cx, ni_cur);
        ni_cur = ni_real_cur;
        const cur = &cx.nodes.items[ni_cur];
        const eq = cx.exprs.getPtr(cur.kind.@"if".condition);
        std.debug.assert(eq.call.args.len == 2);
        const eq_args = getExtra2(cx.extra, eq.call.args);
        const branch_expr = eq_args[1];
        const ni_body = cur.kind.@"if".true;
        const ni_next = cur.kind.@"if".false;
        cx.nodes.items[ni_cur] = .{
            .start = cur.start,
            .end = pc_unknown,
            .prev = ni_prev_branch,
            .next = undefined, // set either in the next loop iteration or at the end
            .kind = .{ .case_branch = .{
                .value = branch_expr,
                .body = ni_body,
            } },
        };
        cx.nodes.items[ni_prev_branch].next = ni_cur;
        chopFirstPop(cx, cur.kind.case_branch.body);
        ni_prev_branch = ni_cur;
        ni_cur = ni_next;
    }

    const ni_else_branch = try appendNode(cx, .{
        .start = cx.nodes.items[ni_cur].start,
        .end = pc_unknown,
        .prev = ni_prev_branch,
        .next = null_node,
        .kind = .{ .case_branch = .{
            .value = null_expr,
            .body = ni_cur,
        } },
    });
    cx.nodes.items[ni_prev_branch].next = ni_else_branch;
    chopFirstPop(cx, ni_cur);

    structureCheckpoint(cx, "makeCase ni={} ni_last={}", .{ ni, ni_last });
}

fn findLastNode(cx: *StructuringCx, ni_initial: NodeIndex) NodeIndex {
    var ni = ni_initial;
    while (true) {
        const node = &cx.nodes.items[ni];
        if (node.next == null_node) return ni;
        ni = node.next;
    }
    return null;
}

fn findNodeWithEnd(cx: *StructuringCx, ni_first: NodeIndex, end: u16) NodeIndex {
    var ni = ni_first;
    while (ni != null_node) : (ni = cx.nodes.items[ni].next) {
        const node = &cx.nodes.items[ni];
        if (node.end == end) return ni;
        if (node.end != pc_unknown and node.end > end) break;
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

    return ni_new;
}

fn orphanNode(cx: *StructuringCx, ni: NodeIndex) void {
    // In debug builds, when orphaning a node, update the links so it doesn't
    // trigger invariant violations.
    if (builtin.mode != .Debug) return;
    const node = &cx.nodes.items[ni];
    node.prev = null_node;
    node.next = null_node;
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
            if (pcOpt(prev.end)) |prev_end|
                try std.testing.expect(prev_end == node.start);
        }
        if (node.next != null_node) {
            const next = &cx.nodes.items[node.next];
            try std.testing.expect(next.prev == ni);
            if (pcOpt(node.end)) |node_end|
                try std.testing.expect(next.start == node_end);
        }
        if (node.kind == .basic_block) {
            if (node.kind.basic_block.exit != .no_jump)
                try std.testing.expect(node.end != pc_unknown);
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

    const items = try cx.gpa.alloc(Item, cx.nodes.items.len);
    defer cx.gpa.free(items);
    for (items, cx.nodes.items, 0..) |*item, *node, ni|
        item.* = .{ .index = ni, .node = node.* };
    std.mem.sort(Item, items, {}, Item.lt);

    for (items) |item| {
        const ni = item.index;
        const node = item.node;
        try out.print(
            "{}: {s:<11} {}/{} 0x{x:0>4}-0x{x:0>4}\n",
            .{ ni, @tagName(node.kind), node.prev, node.next, node.start, node.end },
        );
        switch (node.kind) {
            .basic_block => {},
            .@"if" => |*n| try out.print("      true={} false={}\n", .{ n.true, n.false }),
            .@"while" => |*n| try out.print("      body={}\n", .{n.body}),
            .@"for" => |*n| try out.print("      body={}\n", .{n.body}),
            .do => |*n| try out.print("      body={}\n", .{n.body}),
            .case => |*n| try out.print("      first={}\n", .{n.first_branch}),
            .case_branch => |*n| try out.print("      body={}\n", .{n.body}),
        }
    }
}

const jump_len = 3;

fn chopJump(cx: *StructuringCx, ni: NodeIndex) void {
    const node = &cx.nodes.items[ni];

    if (builtin.mode == .Debug) {
        const ss = node.kind.basic_block.statements;
        const stmt = cx.stmts.getPtr(ss.start + ss.len - 1);
        std.debug.assert(stmt.* == .jump);
    }

    std.debug.assert(node.kind.basic_block.exit == .jump);
    node.kind.basic_block.exit = .no_jump;
    node.kind.basic_block.statements.len -= 1;
    std.debug.assert(node.end != pc_unknown);
    node.end -= jump_len;
}

fn chopJumpCondition(cx: *StructuringCx, ni: NodeIndex) ExprIndex {
    const node = &cx.nodes.items[ni];

    const ss = node.kind.basic_block.statements;
    const stmt = cx.stmts.getPtr(ss.start + ss.len - 1);
    const condition = switch (stmt.*) {
        .jump_if, .jump_unless => |j| j.condition,
        else => unreachable,
    };

    std.debug.assert(node.kind.basic_block.exit == .jump_if or
        node.kind.basic_block.exit == .jump_unless);
    node.kind.basic_block.exit = .no_jump;
    node.kind.basic_block.statements.len -= 1;
    node.end = if (node.kind.basic_block.statements.len == 0)
        node.start
    else
        pc_unknown;
    return condition;
}

fn chopLastStmt(cx: *StructuringCx, ni: NodeIndex) void {
    const node = &cx.nodes.items[ni];

    std.debug.assert(node.kind.basic_block.exit == .no_jump);
    node.kind.basic_block.statements.len -= 1;
    node.end = if (node.kind.basic_block.statements.len == 0)
        node.start
    else
        pc_unknown;
}

fn chopFirstPop(cx: *StructuringCx, ni: NodeIndex) void {
    const node = &cx.nodes.items[ni];

    if (builtin.mode == .Debug) {
        const ss = node.kind.basic_block.statements;
        const stmt = cx.stmts.getPtr(ss.start);
        std.debug.assert(stmt.call.op == .pop);
    }

    node.start += 1;
    node.kind.basic_block.statements.start += 1;
    node.kind.basic_block.statements.len -= 1;
}

fn getExtra2(
    extra: *std.ArrayListUnmanaged(ExprIndex),
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
                .jump, .jump_if, .jump_unless, .override => |target| target,
            };
            try insertSortedNoDup(cx.gpa, &cx.result, target);
        },
        .@"if" => |*n| {
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
        .case => |*n| {
            try findJumpTargetsInNodeList(cx, n.first_branch);
        },
        .case_branch => |*n| {
            try findJumpTargetsInNodeList(cx, n.body);
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
    room_number: u8,
    id: Symbols.ScriptId,
    nodes: utils.SafeManyPointer([*]const Node),
    stmts: utils.SafeManyPointer([*]const Stmt),
    exprs: utils.SafeManyPointer([*]const Expr),
    extra: utils.SafeManyPointer([*]const ExprIndex),
    local_var_usage: *const UsageTracker.LocalVars,
    types: *const ArrayMap(Type),
    jump_targets: []const u16,

    out: *std.ArrayListUnmanaged(u8),
    indent: u16,
};

fn emitScript(cx: *EmitCx) !void {
    try emitLocalVarsDecl(cx);
    try emitNodeList(cx, root_node_index);
}

fn emitLocalVarsDecl(cx: *EmitCx) !void {
    var i: usize = UsageTracker.max_local_vars;
    const max_used = while (i > 0) {
        i -= 1;
        const used = UsageTracker.get(cx.local_var_usage, i);
        if (used) break i;
    } else return;

    const script_symbols = cx.symbols.getScript(cx.id);

    try writeIndent(cx);
    try cx.out.appendSlice(cx.gpa, "var");
    for (0..max_used + 1) |num| {
        const name = name: {
            const used = UsageTracker.get(cx.local_var_usage, num);
            if (!used) break :name "_";
            if (script_symbols) |ss| break :name ss.locals.get(num);
            break :name null;
        };
        try cx.out.append(cx.gpa, ' ');
        if (name) |n|
            try cx.out.appendSlice(cx.gpa, n)
        else
            try cx.out.writer(cx.gpa).print("local{}", .{num});
    }
    try cx.out.appendSlice(cx.gpa, "\n\n");
}

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
            try cx.out.appendSlice(cx.gpa, "if ");
            try emitExpr(cx, k.condition, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
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
        .@"while" => |n| {
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "while ");
            try emitExpr(cx, n.condition, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .@"for" => |*n| {
            try writeIndent(cx);
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
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .do => |n| {
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "do {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.body);
            cx.indent -= indent_size;
            try writeIndent(cx);
            if (n.condition == null_expr) {
                try cx.out.appendSlice(cx.gpa, "}\n");
            } else {
                try cx.out.appendSlice(cx.gpa, "} until ");
                try emitExpr(cx, n.condition, .space);
                try cx.out.appendSlice(cx.gpa, "\n");
            }
        },
        .case => |*n| {
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "case ");
            try emitExpr(cx, n.value, .space);
            try cx.out.appendSlice(cx.gpa, " {\n");
            cx.indent += indent_size;
            try emitNodeList(cx, n.first_branch);
            cx.indent -= indent_size;
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
        .case_branch => |*n| {
            try writeIndent(cx);
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
            try writeIndent(cx);
            try cx.out.appendSlice(cx.gpa, "}\n");
        },
    }
}

fn emitStmt(cx: *const EmitCx, stmt: *const Stmt) !void {
    if (stmt.* == .tombstone) return;
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
        .string => |s| try cx.out.writer(cx.gpa).print("\"{s}\"", .{s}),
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
            } else if (@as(?Ast.BinOp, switch (call.op) {
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
            })) |op| {
                const op_prec = op.precedence();
                if (@intFromEnum(prec) >= @intFromEnum(op_prec))
                    try cx.out.append(cx.gpa, '(');
                try emitExpr(cx, args[0], op_prec);
                try cx.out.writer(cx.gpa).print(" {s} ", .{op.str()});
                try emitExpr(cx, args[1], op_prec);
                if (@intFromEnum(prec) >= @intFromEnum(op_prec))
                    try cx.out.append(cx.gpa, ')');
            } else {
                if (@intFromEnum(prec) >= @intFromEnum(Precedence.space))
                    try cx.out.append(cx.gpa, '(');
                try emitCall(cx, @tagName(call.op), call.args);
                if (@intFromEnum(prec) >= @intFromEnum(Precedence.space))
                    try cx.out.append(cx.gpa, ')');
            }
        },
        .list => |items| try emitList(cx, items),
        .variadic_list => unreachable, // only appears in call args, handled elsewhere
        .dup => return error.BadData,
    }
}

fn emitInt(cx: *const EmitCx, ei: ExprIndex) !void {
    const int = cx.exprs.getPtr(ei).int;
    if (cx.types.get(ei)) |t| switch (t) {
        .script => if (std.math.cast(u32, int)) |n| {
            try cx.symbols.writeScriptName(cx.room_number, n, cx.out.writer(cx.gpa));
            return;
        },
    };
    try cx.out.writer(cx.gpa).print("{}", .{int});
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
    const kind, const number = try variable.decode2();
    switch (kind) {
        .global => {
            if (cx.symbols.globals.get(number)) |name|
                return try cx.out.appendSlice(cx.gpa, name);
        },
        .local => {
            if (cx.symbols.getScript(cx.id)) |ss|
                if (ss.locals.get(number)) |name|
                    return try cx.out.appendSlice(cx.gpa, name);
        },
        .room => {
            if (cx.symbols.getRoom(cx.room_number)) |room|
                if (room.vars.get(number)) |name|
                    return try cx.out.appendSlice(cx.gpa, name);
        },
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
