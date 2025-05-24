const std = @import("std");

const Game = @import("games.zig").Game;

pub const unknown_byte_ins = ".db";

const LangOperandArray = std.BoundedArray(LangOperand, max_operands);
const OperandArray = std.BoundedArray(Operand, max_operands);

pub const max_operands = 2;

pub const Language = struct {
    /// 0 to 255 are normal opcodes. The rest are dynamically-assigned
    /// 256-element chunks for two-byte opcodes.
    opcodes: [256 * 50]Opcode = @splat(.unknown),
    num_nested: u8 = 0,

    fn add(self: *Language, byte: u8, name: anytype, operands: []const LangOperand) void {
        self.addInner(byte, .from(name), operands);
    }

    fn addInner(
        self: *Language,
        byte: u8,
        name: OpcodeName,
        operands: []const LangOperand,
    ) void {
        if (self.opcodes[byte] != .unknown)
            unreachable;

        self.opcodes[byte] = .{ .ins = .{
            .name = name,
            .operands = LangOperandArray.fromSlice(operands) catch unreachable,
        } };
    }

    fn addNested(
        self: *Language,
        byte1: u8,
        byte2: u8,
        name: anytype,
        operands: []const LangOperand,
    ) void {
        self.addNestedInner(byte1, byte2, .from(name), operands);
    }

    fn addNestedInner(
        self: *Language,
        byte1: u8,
        byte2: u8,
        name: OpcodeName,
        operands: []const LangOperand,
    ) void {
        const n = switch (self.opcodes[byte1]) {
            .unknown => n: {
                self.num_nested += 1;
                self.opcodes[byte1] = .{ .nested = self.num_nested };
                break :n self.num_nested;
            },
            .nested => |n| n,
            else => unreachable,
        };

        self.opcodes[n << 8 | byte2] = .{ .ins = .{
            .name = name,
            .operands = LangOperandArray.fromSlice(operands) catch unreachable,
        } };
    }
};

pub const Op = enum {
    @"push-u8",
    @"push-i16",
    @"push-i32",
    @"push-var",
    @"push-str",
    @"get-array-item",
    @"dup-multi",
    @"get-array-item-2d",
    dup,
    not,
    eq,
    ne,
    gt,
    lt,
    le,
    ge,
    add,
    sub,
    mul,
    div,
    land,
    lor,
    pop,
    @"in-list",
    @"image-set-width",
    @"image-set-height",
    @"image-draw",
    @"image-load-external",
    @"image-capture",
    @"image-set-state",
    @"image-set-flags",
    @"draw-image-at",
    @"image-select",
    @"image-set-pos",
    @"image-set-palette",
    @"image-set-shadow",
    @"image-set-draw-box",
    @"image-set-render-image",
    @"image-set-hotspot",
    @"image-new",
    @"image-commit",
    min,
    max,
    sin,
    cos,
    @"angle-from-delta",
    @"angle-from-line",
    @"line-length-2d",
    @"line-length-3d",
    @"sprite-get-object-y",
    @"sprite-get-state-count",
    @"sprite-get-group",
    @"sprite-get-object-draw-x",
    @"sprite-get-object-draw-y",
    @"sprite-get-order",
    @"find-sprite",
    @"sprite-get-state",
    @"sprite-get-image",
    @"sprite-get-update-type",
    @"sprite-class",
    @"sprite-get-variable",
    @"sprite-set-group",
    @"sprite-set-property",
    @"sprite-set-order",
    @"sprite-move",
    @"sprite-set-state",
    @"sprite-select-range",
    @"sprite-set-image",
    @"sprite-set-position",
    @"sprite-set-step-dist",
    @"sprite-set-animation-type",
    @"sprite-set-palette",
    @"sprite-set-animation-speed",
    @"sprite-set-shadow",
    @"sprite-set-update-type",
    @"sprite-set-class",
    @"sprite-mask-image",
    @"sprite-restart",
    @"sprite-variable-range",
    @"sprite-new",
    @"sprite-group-get",
    @"sprite-group-get-object-x",
    @"sprite-group-get-object-y",
    @"sprite-group-move",
    @"sprite-group-select",
    @"sprite-group-set-position",
    @"sprite-group-set-clip",
    @"sprite-group-new",
    @"image-get-object-x",
    @"image-get-object-y",
    @"image-get-width",
    @"image-get-height",
    @"image-get-state-count",
    @"image-get-color-at",
    @"actor-get-property",
    @"start-script-order",
    mod,
    shl,
    shr,
    @"find-all-objects",
    iif,
    @"dim-array-range.int8",
    @"dim-array-range.int16",
    @"array-sort",
    set,
    @"file-size",
    @"set-array-item",
    @"string-number",
    @"set-array-item-2d",
    @"read-ini-int",
    @"read-ini-string",
    @"write-ini-int",
    @"write-ini-string",
    inc,
    @"override-off-off",
    @"inc-array-item",
    dec,
    @"get-timer",
    @"sound-position",
    @"dec-array-item",
    @"jump-if",
    @"jump-unless",
    @"start-script",
    @"start-script-rec",
    @"start-object",
    @"start-object-rec",
    @"draw-object",
    @"print-image",
    @"array-get-dim",
    @"array-get-height",
    @"array-get-width",
    @"free-arrays",
    end2,
    end,
    @"window-select",
    @"window-set-script",
    @"window-set-image",
    @"window-new",
    @"window-set-title-bar",
    @"window-commit",
    @"freeze-scripts",
    @"cursor-bw",
    @"cursor-color",
    @"cursor-on",
    @"cursor-off",
    @"userput-on",
    @"userput-off",
    charset,
    @"break-here",
    @"class-of",
    @"object-set-class",
    @"object-get-state",
    @"object-set-state",
    jump,
    @"sound-soft",
    @"sound-channel",
    @"sound-at",
    @"sound-select",
    @"sound-start",
    @"stop-sound",
    @"current-room",
    @"stop-script",
    @"put-actor",
    @"do-animation",
    random,
    @"random-between",
    @"script-running",
    @"actor-room",
    @"actor-x",
    @"actor-y",
    @"actor-get-costume",
    @"palette-color",
    rgb,
    override,
    @"override-off",
    @"sound-running",
    @"load-script",
    @"load-costume",
    @"nuke-sound",
    @"nuke-costume",
    @"lock-script",
    @"lock-costume",
    @"unlock-costume",
    @"load-charset",
    @"preload-sound",
    @"preload-costume",
    @"preload-room",
    @"nuke-image",
    @"lock-image",
    @"preload-image",
    fades,
    palette,
    @"saveload-game",
    @"actor-set-condition",
    @"actor-set-order",
    @"actor-set-clipped",
    @"actor-set-position",
    @"actor-set-clip",
    @"actor-set-costume",
    @"actor-set-sounds",
    @"actor-set-talk-animation",
    @"actor-set-elevation",
    @"actor-set-color",
    @"actor-set-talk-color",
    @"actor-set-scale",
    @"actor-ignore-boxes",
    @"actor-set-animation-speed",
    @"actor-set-shadow",
    @"actor-set-text-offset",
    @"actor-select",
    @"actor-set-var",
    @"actor-new",
    @"palette-select",
    @"palette-from-image",
    @"palette-set-rgb",
    @"palette-set-color",
    @"palette-from-palette",
    @"palette-new",
    @"palette-commit",
    @"find-actor",
    @"find-object",
    @"actor-get-elevation",
    @"valid-verb",
    @"assign-string",
    @"array-assign-list",
    @"array-assign-slice",
    @"array-assign-range",
    sprintf,
    @"array-assign",
    @"array-set-row",
    @"draw-box",
    debug,
    @"wait-for-message",
    @"actor-get-scale",
    in,
    @"update-screen",
    quit,
    @"quit-quit",
    @"sleep-for",
    @"sleep-for-seconds",
    @"stop-sentence",
    @"print-text-position",
    @"print-text-center",
    @"print-text-printf",
    @"print-text-start",
    @"print-debug-string",
    @"print-debug-printf",
    @"print-debug-start",
    @"print-debug-empty",
    @"print-system-string",
    @"print-system-printf",
    @"print-system-start",
    @"say-line-position",
    @"say-line-string",
    @"say-line-talkie",
    @"say-line-color",
    @"say-line-start",
    @"say-line-actor",
    @"say-line",
    @"dim-array.int8",
    @"dim-array.int16",
    @"dim-array.int32",
    @"dim-array.string",
    undim,
    @"return",
    @"call-script",
    @"dim-array-2d.int8",
    @"dim-array-2d.int16",
    @"dim-array-2d.int32",
    @"debug-string",
    abs,
    @"kludge-call",
    kludge,
    @"break-here-multi",
    pick,
    @"debug-input",
    @"get-time-date",
    @"stop-line",
    @"actor-get-var",
    shuffle,
    @"chain-script",
    @"chain-script-rec",
    band,
    bor,
    @"close-file",
    @"open-file",
    @"read-file-int16",
    @"read-file-int8",
    @"write-file-int16",
    @"write-file-int8",
    @"delete-file",
    @"array-line-draw",
    localize,
    @"pick-random",
    @"seek-file",
    @"redim-array.int8",
    @"redim-array.int16",
    @"redim-array.int32",
    @"tell-file",
    @"string-copy",
    @"string-width",
    @"string-length",
    @"string-substr",
    @"string-compare",
    @"costume-loaded",
    @"read-system-ini-int",
    @"read-system-ini-string",
    @"write-system-ini-int",
    @"write-system-ini-string",
    @"string-margin",
    @"string-search",
    @"sound-size",
    @"create-directory",
    @"title-bar",
    @"delete-polygon",
    @"set-polygon",
    @"find-polygon",
};

// stopgap while migrating from string to enum
const OpcodeName = union(enum) {
    op: Op,
    str: []const u8,

    fn from(name: anytype) OpcodeName {
        return switch (@typeInfo(@TypeOf(name))) {
            .enum_literal => .{ .op = name },
            .pointer => .{ .str = name },
            else => unreachable,
        };
    }

    pub fn asStr(self: OpcodeName) []const u8 {
        return switch (self) {
            .op => |op| @tagName(op),
            .str => |str| str,
        };
    }
};

const Opcode = union(enum) {
    unknown,
    ins: LangIns,
    nested: u16,
};

pub const LangIns = struct {
    name: OpcodeName,
    operands: LangOperandArray,
};

pub const LangOperand = enum {
    u8,
    i16,
    i32,
    relative_offset,
    variable,
    string,
};

pub fn buildLanguage(game: Game) Language {
    return if (game != .basketball)
        buildNormalLanguage()
    else
        builtBasketballLanguage();
}

fn buildNormalLanguage() Language {
    var lang: Language = .{};

    lang.add(0x00, .@"push-u8", &.{.u8});
    lang.add(0x01, .@"push-i16", &.{.i16});
    lang.add(0x02, .@"push-i32", &.{.i32});
    lang.add(0x03, .@"push-var", &.{.variable});
    lang.add(0x04, .@"push-str", &.{.string});
    lang.add(0x07, .@"get-array-item", &.{.variable});
    lang.add(0x0a, .@"dup-multi", &.{.i16});
    lang.add(0x0b, .@"get-array-item-2d", &.{.variable});
    lang.add(0x0c, .dup, &.{});
    lang.add(0x0d, .not, &.{});
    lang.add(0x0e, .eq, &.{});
    lang.add(0x0f, .ne, &.{});
    lang.add(0x10, .gt, &.{});
    lang.add(0x11, .lt, &.{});
    lang.add(0x12, .le, &.{});
    lang.add(0x13, .ge, &.{});
    lang.add(0x14, .add, &.{});
    lang.add(0x15, .sub, &.{});
    lang.add(0x16, .mul, &.{});
    lang.add(0x17, .div, &.{});
    lang.add(0x18, .land, &.{});
    lang.add(0x19, .lor, &.{});
    lang.add(0x1a, .pop, &.{});
    lang.add(0x1b, .@"in-list", &.{});

    lang.addNested(0x1c, 0x20, .@"image-set-width", &.{});
    lang.addNested(0x1c, 0x21, .@"image-set-height", &.{});
    lang.addNested(0x1c, 0x30, .@"image-draw", &.{});
    lang.addNested(0x1c, 0x31, .@"image-load-external", &.{});
    lang.addNested(0x1c, 0x33, .@"image-capture", &.{});
    lang.addNested(0x1c, 0x34, .@"image-set-state", &.{});
    lang.addNested(0x1c, 0x36, .@"image-set-flags", &.{});
    lang.addNested(0x1c, 0x38, .@"draw-image-at", &.{});
    lang.addNested(0x1c, 0x39, .@"image-select", &.{});
    lang.addNested(0x1c, 0x41, .@"image-set-pos", &.{});
    lang.addNested(0x1c, 0x42, "image-set-color", &.{});
    lang.addNested(0x1c, 0x43, "image-set-clip", &.{});
    lang.addNested(0x1c, 0x56, .@"image-set-palette", &.{});
    lang.addNested(0x1c, 0x62, .@"image-set-shadow", &.{});
    lang.addNested(0x1c, 0x85, .@"image-set-draw-box", &.{});
    lang.addNested(0x1c, 0x86, "image-set-draw-line", &.{});
    lang.addNested(0x1c, 0x89, .@"image-set-render-image", &.{});
    lang.addNested(0x1c, 0x9a, .@"image-set-hotspot", &.{});
    lang.addNested(0x1c, 0xd9, .@"image-new", &.{});
    lang.addNested(0x1c, 0xf6, "image-set-polygon", &.{});
    lang.addNested(0x1c, 0xf9, "image-unknown-1c-f9", &.{});
    lang.addNested(0x1c, 0xff, .@"image-commit", &.{});

    lang.add(0x1d, .min, &.{});
    lang.add(0x1e, .max, &.{});
    lang.add(0x1f, .sin, &.{});
    lang.add(0x20, .cos, &.{});
    lang.add(0x21, "sqrt", &.{});
    lang.add(0x22, .@"angle-from-delta", &.{});
    lang.add(0x23, .@"angle-from-line", &.{});

    lang.addNested(0x24, 0x1c, .@"line-length-2d", &.{});
    lang.addNested(0x24, 0x1d, .@"line-length-3d", &.{});

    lang.addNested(0x25, 0x1e, "sprite-get-object-x", &.{});
    lang.addNested(0x25, 0x1f, .@"sprite-get-object-y", &.{});
    lang.addNested(0x25, 0x24, .@"sprite-get-state-count", &.{});
    lang.addNested(0x25, 0x25, .@"sprite-get-group", &.{});
    lang.addNested(0x25, 0x26, .@"sprite-get-object-draw-x", &.{});
    lang.addNested(0x25, 0x27, .@"sprite-get-object-draw-y", &.{});
    lang.addNested(0x25, 0x2a, "sprite-get-property", &.{});
    lang.addNested(0x25, 0x2b, .@"sprite-get-order", &.{});
    lang.addNested(0x25, 0x2d, .@"find-sprite", &.{});
    lang.addNested(0x25, 0x34, .@"sprite-get-state", &.{});
    lang.addNested(0x25, 0x3f, .@"sprite-get-image", &.{});
    lang.addNested(0x25, 0x52, "sprite-get-animation", &.{});
    lang.addNested(0x25, 0x56, "sprite-get-palette", &.{});
    lang.addNested(0x25, 0x7c, .@"sprite-get-update-type", &.{});
    lang.addNested(0x25, 0x7d, .@"sprite-class", &.{});
    lang.addNested(0x25, 0xc6, .@"sprite-get-variable", &.{});

    lang.addNested(0x26, 0x25, .@"sprite-set-group", &.{});
    lang.addNested(0x26, 0x2a, .@"sprite-set-property", &.{});
    lang.addNested(0x26, 0x2b, .@"sprite-set-order", &.{});
    lang.addNested(0x26, 0x2c, .@"sprite-move", &.{});
    lang.addNested(0x26, 0x34, .@"sprite-set-state", &.{});
    lang.addNested(0x26, 0x39, .@"sprite-select-range", &.{});
    lang.addNested(0x26, 0x3f, .@"sprite-set-image", &.{});
    lang.addNested(0x26, 0x41, .@"sprite-set-position", &.{});
    lang.addNested(0x26, 0x44, "sprite-erase", &.{});
    lang.addNested(0x26, 0x4d, .@"sprite-set-step-dist", &.{});
    lang.addNested(0x26, 0x52, .@"sprite-set-animation-type", &.{});
    lang.addNested(0x26, 0x56, .@"sprite-set-palette", &.{});
    lang.addNested(0x26, 0x61, .@"sprite-set-animation-speed", &.{});
    lang.addNested(0x26, 0x62, .@"sprite-set-shadow", &.{});
    lang.addNested(0x26, 0x7c, .@"sprite-set-update-type", &.{});
    lang.addNested(0x26, 0x7d, .@"sprite-set-class", &.{});
    lang.addNested(0x26, 0x8c, .@"sprite-mask-image", &.{});
    lang.addNested(0x26, 0x9e, .@"sprite-restart", &.{});
    lang.addNested(0x26, 0xc6, .@"sprite-variable-range", &.{});
    lang.addNested(0x26, 0xd9, .@"sprite-new", &.{});

    lang.addNested(0x27, 0x02, "sprite-group-unknown-27-02", &.{});
    lang.addNested(0x27, 0x08, .@"sprite-group-get", &.{});
    lang.addNested(0x27, 0x1e, .@"sprite-group-get-object-x", &.{});
    lang.addNested(0x27, 0x1f, .@"sprite-group-get-object-y", &.{});
    lang.addNested(0x27, 0x2b, "sprite-group-get-order", &.{});

    lang.addNested(0x28, 0x25, "sprite-group-set-group", &.{});
    lang.addNested(0x28, 0x2b, "sprite-group-set-order", &.{});
    lang.addNested(0x28, 0x2c, .@"sprite-group-move", &.{});
    lang.addNested(0x28, 0x39, .@"sprite-group-select", &.{});
    lang.addNested(0x28, 0x41, .@"sprite-group-set-position", &.{});
    lang.addNested(0x28, 0x43, .@"sprite-group-set-clip", &.{});
    lang.addNested(0x28, 0xd9, .@"sprite-group-new", &.{});

    lang.addNested(0x29, 0x1e, .@"image-get-object-x", &.{});
    lang.addNested(0x29, 0x1f, .@"image-get-object-y", &.{});
    lang.addNested(0x29, 0x20, .@"image-get-width", &.{});
    lang.addNested(0x29, 0x21, .@"image-get-height", &.{});
    lang.addNested(0x29, 0x24, .@"image-get-state-count", &.{});
    lang.addNested(0x29, 0x42, .@"image-get-color-at", &.{});

    lang.add(0x2a, .@"actor-get-property", &.{});

    lang.addNested(0x2b, 0x01, .@"start-script-order", &.{});
    lang.addNested(0x2b, 0xc3, "start-script-rec-order", &.{});

    lang.addNested(0x2c, 0x01, "chain-script-order", &.{});

    lang.add(0x30, .mod, &.{});
    lang.add(0x31, .shl, &.{});
    lang.add(0x32, .shr, &.{});
    lang.add(0x34, .@"find-all-objects", &.{});
    lang.add(0x36, .iif, &.{});

    lang.addNested(0x37, 0x04, .@"dim-array-range.int8", &.{.variable});
    lang.addNested(0x37, 0x05, .@"dim-array-range.int16", &.{.variable});
    lang.addNested(0x37, 0x06, "dim-array-range.int32", &.{.variable});

    lang.add(0x38, "redim-array-range", &.{ .u8, .variable });
    lang.add(0x39, "find-segment-intersection", &.{ .variable, .variable });

    lang.addNested(0x3a, 0x81, .@"array-sort", &.{.variable});

    lang.add(0x43, .set, &.{.variable});
    lang.add(0x46, .@"file-size", &.{});
    lang.add(0x47, .@"set-array-item", &.{.variable});
    lang.add(0x48, .@"string-number", &.{});
    lang.add(0x4b, .@"set-array-item-2d", &.{.variable});

    lang.addNested(0x4d, 0x06, .@"read-ini-int", &.{});
    lang.addNested(0x4d, 0x07, .@"read-ini-string", &.{});

    lang.addNested(0x4e, 0x06, .@"write-ini-int", &.{});
    lang.addNested(0x4e, 0x07, .@"write-ini-string", &.{});

    lang.add(0x4f, .inc, &.{.variable});
    lang.add(0x50, .@"override-off-off", &.{});
    lang.add(0x53, .@"inc-array-item", &.{.variable});
    lang.add(0x54, "get-object-image-x", &.{});
    lang.add(0x55, "get-object-image-y", &.{});
    lang.add(0x57, .dec, &.{.variable});

    lang.addNested(0x58, 0x0a, .@"get-timer", &.{});

    lang.addNested(0x59, 0x9e, "set-timer", &.{});

    lang.add(0x5a, .@"sound-position", &.{});
    lang.add(0x5b, .@"dec-array-item", &.{.variable});
    lang.add(0x5c, .@"jump-if", &.{.relative_offset});
    lang.add(0x5d, .@"jump-unless", &.{.relative_offset});

    lang.addNested(0x5e, 0x01, .@"start-script", &.{});
    lang.addNested(0x5e, 0xc3, .@"start-script-rec", &.{});

    lang.addNested(0x60, 0x01, .@"start-object", &.{});
    lang.addNested(0x60, 0xc3, .@"start-object-rec", &.{});

    lang.addNested(0x61, 0x3f, .@"draw-object", &.{});
    lang.addNested(0x61, 0x41, "draw-object-at", &.{});

    lang.add(0x62, .@"print-image", &.{});

    lang.addNested(0x63, 0x01, .@"array-get-dim", &.{.variable});
    lang.addNested(0x63, 0x02, .@"array-get-height", &.{.variable});
    lang.addNested(0x63, 0x03, .@"array-get-width", &.{.variable});
    lang.addNested(0x63, 0x04, "array-get-x-start", &.{.variable});
    lang.addNested(0x63, 0x05, "array-get-x-end", &.{.variable});
    lang.addNested(0x63, 0x06, "array-get-y-start", &.{.variable});
    lang.addNested(0x63, 0x07, "array-get-y-end", &.{.variable});

    lang.add(0x64, .@"free-arrays", &.{});
    lang.add(0x65, .end2, &.{});
    lang.add(0x66, .end, &.{});

    lang.addNested(0x69, 0x39, .@"window-select", &.{});
    lang.addNested(0x69, 0x3a, .@"window-set-script", &.{});
    lang.addNested(0x69, 0x3f, .@"window-set-image", &.{});
    lang.addNested(0x69, 0xd9, .@"window-new", &.{});
    lang.addNested(0x69, 0xf3, .@"window-set-title-bar", &.{});
    lang.addNested(0x69, 0xff, .@"window-commit", &.{});

    lang.add(0x6a, .@"freeze-scripts", &.{});

    lang.addNested(0x6b, 0x13, .@"cursor-bw", &.{});
    lang.addNested(0x6b, 0x14, .@"cursor-color", &.{});
    lang.addNested(0x6b, 0x90, .@"cursor-on", &.{});
    lang.addNested(0x6b, 0x91, .@"cursor-off", &.{});
    lang.addNested(0x6b, 0x92, .@"userput-on", &.{});
    lang.addNested(0x6b, 0x93, .@"userput-off", &.{});
    lang.addNested(0x6b, 0x94, "cursor-soft-on", &.{});
    lang.addNested(0x6b, 0x95, "cursor-soft-off", &.{});
    lang.addNested(0x6b, 0x9c, .charset, &.{});
    lang.addNested(0x6b, 0x9d, "charset-color", &.{});

    lang.add(0x6c, .@"break-here", &.{});
    lang.add(0x6d, .@"class-of", &.{});
    lang.add(0x6e, .@"object-set-class", &.{});
    lang.add(0x6f, .@"object-get-state", &.{});
    lang.add(0x70, .@"object-set-state", &.{});
    lang.add(0x73, .jump, &.{.relative_offset});

    lang.addNested(0x74, 0x09, .@"sound-soft", &.{});
    lang.addNested(0x74, 0xe6, .@"sound-channel", &.{});
    lang.addNested(0x74, 0xe7, .@"sound-at", &.{});
    lang.addNested(0x74, 0xe8, .@"sound-select", &.{});
    lang.addNested(0x74, 0xf5, "sound-looping", &.{});
    lang.addNested(0x74, 0xff, .@"sound-start", &.{});

    lang.add(0x75, .@"stop-sound", &.{});
    lang.add(0x77, "stop-object", &.{});
    lang.add(0x7b, .@"current-room", &.{});
    lang.add(0x7c, .@"stop-script", &.{});
    lang.add(0x7f, .@"put-actor", &.{});
    lang.add(0x82, .@"do-animation", &.{});
    lang.add(0x87, .random, &.{});
    lang.add(0x88, .@"random-between", &.{});
    lang.add(0x8b, .@"script-running", &.{});
    lang.add(0x8c, .@"actor-room", &.{});
    lang.add(0x8d, .@"actor-x", &.{});
    lang.add(0x8e, .@"actor-y", &.{});
    lang.add(0x8f, "actor-facing", &.{});
    lang.add(0x91, .@"actor-get-costume", &.{});

    lang.addNested(0x94, 0x42, .@"palette-color", &.{});
    lang.addNested(0x94, 0xd9, .rgb, &.{});

    lang.addNested(0x95, 0x73, .override, &.{.relative_offset});

    lang.add(0x96, .@"override-off", &.{});
    lang.add(0x98, .@"sound-running", &.{});

    lang.addNested(0x9b, 0x64, .@"load-script", &.{});
    lang.addNested(0x9b, 0x65, "load-sound", &.{});
    lang.addNested(0x9b, 0x66, .@"load-costume", &.{});
    lang.addNested(0x9b, 0x67, "load-room", &.{});
    lang.addNested(0x9b, 0x69, .@"nuke-sound", &.{});
    lang.addNested(0x9b, 0x6a, .@"nuke-costume", &.{});
    lang.addNested(0x9b, 0x6c, .@"lock-script", &.{});
    lang.addNested(0x9b, 0x6e, .@"lock-costume", &.{});
    lang.addNested(0x9b, 0x72, .@"unlock-costume", &.{});
    lang.addNested(0x9b, 0x75, .@"load-charset", &.{});
    lang.addNested(0x9b, 0x78, "preload-script", &.{});
    lang.addNested(0x9b, 0x79, .@"preload-sound", &.{});
    lang.addNested(0x9b, 0x7a, .@"preload-costume", &.{});
    lang.addNested(0x9b, 0x7b, .@"preload-room", &.{});
    lang.addNested(0x9b, 0x9f, "unlock-image", &.{});
    lang.addNested(0x9b, 0xc0, .@"nuke-image", &.{});
    lang.addNested(0x9b, 0xc9, "load-image", &.{});
    lang.addNested(0x9b, 0xca, .@"lock-image", &.{});
    lang.addNested(0x9b, 0xcb, .@"preload-image", &.{});
    lang.addNested(0x9b, 0xef, "preload-flush", &.{});

    lang.addNested(0x9c, 0xaf, "palette-set", &.{});
    lang.addNested(0x9c, 0xb3, "intensity", &.{});
    lang.addNested(0x9c, 0xb5, .fades, &.{});
    lang.addNested(0x9c, 0xb6, "intensity-rgb", &.{});
    lang.addNested(0x9c, 0xd5, .palette, &.{});
    lang.addNested(0x9c, 0xdc, "copy-palette", &.{});
    lang.addNested(0x9c, 0xdd, .@"saveload-game", &.{});
    lang.addNested(0x9c, 0xea, "object-order", &.{});

    lang.addNested(0x9d, 0x15, .@"actor-set-condition", &.{});
    lang.addNested(0x9d, 0x2b, .@"actor-set-order", &.{});
    lang.addNested(0x9d, 0x40, .@"actor-set-clipped", &.{});
    lang.addNested(0x9d, 0x41, .@"actor-set-position", &.{});
    lang.addNested(0x9d, 0x44, "actor-erase", &.{});
    lang.addNested(0x9d, 0x43, .@"actor-set-clip", &.{});
    lang.addNested(0x9d, 0x4c, .@"actor-set-costume", &.{});
    lang.addNested(0x9d, 0x4e, .@"actor-set-sounds", &.{});
    lang.addNested(0x9d, 0x50, .@"actor-set-talk-animation", &.{});
    lang.addNested(0x9d, 0x54, .@"actor-set-elevation", &.{});
    lang.addNested(0x9d, 0x56, .@"actor-set-color", &.{});
    lang.addNested(0x9d, 0x57, .@"actor-set-talk-color", &.{});
    lang.addNested(0x9d, 0x5c, .@"actor-set-scale", &.{});
    lang.addNested(0x9d, 0x5d, "actor-never-zclip", &.{});
    lang.addNested(0x9d, 0x5e, "actor-always-zclip", &.{});
    lang.addNested(0x9d, 0x5f, .@"actor-ignore-boxes", &.{});
    lang.addNested(0x9d, 0x61, .@"actor-set-animation-speed", &.{});
    lang.addNested(0x9d, 0x62, .@"actor-set-shadow", &.{});
    lang.addNested(0x9d, 0x63, .@"actor-set-text-offset", &.{});
    lang.addNested(0x9d, 0xc5, .@"actor-select", &.{});
    lang.addNested(0x9d, 0xc6, .@"actor-set-var", &.{});
    lang.addNested(0x9d, 0xd9, .@"actor-new", &.{});
    lang.addNested(0x9d, 0xda, "actor-bak-on", &.{});

    lang.addNested(0x9e, 0x39, .@"palette-select", &.{});
    lang.addNested(0x9e, 0x3f, .@"palette-from-image", &.{});
    lang.addNested(0x9e, 0x42, .@"palette-set-rgb", &.{});
    lang.addNested(0x9e, 0x46, .@"palette-set-color", &.{});
    lang.addNested(0x9e, 0x56, .@"palette-from-palette", &.{});
    lang.addNested(0x9e, 0xd9, .@"palette-new", &.{});
    lang.addNested(0x9e, 0xff, .@"palette-commit", &.{});

    lang.add(0x9f, .@"find-actor", &.{});
    lang.add(0xa0, .@"find-object", &.{});
    lang.add(0xa2, .@"actor-get-elevation", &.{});
    lang.add(0xa3, .@"valid-verb", &.{});

    lang.addNested(0xa4, 0x07, .@"assign-string", &.{.variable});
    lang.addNested(0xa4, 0x7e, .@"array-assign-list", &.{.variable});
    lang.addNested(0xa4, 0x7f, .@"array-assign-slice", &.{ .variable, .variable });
    lang.addNested(0xa4, 0x80, .@"array-assign-range", &.{.variable});
    lang.addNested(0xa4, 0x8a, "array-math", &.{ .variable, .variable });
    lang.addNested(0xa4, 0xc2, .sprintf, &.{.variable});
    lang.addNested(0xa4, 0xd0, .@"array-assign", &.{.variable});
    lang.addNested(0xa4, 0xd4, .@"array-set-row", &.{.variable});

    lang.add(0xa6, .@"draw-box", &.{});
    lang.add(0xa7, .debug, &.{});

    lang.addNested(0xa9, 0xa9, .@"wait-for-message", &.{});

    lang.add(0xaa, .@"actor-get-scale", &.{});
    lang.add(0xad, .in, &.{});

    lang.addNested(0xae, 0x16, "flush-object-draw-que", &.{});
    lang.addNested(0xae, 0x1a, .@"update-screen", &.{});
    lang.addNested(0xae, 0xa0, .quit, &.{});
    lang.addNested(0xae, 0xf4, .@"quit-quit", &.{});

    lang.add(0xb0, .@"sleep-for", &.{});
    lang.add(0xb1, .@"sleep-for-seconds", &.{});
    lang.add(0xb3, .@"stop-sentence", &.{});

    lang.addNested(0xb5, 0x41, .@"print-text-position", &.{});
    lang.addNested(0xb5, 0x43, "print-text-clipped", &.{});
    lang.addNested(0xb5, 0x45, .@"print-text-center", &.{});
    lang.addNested(0xb5, 0x4b, "print-text-string", &.{.string});
    lang.addNested(0xb5, 0xc2, .@"print-text-printf", &.{.string});
    lang.addNested(0xb5, 0xf9, "print-text-color", &.{});
    lang.addNested(0xb5, 0xfe, .@"print-text-start", &.{});

    lang.addNested(0xb6, 0x4b, .@"print-debug-string", &.{.string});
    lang.addNested(0xb6, 0xc2, .@"print-debug-printf", &.{.string});
    lang.addNested(0xb6, 0xfe, .@"print-debug-start", &.{});
    lang.addNested(0xb6, 0xff, .@"print-debug-empty", &.{});

    lang.addNested(0xb7, 0x4b, .@"print-system-string", &.{.string});
    lang.addNested(0xb7, 0xc2, .@"print-system-printf", &.{.string});
    lang.addNested(0xb7, 0xfe, .@"print-system-start", &.{});

    lang.addNested(0xb8, 0x41, .@"say-line-position", &.{});
    lang.addNested(0xb8, 0x45, "say-line-center", &.{});
    lang.addNested(0xb8, 0x4b, .@"say-line-string", &.{.string});
    lang.addNested(0xb8, 0xe1, .@"say-line-talkie", &.{});
    lang.addNested(0xb8, 0xf9, .@"say-line-color", &.{});
    lang.addNested(0xb8, 0xfe, .@"say-line-start", &.{});

    lang.addNested(0xb9, 0xfe, "say-line-actor-start", &.{});

    lang.add(0xba, .@"say-line-actor", &.{.string});
    lang.add(0xbb, .@"say-line", &.{.string});

    lang.addNested(0xbc, 0x02, "dim-array.int1", &.{.variable});
    lang.addNested(0xbc, 0x04, .@"dim-array.int8", &.{.variable});
    lang.addNested(0xbc, 0x05, .@"dim-array.int16", &.{.variable});
    lang.addNested(0xbc, 0x06, .@"dim-array.int32", &.{.variable});
    lang.addNested(0xbc, 0x07, .@"dim-array.string", &.{.variable});
    lang.addNested(0xbc, 0xcc, .undim, &.{.variable});

    lang.add(0xbd, .@"return", &.{});
    lang.add(0xbf, .@"call-script", &.{});

    lang.addNested(0xc0, 0x04, .@"dim-array-2d.int8", &.{.variable});
    lang.addNested(0xc0, 0x05, .@"dim-array-2d.int16", &.{.variable});
    lang.addNested(0xc0, 0x06, .@"dim-array-2d.int32", &.{.variable});

    lang.add(0xc1, .@"debug-string", &.{});
    lang.add(0xc4, .abs, &.{});
    lang.add(0xc8, .@"kludge-call", &.{});
    lang.add(0xc9, .kludge, &.{});
    lang.add(0xca, .@"break-here-multi", &.{});
    lang.add(0xcb, .pick, &.{});
    lang.add(0xcd, "stamp-object", &.{});
    lang.add(0xcf, .@"debug-input", &.{});
    lang.add(0xd0, .@"get-time-date", &.{});
    lang.add(0xd1, .@"stop-line", &.{});
    lang.add(0xd2, .@"actor-get-var", &.{});
    lang.add(0xd4, .shuffle, &.{.variable});

    lang.addNested(0xd5, 0x01, .@"chain-script", &.{});
    lang.addNested(0xd5, 0xc3, .@"chain-script-rec", &.{});

    lang.add(0xd6, .band, &.{});
    lang.add(0xd7, .bor, &.{});
    lang.add(0xd9, .@"close-file", &.{});
    lang.add(0xda, .@"open-file", &.{});

    lang.addNested(0xdb, 0x05, .@"read-file-int16", &.{});
    lang.addNested(0xdb, 0x08, .@"read-file-int8", &.{.u8});

    lang.addNested(0xdc, 0x05, .@"write-file-int16", &.{});
    lang.addNested(0xdc, 0x08, .@"write-file-int8", &.{.u8});

    lang.add(0xdd, "find-all-objects2", &.{});
    lang.add(0xde, .@"delete-file", &.{});
    lang.add(0xdf, "rename-file", &.{});

    lang.addNested(0xe0, 0x42, .@"array-line-draw", &.{});

    lang.add(0xe2, .localize, &.{});
    lang.add(0xe3, .@"pick-random", &.{.variable});
    lang.add(0xe9, .@"seek-file", &.{});

    lang.addNested(0xea, 0x04, .@"redim-array.int8", &.{.variable});
    lang.addNested(0xea, 0x05, .@"redim-array.int16", &.{.variable});
    lang.addNested(0xea, 0x06, .@"redim-array.int32", &.{.variable});

    lang.add(0xeb, .@"tell-file", &.{});
    lang.add(0xec, .@"string-copy", &.{});
    lang.add(0xed, .@"string-width", &.{});
    lang.add(0xee, .@"string-length", &.{});
    lang.add(0xef, .@"string-substr", &.{});
    lang.add(0xf0, "string-concat", &.{});
    lang.add(0xf1, .@"string-compare", &.{});

    lang.addNested(0xf2, 0xe3, .@"costume-loaded", &.{});
    lang.addNested(0xf2, 0xe4, "sound-loaded", &.{});

    lang.addNested(0xf3, 0x06, .@"read-system-ini-int", &.{});
    lang.addNested(0xf3, 0x07, .@"read-system-ini-string", &.{});

    lang.addNested(0xf4, 0x06, .@"write-system-ini-int", &.{});
    lang.addNested(0xf4, 0x07, .@"write-system-ini-string", &.{});

    lang.add(0xf5, .@"string-margin", &.{});
    lang.add(0xf6, .@"string-search", &.{});

    lang.addNested(0xf8, 0x0d, .@"sound-size", &.{});

    lang.add(0xf9, .@"create-directory", &.{});

    lang.addNested(0xfa, 0xf3, .@"title-bar", &.{});

    lang.addNested(0xfb, 0xf6, "set-polygon-2", &.{});
    lang.addNested(0xfb, 0xf7, .@"delete-polygon", &.{});
    lang.addNested(0xfb, 0xf8, .@"set-polygon", &.{});

    lang.add(0xfc, .@"find-polygon", &.{});

    return lang;
}

fn builtBasketballLanguage() Language {
    var lang: Language = .{};

    lang.addNested(0x00, 0x04, "actor-set-animation-speed", &.{});
    lang.addNested(0x00, 0x06, "actor-set-position", &.{});
    lang.addNested(0x00, 0x09, "actor-bak-on", &.{});
    lang.addNested(0x00, 0x16, "actor-set-class", &.{});
    lang.addNested(0x00, 0x19, "actor-set-costume", &.{});
    lang.addNested(0x00, 0x35, "actor-new", &.{});
    lang.addNested(0x00, 0x3b, "actor-set-order", &.{});
    lang.addNested(0x00, 0x3f, "actor-set-room-palette", &.{});
    lang.addNested(0x00, 0x41, "actor-set-scale", &.{});
    lang.addNested(0x00, 0x46, "actor-set-shadow", &.{});
    lang.addNested(0x00, 0x53, "actor-set-variable", &.{});
    lang.addNested(0x00, 0x59, "actor-never-zclip", &.{});
    lang.addNested(0x00, 0x80, "actor-set-clipped", &.{});
    lang.addNested(0x00, 0x81, "actor-select", &.{});
    lang.addNested(0x00, 0x82, "actor-set-sounds", &.{});
    lang.addNested(0x00, 0x87, "actor-ignore-boxes", &.{});
    lang.addNested(0x00, 0x8e, "actor-set-talk-condition", &.{});

    lang.add(0x01, "add", &.{});

    lang.addNested(0x03, 0x86, "array-sort", &.{.variable});

    lang.addNested(0x04, 0x23, "sprintf", &.{.variable});
    lang.addNested(0x04, 0x4d, "assign-string", &.{.variable});
    lang.addNested(0x04, 0x81, "array-assign", &.{.variable});
    lang.addNested(0x04, 0x82, "array-assign-list", &.{.variable});
    lang.addNested(0x04, 0x83, "array-assign-slice", &.{ .variable, .variable });
    lang.addNested(0x04, 0x85, "array-assign-range", &.{.variable});

    lang.add(0x05, "band", &.{});
    lang.add(0x06, "bor", &.{});
    lang.add(0x07, "break-here", &.{});
    lang.add(0x08, "break-here-multi", &.{});
    lang.add(0x09, "shl", &.{});
    lang.add(0x0a, "shr", &.{});
    lang.add(0x0b, "xor", &.{});
    lang.add(0x0e, "current-room", &.{});

    lang.addNested(0x11, 0x5a, "chain-script", &.{});
    lang.addNested(0x11, 0x82, "chain-script-rec", &.{});

    lang.add(0x12, "object-set-class", &.{});
    lang.add(0x13, "close-file", &.{});
    lang.add(0x16, "create-directory", &.{});
    lang.add(0x19, "debug", &.{});
    lang.add(0x1b, "dec", &.{.variable});
    lang.add(0x1c, "dec-array-item", &.{.variable});
    lang.add(0x1d, "delete-file", &.{});
    lang.add(0x1e, "dim-array-2d", &.{ .u8, .variable });
    // TODO: first operand is item size; 0x87 means undim
    lang.add(0x1f, "dim-array", &.{ .u8, .variable });
    lang.add(0x20, "div", &.{});
    lang.add(0x21, "do-animation", &.{});
    lang.add(0x23, "draw-box", &.{});

    lang.addNested(0x26, 0x14, "array-line-draw", &.{});

    lang.addNested(0x27, 0x28, "draw-object", &.{});

    lang.add(0x28, "dup", &.{});
    lang.add(0x29, "dup-multi", &.{.i16});
    lang.add(0x2b, "end2", &.{});
    lang.add(0x2c, "end", &.{});
    lang.add(0x2d, "eq", &.{});
    lang.add(0x30, "ge", &.{});
    lang.add(0x31, "get-time-date", &.{});

    lang.addNested(0x32, 0x00, "sprite-group-select", &.{});
    lang.addNested(0x32, 0x06, "sprite-group-set-position", &.{});
    lang.addNested(0x32, 0x12, "sprite-group-set-clip", &.{});
    lang.addNested(0x32, 0x26, "sprite-group-set-group", &.{});
    lang.addNested(0x32, 0x31, "sprite-group-move", &.{});
    lang.addNested(0x32, 0x35, "sprite-group-new", &.{});
    lang.addNested(0x32, 0x3b, "sprite-group-set-order", &.{});
    lang.addNested(0x32, 0x59, "sprite-group-never-zclip", &.{});

    lang.add(0x33, "gt", &.{});

    lang.addNested(0x34, 0x0e, "resource-charset", &.{});
    lang.addNested(0x34, 0x19, "resource-costume", &.{});
    lang.addNested(0x34, 0x28, "resource-image", &.{});
    lang.addNested(0x34, 0x2f, "resource-load", &.{});
    lang.addNested(0x34, 0x3e, "resource-room", &.{});
    lang.addNested(0x34, 0x42, "resource-script", &.{});
    lang.addNested(0x34, 0x48, "resource-sound", &.{});
    lang.addNested(0x34, 0x84, "resource-lock", &.{});
    lang.addNested(0x34, 0x85, "resource-nuke", &.{});
    lang.addNested(0x34, 0x86, "resource-off-heap", &.{});
    lang.addNested(0x34, 0x87, "resource-on-heap", &.{});
    lang.addNested(0x34, 0x88, "resource-preload", &.{});

    lang.add(0x35, "jump-if", &.{.relative_offset});
    lang.add(0x36, "jump-unless", &.{.relative_offset});

    lang.addNested(0x37, 0x00, "image-select", &.{});
    lang.addNested(0x37, 0x06, "image-set-pos", &.{});
    lang.addNested(0x37, 0x07, "image-set-source-image", &.{});
    lang.addNested(0x37, 0x0b, "image-capture", &.{});
    lang.addNested(0x37, 0x12, "image-set-clip", &.{});
    lang.addNested(0x37, 0x1d, "image-draw", &.{});
    lang.addNested(0x37, 0x27, "image-set-height", &.{});
    lang.addNested(0x37, 0x2f, "image-load-external", &.{});
    lang.addNested(0x37, 0x35, "image-new", &.{});
    lang.addNested(0x37, 0x39, "image-set-palette", &.{});
    lang.addNested(0x37, 0x3a, "image-polygon-capture", &.{});
    lang.addNested(0x37, 0x43, "image-set-flags", &.{});
    lang.addNested(0x37, 0x49, "image-set-state", &.{});
    lang.addNested(0x37, 0x54, "image-set-width", &.{});
    lang.addNested(0x37, 0x5c, "image-commit", &.{});
    lang.addNested(0x37, 0x80, "image-font-create", &.{});
    lang.addNested(0x37, 0x81, "image-font-end", &.{});
    lang.addNested(0x37, 0x82, "image-font-render", &.{});
    lang.addNested(0x37, 0x83, "image-font-start", &.{});
    lang.addNested(0x37, 0x84, "image-set-histogram", &.{});
    lang.addNested(0x37, 0x86, "image-flood-fill", &.{});
    lang.addNested(0x37, 0x87, "image-set-render-image", &.{});
    lang.addNested(0x37, 0x8a, "image-set-draw-box", &.{});

    lang.add(0x38, "in-list", &.{});
    lang.add(0x39, "inc", &.{.variable});
    lang.add(0x3a, "inc-array-item", &.{.variable});
    lang.add(0x3b, "jump", &.{.relative_offset});
    lang.add(0x3c, "kludge", &.{});
    lang.add(0x3d, "land", &.{});
    lang.add(0x3e, "le", &.{});
    lang.add(0x3f, "localize", &.{});
    lang.add(0x40, "get-array-item", &.{.variable});
    lang.add(0x41, "get-array-item-2d", &.{.variable});
    lang.add(0x42, "lor", &.{});
    lang.add(0x43, "lt", &.{});
    lang.add(0x44, "mod", &.{});
    lang.add(0x45, "mul", &.{});
    lang.add(0x46, "ne", &.{});
    lang.add(0x47, "dim-array-2d-range", &.{ .u8, .variable });
    lang.add(0x49, "redim-array-range", &.{ .u8, .variable });
    lang.add(0x4a, "not", &.{});
    lang.add(0x4c, "override", &.{ .u8, .relative_offset });
    lang.add(0x4d, "override-off", &.{});
    lang.add(0x4e, "override-off-off", &.{});

    lang.addNested(0x50, 0x00, "palette-select", &.{});
    lang.addNested(0x50, 0x14, "palette-set-rgb", &.{});
    lang.addNested(0x50, 0x19, "palette-from-costume", &.{});
    lang.addNested(0x50, 0x28, "palette-from-image", &.{});
    lang.addNested(0x50, 0x35, "palette-new", &.{});
    lang.addNested(0x50, 0x51, "palette-set-color", &.{});
    lang.addNested(0x50, 0x5c, "palette-commit", &.{});

    lang.addNested(0x52, 0x1c, "delete-polygon", &.{});
    lang.addNested(0x52, 0x44, "set-polygon-2", &.{});
    lang.addNested(0x52, 0x45, "set-polygon", &.{});

    lang.add(0x53, "pop", &.{});

    lang.addNested(0x54, 0x23, "print-debug-printf", &.{.string});
    lang.addNested(0x54, 0x4f, "print-debug-string", &.{.string});
    lang.addNested(0x54, 0x5b, "print-debug-start", &.{});

    lang.add(0x55, "print-image", &.{});

    lang.addNested(0x57, 0x23, "print-system-printf", &.{.string});
    lang.addNested(0x57, 0x4f, "print-system-string", &.{.string});
    lang.addNested(0x57, 0x5b, "print-system-start", &.{});

    lang.addNested(0x58, 0x06, "print-text-position", &.{});
    lang.addNested(0x58, 0x0c, "print-text-center", &.{});
    lang.addNested(0x58, 0x15, "print-text-color", &.{});
    lang.addNested(0x58, 0x23, "print-text-printf", &.{.string});
    lang.addNested(0x58, 0x5b, "print-text-start", &.{});

    lang.addNested(0x5a, 0x82, "start-script-rec-order", &.{});

    lang.add(0x5c, "push-u8", &.{.u8});
    lang.add(0x5d, "push-i32", &.{.i32});
    lang.add(0x5e, "push-str", &.{.string});
    lang.add(0x5f, "push-i16", &.{.i16});
    lang.add(0x60, "push-var", &.{.variable});
    lang.add(0x62, "put-actor", &.{});
    lang.add(0x64, "redim-array", &.{ .u8, .variable });
    lang.add(0x66, "return", &.{});

    lang.addNested(0x68, 0x3f, "palette-set", &.{});
    lang.addNested(0x68, 0x83, "fades", &.{});

    lang.addNested(0x69, 0x06, "say-line-position", &.{});
    lang.addNested(0x69, 0x4e, "say-line-talkie", &.{});
    lang.addNested(0x69, 0x5b, "say-line-start", &.{});

    lang.add(0x6b, "say-line-actor", &.{.string});
    lang.add(0x6e, "seek-file", &.{});

    lang.addNested(0x72, 0x50, "title-bar", &.{});

    lang.add(0x73, "shuffle", &.{.variable});
    lang.add(0x74, "sleep-for", &.{});
    lang.add(0x76, "sleep-for-seconds", &.{});

    lang.addNested(0x77, 0x06, "sound-at", &.{});
    lang.addNested(0x77, 0x5c, "sound-start", &.{});
    lang.addNested(0x77, 0x81, "sound-channel", &.{});
    lang.addNested(0x77, 0x83, "sound-looping", &.{});
    lang.addNested(0x77, 0x84, "sound-select-modify", &.{});
    lang.addNested(0x77, 0x85, "sound-pan", &.{});
    lang.addNested(0x77, 0x86, "sound-select", &.{});
    lang.addNested(0x77, 0x87, "sound-soft", &.{});
    lang.addNested(0x77, 0x88, "sound-volume", &.{});

    lang.addNested(0x79, 0x00, "sprite-select", &.{});
    lang.addNested(0x79, 0x02, "sprite-set-angle", &.{});
    lang.addNested(0x79, 0x03, "sprite-set-animation-type", &.{});
    lang.addNested(0x79, 0x04, "sprite-set-animation-speed", &.{});
    lang.addNested(0x79, 0x06, "sprite-set-position", &.{});
    lang.addNested(0x79, 0x07, "sprite-set-source-image", &.{});
    lang.addNested(0x79, 0x10, "sprite-set-class", &.{});
    lang.addNested(0x79, 0x20, "sprite-erase", &.{});
    lang.addNested(0x79, 0x26, "sprite-set-group", &.{});
    lang.addNested(0x79, 0x28, "sprite-image", &.{});
    lang.addNested(0x79, 0x30, "sprite-mask-image", &.{});
    lang.addNested(0x79, 0x31, "sprite-move", &.{});
    lang.addNested(0x79, 0x35, "sprite-new", &.{});
    lang.addNested(0x79, 0x36, "sprite-set-property2", &.{});
    lang.addNested(0x79, 0x39, "sprite-set-palette", &.{});
    lang.addNested(0x79, 0x3b, "sprite-set-order", &.{});
    lang.addNested(0x79, 0x3c, "sprite-set-property", &.{});
    lang.addNested(0x79, 0x41, "sprite-set-scale", &.{});
    lang.addNested(0x79, 0x46, "sprite-set-shadow", &.{});
    lang.addNested(0x79, 0x49, "sprite-set-state", &.{});
    lang.addNested(0x79, 0x4a, "sprite-set-step-dist", &.{});
    lang.addNested(0x79, 0x52, "sprite-set-update-type", &.{});
    lang.addNested(0x79, 0x53, "sprite-set-variable", &.{});

    lang.addNested(0x7b, 0x5a, "start-object", &.{});
    lang.addNested(0x7b, 0x82, "start-object-rec", &.{});

    lang.addNested(0x7c, 0x5a, "start-script", &.{});
    lang.addNested(0x7c, 0x82, "start-script-rec", &.{});

    lang.add(0x7e, "object-set-state", &.{});
    lang.add(0x80, "stop-script", &.{});
    lang.add(0x81, "stop-sentence", &.{});
    lang.add(0x82, "stop-sound", &.{});
    lang.add(0x83, "stop-line", &.{});
    lang.add(0x84, "set", &.{.variable});
    lang.add(0x85, "set-array-item", &.{.variable});
    lang.add(0x86, "set-array-item-2d", &.{.variable});
    lang.add(0x87, "sub", &.{});

    lang.addNested(0x88, 0x84, "quit", &.{});
    lang.addNested(0x88, 0x85, "quit-quit", &.{});
    lang.addNested(0x88, 0x88, "update-screen", &.{});

    lang.add(0x89, "unknown-89", &.{.u8});

    lang.addNested(0x8a, 0x3d, "set-timer", &.{});

    lang.addNested(0x8b, 0x0e, "charset", &.{});
    lang.addNested(0x8b, 0x0f, "charset-color", &.{});
    lang.addNested(0x8b, 0x80, "cursor-bw", &.{});
    lang.addNested(0x8b, 0x81, "cursor-color", &.{});
    lang.addNested(0x8b, 0x86, "cursor-on", &.{});
    lang.addNested(0x8b, 0x87, "cursor-off", &.{});
    lang.addNested(0x8b, 0x8b, "userput-on", &.{});
    lang.addNested(0x8b, 0x8c, "userput-off", &.{});

    lang.addNested(0x8c, 0x00, "video-select", &.{});
    lang.addNested(0x8c, 0x13, "video-close", &.{});
    lang.addNested(0x8c, 0x2f, "video-load", &.{});
    lang.addNested(0x8c, 0x43, "video-set-flags", &.{});
    lang.addNested(0x8c, 0x5c, "video-commit", &.{});

    lang.addNested(0x8d, 0x82, "wait-for-message", &.{});

    lang.addNested(0x90, 0x05, "write-file-int8", &.{.u8});
    lang.addNested(0x90, 0x2b, "write-file-int32", &.{});

    lang.addNested(0x91, 0x2b, "write-system-ini-int", &.{});
    lang.addNested(0x91, 0x4d, "write-system-ini-string", &.{});

    lang.addNested(0x92, 0x4d, "write-ini-string", &.{});
    lang.addNested(0x92, 0x2b, "write-ini-int", &.{});

    lang.add(0x93, "abs", &.{});
    lang.add(0x95, "actor-get-costume", &.{});
    lang.add(0x99, "actor-get-property", &.{});
    lang.add(0x9a, "actor-room", &.{});
    lang.add(0x9b, "actor-get-scale", &.{});
    lang.add(0x9c, "actor-moving", &.{});
    lang.add(0x9e, "actor-x", &.{});
    lang.add(0x9f, "actor-y", &.{});
    lang.add(0xa0, "angle-from-delta", &.{});
    lang.add(0xa1, "angle-from-line", &.{});

    lang.addNested(0xa3, 0x17, "line-length-2d", &.{});
    lang.addNested(0xa3, 0x18, "line-length-3d", &.{});

    lang.add(0xa4, "class-of", &.{});
    lang.add(0xa6, "iif", &.{});
    lang.add(0xa7, "cos", &.{});
    lang.add(0xa8, "debug-input", &.{});
    lang.add(0xa9, "file-size", &.{});
    lang.add(0xaa, "find-actor", &.{});
    lang.add(0xac, "find-all-objects", &.{});
    lang.add(0xaf, "find-object", &.{});

    lang.addNested(0xb3, 0x00, "font-enumerate-start", &.{});
    lang.addNested(0xb3, 0x3c, "font-enumerate-property", &.{});

    lang.add(0xb4, "free-arrays", &.{});

    lang.addNested(0xb5, 0x01, "array-get-dim", &.{.variable});
    lang.addNested(0xb5, 0x02, "array-get-height", &.{.variable});
    lang.addNested(0xb5, 0x03, "array-get-width", &.{.variable});
    lang.addNested(0xb5, 0x04, "array-get-x-start", &.{.variable});
    lang.addNested(0xb5, 0x05, "array-get-x-end", &.{.variable});
    lang.addNested(0xb5, 0x06, "array-get-y-start", &.{.variable});
    lang.addNested(0xb5, 0x07, "array-get-y-end", &.{.variable});

    lang.addNested(0xb7, 0x48, "sound-size", &.{});

    lang.addNested(0xb8, 0x05, "sprite-group-get", &.{});
    lang.addNested(0xb8, 0x3b, "sprite-group-get-order", &.{});
    lang.addNested(0xb8, 0x55, "sprite-group-get-object-x", &.{});
    lang.addNested(0xb8, 0x56, "sprite-group-get-object-y", &.{});

    lang.addNested(0xba, 0x14, "image-get-color-at", &.{});
    lang.addNested(0xba, 0x1a, "image-get-state-count", &.{});
    lang.addNested(0xba, 0x27, "image-get-height", &.{});
    lang.addNested(0xba, 0x36, "image-get-property2", &.{});
    lang.addNested(0xba, 0x54, "image-get-width", &.{});
    lang.addNested(0xba, 0x55, "image-get-object-x", &.{});
    lang.addNested(0xba, 0x56, "image-get-object-y", &.{});
    lang.addNested(0xba, 0x83, "image-get-font-start", &.{});

    lang.add(0xbc, "in", &.{});
    lang.add(0xbe, "kludge-call", &.{});
    lang.add(0xbf, "max", &.{});
    lang.add(0xc0, "min", &.{});
    lang.add(0xc1, "get-object-image-x", &.{});
    lang.add(0xc2, "get-object-image-y", &.{});
    lang.add(0xc5, "open-file", &.{});
    lang.add(0xc6, "overlap", &.{});

    lang.addNested(0xc8, 0x14, "palette-color", &.{});
    lang.addNested(0xc8, 0x35, "rgb", &.{});
    lang.addNested(0xc8, 0x49, "palette-get-channel", &.{});

    lang.add(0xc9, "pick", &.{});
    lang.add(0xcb, "pick-random", &.{.variable});
    lang.add(0xd0, "random", &.{});
    lang.add(0xd1, "random-between", &.{});

    lang.addNested(0xd3, 0x05, "read-file-int8", &.{.u8});
    lang.addNested(0xd3, 0x2b, "read-file-int32", &.{});

    lang.addNested(0xd4, 0x2b, "read-system-ini-int", &.{});
    lang.addNested(0xd4, 0x4d, "read-system-ini-string", &.{});

    lang.addNested(0xd5, 0x2b, "read-ini-int", &.{});
    lang.addNested(0xd5, 0x4d, "read-ini-string", &.{});

    lang.add(0xd6, "script-running", &.{});
    lang.add(0xd7, "sin", &.{});
    lang.add(0xd8, "sound-position", &.{});
    lang.add(0xd9, "sound-running", &.{});
    lang.add(0xdc, "sqrt", &.{});

    lang.addNested(0xdb, 0x07, "sprite-get-image-at", &.{});
    lang.addNested(0xdb, 0x10, "sprite-class", &.{});
    lang.addNested(0xdb, 0x1a, "sprite-get-state-count", &.{});
    lang.addNested(0xdb, 0x1e, "sprite-get-object-draw-x", &.{});
    lang.addNested(0xdb, 0x1f, "sprite-get-object-draw-y", &.{});
    lang.addNested(0xdb, 0x21, "find-sprite", &.{});
    lang.addNested(0xdb, 0x27, "sprite-get-height", &.{});
    lang.addNested(0xdb, 0x28, "sprite-get-image", &.{});
    lang.addNested(0xdb, 0x39, "sprite-get-palette", &.{});
    lang.addNested(0xdb, 0x3b, "sprite-get-order", &.{});
    lang.addNested(0xdb, 0x49, "sprite-get-state", &.{});
    lang.addNested(0xdb, 0x53, "sprite-get-variable", &.{});
    lang.addNested(0xdb, 0x54, "sprite-get-width", &.{});
    lang.addNested(0xdb, 0x55, "sprite-get-object-x", &.{});
    lang.addNested(0xdb, 0x56, "sprite-get-object-y", &.{});

    lang.add(0xde, "call-script", &.{});
    lang.add(0xe0, "string-compare", &.{});
    lang.add(0xe1, "string-copy", &.{});
    lang.add(0xe2, "string-substr", &.{});
    lang.add(0xe4, "string-length", &.{});
    lang.add(0xe5, "string-margin", &.{});
    lang.add(0xe6, "string-number", &.{});
    lang.add(0xe7, "string-search", &.{});
    lang.add(0xe8, "string-width", &.{});
    lang.add(0xe9, "tell-file", &.{});

    lang.addNested(0xea, 0x32, "get-timer", &.{});

    lang.add(0xeb, "valid-verb", &.{});

    lang.addNested(0xec, 0x49, "video-get-cur-frame", &.{});

    return lang;
}

pub fn buildInsMap(
    allocator: std.mem.Allocator,
    language: *const Language,
) !std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)) {
    var inss: std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)) = .empty;
    errdefer inss.deinit(allocator);
    try inss.ensureUnusedCapacity(allocator, 256);

    for (0..256) |b1_usize| {
        const b1: u8 = @intCast(b1_usize);
        switch (language.opcodes[b1]) {
            .unknown => {},
            .ins => |ins| {
                const bytes = std.BoundedArray(u8, 2).fromSlice(&.{b1}) catch unreachable;
                try inss.putNoClobber(allocator, ins.name.asStr(), bytes);
            },
            .nested => |n| {
                for (0..256) |b2_usize| {
                    const b2: u8 = @intCast(b2_usize);
                    switch (language.opcodes[n << 8 | b2]) {
                        .unknown => {},
                        .ins => |ins| {
                            const bytes = std.BoundedArray(u8, 2).fromSlice(&.{ b1, b2 }) catch unreachable;
                            try inss.putNoClobber(allocator, ins.name.asStr(), bytes);
                        },
                        .nested => unreachable,
                    }
                }
            },
        }
    }

    return inss;
}

pub fn lookup(
    language: *const Language,
    inss: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    name: []const u8,
) ?struct { std.BoundedArray(u8, 2), *const LangIns } {
    const opcode = inss.get(name) orelse return null;
    const ins = switch (opcode.len) {
        1 => &language.opcodes[opcode.get(0)].ins,
        2 => blk: {
            const nest_start = language.opcodes[opcode.get(0)].nested;
            break :blk &language.opcodes[nest_start << 8 | opcode.get(1)].ins;
        },
        else => unreachable,
    };
    return .{ opcode, ins };
}

pub const Ins = struct {
    start: u16,
    end: u16,
    name: OpcodeName,
    operands: OperandArray,
};

pub const Operand = union(LangOperand) {
    u8: u8,
    i16: i16,
    i32: i32,
    relative_offset: i16,
    variable: Variable,
    string: []const u8,
};

pub const Variable = struct {
    raw: u16,

    pub const Kind = enum {
        global,
        local,
        room,
    };

    const Decoded = union(Kind) {
        global: u16,
        local: u16,
        room: u16,
    };

    pub fn init(variable: Decoded) Variable {
        return switch (variable) {
            .global => |n| .{ .raw = n | 0 },
            .local => |n| .{ .raw = n | 0x4000 },
            .room => |n| .{ .raw = n | 0x8000 },
        };
    }

    pub fn init2(kind: Kind, num: u14) Variable {
        const k: u16 = switch (kind) {
            .global => 0,
            .local => 0x4000,
            .room => 0x8000,
        };
        return .{ .raw = k | num };
    }

    pub fn decode(self: Variable) !Decoded {
        return switch (self.raw & 0xc000) {
            0x0000 => .{ .global = self.raw & 0x3fff },
            0x4000 => .{ .local = self.raw & 0x3fff },
            0x8000 => .{ .room = self.raw & 0x3fff },
            0xc000 => error.BadData,
            else => unreachable,
        };
    }

    pub fn decode2(self: Variable) !struct { Kind, u14 } {
        const kind: Kind = switch (self.raw & 0xc000) {
            0x0000 => .global,
            0x4000 => .local,
            0x8000 => .room,
            0xc000 => return error.BadData,
            else => unreachable,
        };
        return .{ kind, @truncate(self.raw) };
    }
};

pub const Disasm = struct {
    lang: *const Language,
    reader: std.io.FixedBufferStream([]const u8),
    poison: bool,

    pub fn init(lang: *const Language, bytecode: []const u8) Disasm {
        const reader = std.io.fixedBufferStream(bytecode);

        return .{
            .lang = lang,
            .reader = reader,
            .poison = false,
        };
    }

    pub fn next(self: *Disasm) !?Ins {
        if (self.reader.pos == self.reader.buffer.len)
            return null;

        if (self.poison)
            return unknownByte(&self.reader);

        const start: u16 = @intCast(self.reader.pos);
        const b1 = self.reader.reader().readByte() catch unreachable;
        return switch (self.lang.opcodes[b1]) {
            .unknown => try self.becomePoison(1),
            .ins => |*ins| try disasmIns(&self.reader, start, ins),
            .nested => |n| {
                const b2 = try self.reader.reader().readByte();
                return switch (self.lang.opcodes[n << 8 | b2]) {
                    .unknown => try self.becomePoison(2),
                    .ins => |*ins| try disasmIns(&self.reader, start, ins),
                    .nested => unreachable,
                };
            },
        };
    }

    // The stream is not self-synchronizing, so if we fail to decode any byte,
    // it's not possible to recover.
    fn becomePoison(self: *Disasm, rewind: u8) !?Ins {
        self.reader.pos -= rewind;
        self.poison = true;
        return unknownByte(&self.reader);
    }
};

// precondition: not at EOF
fn unknownByte(reader: anytype) !?Ins {
    const start: u16 = @intCast(reader.pos);
    const byte = reader.reader().readByte() catch unreachable;
    const end: u16 = @intCast(reader.pos);
    var operands: OperandArray = .{};
    operands.appendAssumeCapacity(.{ .u8 = byte });
    return .{
        .start = start,
        .end = end,
        .name = .{ .str = unknown_byte_ins },
        .operands = operands,
    };
}

fn disasmIns(reader: anytype, start: u16, ins: *const LangIns) !Ins {
    var operands: OperandArray = .{};
    for (ins.operands.slice()) |lang_op| {
        const op = try disasmOperand(reader, lang_op);
        operands.appendAssumeCapacity(op);
    }
    const end: u16 = @intCast(reader.pos);
    return .{
        .start = start,
        .end = end,
        .name = ins.name,
        .operands = operands,
    };
}

fn disasmOperand(reader: anytype, op: LangOperand) !Operand {
    switch (op) {
        .u8 => {
            const n = try reader.reader().readInt(u8, .little);
            return .{ .u8 = n };
        },
        .i16 => {
            const n = try reader.reader().readInt(i16, .little);
            return .{ .i16 = n };
        },
        .i32 => {
            const n = try reader.reader().readInt(i32, .little);
            return .{ .i32 = n };
        },
        .relative_offset => {
            const n = try reader.reader().readInt(i16, .little);
            return .{ .relative_offset = n };
        },
        .variable => {
            const variable = try readVariable(reader);
            return .{ .variable = variable };
        },
        .string => {
            const string = try readString(reader);
            return .{ .string = string };
        },
    }
}

fn readVariable(reader: anytype) !Variable {
    const raw = try reader.reader().readInt(u16, .little);
    return .{ .raw = raw };
}

fn readString(reader: anytype) ![]const u8 {
    const start = reader.pos;
    const null_pos = std.mem.indexOfScalarPos(u8, reader.buffer, start, 0) orelse
        return error.BadData;
    reader.pos = null_pos + 1;
    return reader.buffer[start..null_pos];
}
