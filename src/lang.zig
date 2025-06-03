const std = @import("std");

const Game = @import("games.zig").Game;
const utils = @import("utils.zig");

const LangOperandArray = std.BoundedArray(LangOperand, max_operands);
const OperandArray = std.BoundedArray(Operand, max_operands);

pub const max_operands = 3;

pub const Vm = struct {
    const op_count = @typeInfo(Op).@"enum".fields.len;

    opcodes: [op_count]utils.TinyArray(u8, 3),
    operands: [op_count]LangOperands,
    /// Mapping from one or more opcode bytes, to `Op`s, stored as a flat array
    /// of `Entry`s.
    opcode_lookup: [256 * 53]OpcodeEntry,
};

const OpcodeEntry = struct {
    raw: u16,

    const Decoded = union(enum) {
        op: Op,
        /// Points to the starting offset of 256 subentries.
        nested: u16,
        unset,
    };

    fn encode(entry: Decoded) OpcodeEntry {
        const raw = switch (entry) {
            .op => |op| @intFromEnum(op),
            .nested => |i| 0x8000 | i,
            .unset => 0xffff,
        };
        return .{ .raw = raw };
    }

    fn decode(self: OpcodeEntry) Decoded {
        if (self.raw & 0x8000 == 0)
            return .{ .op = @enumFromInt(self.raw & 0x7fff) };
        if (self.raw == 0xffff)
            return .unset;
        return .{ .nested = self.raw & 0x7fff };
    }
};

const VmBuilder = struct {
    vm: Vm,
    opcode_lookup_pos: u16,

    fn init() VmBuilder {
        var result: VmBuilder = .{
            .vm = .{
                .opcodes = @splat(.empty),
                .operands = undefined,
                .opcode_lookup = undefined,
            },
            .opcode_lookup_pos = 256,
        };
        // Fill in the first 256 (each possible first byte of an opcode)
        @memset(result.vm.opcode_lookup[0..256], .encode(.unset));
        return result;
    }

    fn add1(self: *VmBuilder, byte: u8, op: Op, operands: LangOperands) void {
        self.add(&.{byte}, op, operands);
    }

    fn add2(self: *VmBuilder, b1: u8, b2: u8, op: Op, operands: LangOperands) void {
        self.add(&.{ b1, b2 }, op, operands);
    }

    fn add3(self: *VmBuilder, b1: u8, b2: u8, b3: u8, op: Op, operands: LangOperands) void {
        self.add(&.{ b1, b2, b3 }, op, operands);
    }

    fn add(self: *VmBuilder, bytes: []const u8, op: Op, operands: LangOperands) void {
        std.debug.assert(self.vm.opcodes[@intFromEnum(op)].len == 0);
        self.vm.opcodes[@intFromEnum(op)] = .init(bytes);
        self.vm.operands[@intFromEnum(op)] = operands;

        var start: u16 = 0;
        for (bytes[0 .. bytes.len - 1]) |byte| {
            const index = start + byte;
            switch (self.vm.opcode_lookup[index].decode()) {
                .op => unreachable,
                .nested => {},
                .unset => self.makeNested(index),
            }
            start = self.vm.opcode_lookup[index].decode().nested;
        }
        const index = start + bytes[bytes.len - 1];
        std.debug.assert(self.vm.opcode_lookup[index].decode() == .unset);
        self.vm.opcode_lookup[index] = .encode(.{ .op = op });
    }

    fn makeNested(self: *VmBuilder, index: u16) void {
        const pos = self.opcode_lookup_pos;

        std.debug.assert(self.vm.opcode_lookup[index].decode() == .unset);
        self.vm.opcode_lookup[index] = .encode(.{ .nested = pos });
        @memset(self.vm.opcode_lookup[pos..][0..256], .encode(.unset));

        self.opcode_lookup_pos += 256;
    }
};

/// Packs up to 3 `LangOperand`s into 11 bits
pub const LangOperands = packed struct {
    len: u2,
    @"0": u3,
    @"1": u3,
    @"2": u3,

    const empty: LangOperands = .{
        .len = 0,
        .@"0" = undefined,
        .@"1" = undefined,
        .@"2" = undefined,
    };

    fn mk1(a: LangOperand) LangOperands {
        return .{
            .len = 1,
            .@"0" = @intFromEnum(a),
            .@"1" = undefined,
            .@"2" = undefined,
        };
    }

    fn mk2(a: LangOperand, b: LangOperand) LangOperands {
        return .{
            .len = 2,
            .@"0" = @intFromEnum(a),
            .@"1" = @intFromEnum(b),
            .@"2" = undefined,
        };
    }

    fn mk3(a: LangOperand, b: LangOperand, c: LangOperand) LangOperands {
        return .{
            .len = 3,
            .@"0" = @intFromEnum(a),
            .@"1" = @intFromEnum(b),
            .@"2" = @intFromEnum(c),
        };
    }

    pub fn items(self: LangOperands) std.BoundedArray(LangOperand, 3) {
        var result: std.BoundedArray(LangOperand, 3) = .{};
        if (self.len > 0) result.appendAssumeCapacity(@enumFromInt(self.@"0"));
        if (self.len > 1) result.appendAssumeCapacity(@enumFromInt(self.@"1"));
        if (self.len > 2) result.appendAssumeCapacity(@enumFromInt(self.@"2"));
        return result;
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
    @"image-set-source-image",
    @"image-set-pos",
    @"image-set-color",
    @"image-set-clip",
    @"image-set-palette",
    @"image-set-shadow",
    @"image-set-histogram",
    @"image-polygon-capture",
    @"image-set-draw-box",
    @"image-set-draw-line",
    @"image-flood-fill",
    @"image-set-render-image",
    @"image-set-hotspot",
    @"image-new",
    @"image-set-polygon",
    @"image-unknown-1c-f9",
    @"image-commit",
    min,
    max,
    sin,
    cos,
    sqrt,
    @"angle-from-delta",
    @"angle-from-line",
    @"line-length-2d",
    @"line-length-3d",
    @"sprite-get-object-x",
    @"sprite-get-object-y",
    @"sprite-get-width",
    @"sprite-get-height",
    @"sprite-get-state-count",
    @"sprite-get-group",
    @"sprite-get-object-draw-x",
    @"sprite-get-object-draw-y",
    @"sprite-get-property",
    @"sprite-get-order",
    @"find-sprite",
    @"sprite-get-state",
    @"sprite-get-image-at",
    @"sprite-get-image",
    @"sprite-get-animation",
    @"sprite-get-palette",
    @"sprite-get-update-type",
    @"sprite-class",
    @"sprite-get-variable",
    @"sprite-set-group",
    @"sprite-set-property",
    @"sprite-set-order",
    @"sprite-move",
    @"sprite-set-state",
    @"sprite-set-angle",
    @"sprite-select-one",
    @"sprite-select-range",
    @"sprite-set-source-image",
    @"sprite-set-image",
    @"sprite-set-position",
    @"sprite-erase",
    @"sprite-set-step-dist",
    @"sprite-set-animation-type",
    @"sprite-set-palette",
    @"sprite-set-scale",
    @"sprite-set-animation-speed",
    @"sprite-set-shadow",
    @"sprite-set-update-type",
    @"sprite-set-class",
    @"sprite-set-property2",
    @"sprite-mask-image",
    @"sprite-restart",
    @"sprite-variable-range",
    @"sprite-new",
    @"sprite-group-get",
    @"sprite-group-get-object-x",
    @"sprite-group-get-object-y",
    @"sprite-group-get-order",
    @"sprite-group-set-group",
    @"sprite-group-set-order",
    @"sprite-group-move",
    @"sprite-group-select",
    @"sprite-group-set-position",
    @"sprite-group-set-clip",
    @"sprite-group-never-zclip",
    @"sprite-group-new",
    @"image-get-object-x",
    @"image-get-object-y",
    @"image-get-width",
    @"image-get-height",
    @"image-get-state-count",
    @"image-get-color-at",
    @"image-get-property2",
    @"actor-get-property",
    @"start-script-order",
    @"start-script-rec-order",
    @"chain-script-order",
    @"video-load",
    @"video-set-flags",
    @"video-select",
    @"video-close",
    @"video-commit",
    @"video-get-cur-frame",
    mod,
    shl,
    shr,
    xor,
    @"find-all-objects",
    overlap,
    iif,
    @"dim-array-range.int8",
    @"dim-array-range.int16",
    @"dim-array-range.int32",
    @"redim-array-range.int8",
    @"redim-array-range.int16",
    @"redim-array-range.int32",
    @"find-segment-intersection",
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
    @"get-object-image-x",
    @"get-object-image-y",
    dec,
    @"get-timer",
    @"set-timer",
    @"sound-position",
    @"dec-array-item",
    @"jump-if",
    @"jump-unless",
    @"start-script",
    @"start-script-rec",
    @"start-object",
    @"start-object-rec",
    @"draw-object",
    @"draw-object-at",
    @"print-image",
    @"array-get-dim",
    @"array-get-height",
    @"array-get-width",
    @"array-get-x-start",
    @"array-get-x-end",
    @"array-get-y-start",
    @"array-get-y-end",
    @"free-arrays",
    end2,
    end,
    @"window-select",
    @"window-set-script",
    @"window-set-image",
    @"window-set-position",
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
    @"cursor-soft-on",
    @"cursor-soft-off",
    charset,
    @"charset-color",
    @"break-here",
    @"class-of",
    @"object-set-class",
    @"object-get-state",
    @"object-set-state",
    jump,
    @"sound-soft",
    @"sound-volume",
    @"sound-channel",
    @"sound-at",
    @"sound-select",
    @"sound-looping",
    @"sound-start",
    @"stop-sound",
    @"stop-object",
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
    @"actor-facing",
    @"actor-get-costume",
    @"palette-get-channel",
    @"palette-color",
    rgb,
    override,
    @"override-off",
    @"sound-running",
    @"load-script",
    @"load-sound",
    @"load-costume",
    @"load-room",
    @"nuke-sound",
    @"nuke-costume",
    @"lock-script",
    @"lock-costume",
    @"unlock-costume",
    @"load-charset",
    @"preload-script",
    @"preload-sound",
    @"preload-costume",
    @"preload-room",
    @"unlock-image",
    @"nuke-image",
    @"load-image",
    @"lock-image",
    @"preload-image",
    @"preload-flush",
    @"palette-set",
    intensity,
    fades,
    @"intensity-rgb",
    palette,
    @"copy-palette",
    @"saveload-game",
    @"object-order",
    @"actor-set-condition",
    @"actor-set-talk-condition",
    @"actor-set-order",
    @"actor-set-clipped",
    @"actor-set-position",
    @"actor-set-clip",
    @"actor-erase",
    @"actor-set-costume",
    @"actor-set-sounds",
    @"actor-set-talk-animation",
    @"actor-set-elevation",
    @"actor-set-color",
    @"actor-set-talk-color",
    @"actor-set-scale",
    @"actor-never-zclip",
    @"actor-always-zclip",
    @"actor-ignore-boxes",
    @"actor-set-animation-speed",
    @"actor-set-shadow",
    @"actor-set-text-offset",
    @"actor-set-palette",
    @"actor-select",
    @"actor-set-var",
    @"actor-new",
    @"actor-bak-on",
    @"palette-select",
    @"palette-from-image",
    @"palette-set-rgb",
    @"palette-set-color",
    @"palette-from-costume",
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
    @"array-math",
    sprintf,
    @"array-assign",
    @"array-set-row",
    @"draw-box",
    debug,
    @"wait-for-message",
    @"actor-get-scale",
    in,
    @"flush-object-draw-que",
    @"update-screen",
    quit,
    @"quit-quit",
    @"sleep-for",
    @"sleep-for-seconds",
    @"stop-sentence",
    @"print-text-position",
    @"print-text-clipped",
    @"print-text-center",
    @"print-text-string",
    @"print-text-printf",
    @"print-text-color",
    @"print-text-start",
    @"print-debug-string",
    @"print-debug-printf",
    @"print-debug-start",
    @"print-debug-empty",
    @"print-system-string",
    @"print-system-printf",
    @"print-system-start",
    @"say-line-position",
    @"say-line-center",
    @"say-line-string",
    @"say-line-talkie",
    @"say-line-color",
    @"say-line-start",
    @"say-line-actor-start",
    @"say-line-actor",
    @"say-line",
    @"dim-array.int1",
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
    @"stamp-object",
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
    @"read-file-int32",
    @"read-file-int8",
    @"write-file-int16",
    @"write-file-int32",
    @"write-file-int8",
    @"find-all-objects2",
    @"delete-file",
    @"rename-file",
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
    @"string-concat",
    @"string-compare",
    @"costume-loaded",
    @"sound-loaded",
    @"read-system-ini-int",
    @"read-system-ini-string",
    @"write-system-ini-int",
    @"write-system-ini-string",
    @"string-margin",
    @"string-search",
    @"sound-size",
    @"create-directory",
    @"title-bar",
    @"set-polygon-2",
    @"delete-polygon",
    @"set-polygon",
    @"find-polygon",

    // New in Basketball:

    @"resource-charset",
    @"resource-costume",
    @"resource-image",
    @"resource-load",
    @"resource-room",
    @"resource-script",
    @"resource-sound",
    @"resource-lock",
    @"resource-nuke",
    @"resource-off-heap",
    @"resource-on-heap",
    @"resource-preload",
    @"image-font-create",
    @"image-font-end",
    @"image-font-render",
    @"image-font-start",
    @"sound-select-modify",
    @"sound-pan",
    @"font-enumerate-start",
    @"font-enumerate-property",
    @"image-get-font-start",
};

pub const LangOperand = enum {
    u8,
    i16,
    i32,
    relative_offset,
    variable,
    string,
};

pub fn buildVm(game: Game) Vm {
    return if (game != .basketball)
        buildNormalVm(game)
    else
        builtBasketballVm();
}

fn buildNormalVm(game: Game) Vm {
    var b: VmBuilder = .init();

    b.add1(0x00, .@"push-u8", .mk1(.u8));
    b.add1(0x01, .@"push-i16", .mk1(.i16));
    b.add1(0x02, .@"push-i32", .mk1(.i32));
    b.add1(0x03, .@"push-var", .mk1(.variable));
    b.add1(0x04, .@"push-str", .mk1(.string));
    b.add1(0x07, .@"get-array-item", .mk1(.variable));
    b.add1(0x0a, .@"dup-multi", .mk1(.i16));
    b.add1(0x0b, .@"get-array-item-2d", .mk1(.variable));
    b.add1(0x0c, .dup, .empty);
    b.add1(0x0d, .not, .empty);
    b.add1(0x0e, .eq, .empty);
    b.add1(0x0f, .ne, .empty);
    b.add1(0x10, .gt, .empty);
    b.add1(0x11, .lt, .empty);
    b.add1(0x12, .le, .empty);
    b.add1(0x13, .ge, .empty);
    b.add1(0x14, .add, .empty);
    b.add1(0x15, .sub, .empty);
    b.add1(0x16, .mul, .empty);
    b.add1(0x17, .div, .empty);
    b.add1(0x18, .land, .empty);
    b.add1(0x19, .lor, .empty);
    b.add1(0x1a, .pop, .empty);
    b.add1(0x1b, .@"in-list", .empty);

    b.add2(0x1c, 0x20, .@"image-set-width", .empty);
    b.add2(0x1c, 0x21, .@"image-set-height", .empty);
    b.add2(0x1c, 0x30, .@"image-draw", .empty);
    b.add2(0x1c, 0x31, .@"image-load-external", .empty);
    b.add2(0x1c, 0x33, .@"image-capture", .empty);
    b.add2(0x1c, 0x34, .@"image-set-state", .empty);
    b.add2(0x1c, 0x36, .@"image-set-flags", .empty);
    b.add2(0x1c, 0x38, .@"draw-image-at", .empty);
    b.add2(0x1c, 0x39, .@"image-select", .empty);
    b.add2(0x1c, 0x41, .@"image-set-pos", .empty);
    b.add2(0x1c, 0x42, .@"image-set-color", .empty);
    b.add2(0x1c, 0x43, .@"image-set-clip", .empty);
    b.add2(0x1c, 0x56, .@"image-set-palette", .empty);
    b.add2(0x1c, 0x62, .@"image-set-shadow", .empty);
    b.add2(0x1c, 0x85, .@"image-set-draw-box", .empty);
    b.add2(0x1c, 0x86, .@"image-set-draw-line", .empty);
    b.add2(0x1c, 0x89, .@"image-set-render-image", .empty);
    b.add2(0x1c, 0x9a, .@"image-set-hotspot", .empty);
    b.add2(0x1c, 0xd9, .@"image-new", .empty);
    b.add2(0x1c, 0xf6, .@"image-set-polygon", .empty);
    b.add2(0x1c, 0xf9, .@"image-unknown-1c-f9", .empty);
    b.add2(0x1c, 0xff, .@"image-commit", .empty);

    b.add1(0x1d, .min, .empty);
    b.add1(0x1e, .max, .empty);
    b.add1(0x1f, .sin, .empty);
    b.add1(0x20, .cos, .empty);
    b.add1(0x21, .sqrt, .empty);
    b.add1(0x22, .@"angle-from-delta", .empty);
    b.add1(0x23, .@"angle-from-line", .empty);

    b.add2(0x24, 0x1c, .@"line-length-2d", .empty);
    b.add2(0x24, 0x1d, .@"line-length-3d", .empty);

    b.add2(0x25, 0x1e, .@"sprite-get-object-x", .empty);
    b.add2(0x25, 0x1f, .@"sprite-get-object-y", .empty);
    b.add2(0x25, 0x24, .@"sprite-get-state-count", .empty);
    b.add2(0x25, 0x25, .@"sprite-get-group", .empty);
    b.add2(0x25, 0x26, .@"sprite-get-object-draw-x", .empty);
    b.add2(0x25, 0x27, .@"sprite-get-object-draw-y", .empty);
    b.add2(0x25, 0x2a, .@"sprite-get-property", .empty);
    b.add2(0x25, 0x2b, .@"sprite-get-order", .empty);
    b.add2(0x25, 0x2d, .@"find-sprite", .empty);
    b.add2(0x25, 0x34, .@"sprite-get-state", .empty);
    b.add2(0x25, 0x3f, .@"sprite-get-image", .empty);
    b.add2(0x25, 0x52, .@"sprite-get-animation", .empty);
    b.add2(0x25, 0x56, .@"sprite-get-palette", .empty);
    b.add2(0x25, 0x7c, .@"sprite-get-update-type", .empty);
    b.add2(0x25, 0x7d, .@"sprite-class", .empty);
    b.add2(0x25, 0xc6, .@"sprite-get-variable", .empty);

    b.add2(0x26, 0x25, .@"sprite-set-group", .empty);
    b.add2(0x26, 0x2a, .@"sprite-set-property", .empty);
    b.add2(0x26, 0x2b, .@"sprite-set-order", .empty);
    b.add2(0x26, 0x2c, .@"sprite-move", .empty);
    b.add2(0x26, 0x34, .@"sprite-set-state", .empty);
    if (game.le(.soccer_1998))
        b.add2(0x26, 0x39, .@"sprite-select-one", .empty)
    else
        b.add2(0x26, 0x39, .@"sprite-select-range", .empty);
    b.add2(0x26, 0x3f, .@"sprite-set-image", .empty);
    b.add2(0x26, 0x41, .@"sprite-set-position", .empty);
    b.add2(0x26, 0x44, .@"sprite-erase", .empty);
    b.add2(0x26, 0x4d, .@"sprite-set-step-dist", .empty);
    b.add2(0x26, 0x52, .@"sprite-set-animation-type", .empty);
    b.add2(0x26, 0x56, .@"sprite-set-palette", .empty);
    b.add2(0x26, 0x61, .@"sprite-set-animation-speed", .empty);
    b.add2(0x26, 0x62, .@"sprite-set-shadow", .empty);
    b.add2(0x26, 0x7c, .@"sprite-set-update-type", .empty);
    b.add2(0x26, 0x7d, .@"sprite-set-class", .empty);
    b.add2(0x26, 0x8c, .@"sprite-mask-image", .empty);
    b.add2(0x26, 0x9e, .@"sprite-restart", .empty);
    b.add2(0x26, 0xc6, .@"sprite-variable-range", .empty);
    b.add2(0x26, 0xd9, .@"sprite-new", .empty);

    b.add2(0x27, 0x08, .@"sprite-group-get", .empty);
    b.add2(0x27, 0x1e, .@"sprite-group-get-object-x", .empty);
    b.add2(0x27, 0x1f, .@"sprite-group-get-object-y", .empty);
    b.add2(0x27, 0x2b, .@"sprite-group-get-order", .empty);

    b.add2(0x28, 0x25, .@"sprite-group-set-group", .empty);
    b.add2(0x28, 0x2b, .@"sprite-group-set-order", .empty);
    b.add2(0x28, 0x2c, .@"sprite-group-move", .empty);
    b.add2(0x28, 0x39, .@"sprite-group-select", .empty);
    b.add2(0x28, 0x41, .@"sprite-group-set-position", .empty);
    b.add2(0x28, 0x43, .@"sprite-group-set-clip", .empty);
    b.add2(0x28, 0xd9, .@"sprite-group-new", .empty);

    b.add2(0x29, 0x1e, .@"image-get-object-x", .empty);
    b.add2(0x29, 0x1f, .@"image-get-object-y", .empty);
    b.add2(0x29, 0x20, .@"image-get-width", .empty);
    b.add2(0x29, 0x21, .@"image-get-height", .empty);
    b.add2(0x29, 0x24, .@"image-get-state-count", .empty);
    b.add2(0x29, 0x42, .@"image-get-color-at", .empty);

    b.add1(0x2a, .@"actor-get-property", .empty);

    b.add2(0x2b, 0x01, .@"start-script-order", .empty);
    b.add2(0x2b, 0xc3, .@"start-script-rec-order", .empty);

    b.add2(0x2c, 0x01, .@"chain-script-order", .empty);

    b.add1(0x30, .mod, .empty);
    b.add1(0x31, .shl, .empty);
    b.add1(0x32, .shr, .empty);
    b.add1(0x34, .@"find-all-objects", .empty);
    b.add1(0x36, .iif, .empty);

    b.add2(0x37, 0x04, .@"dim-array-range.int8", .mk1(.variable));
    b.add2(0x37, 0x05, .@"dim-array-range.int16", .mk1(.variable));
    b.add2(0x37, 0x06, .@"dim-array-range.int32", .mk1(.variable));

    b.add2(0x38, 0x04, .@"redim-array-range.int8", .mk1(.variable));
    b.add2(0x38, 0x05, .@"redim-array-range.int16", .mk1(.variable));
    b.add2(0x38, 0x06, .@"redim-array-range.int32", .mk1(.variable));

    b.add1(0x39, .@"find-segment-intersection", .mk2(.variable, .variable));

    b.add2(0x3a, 0x81, .@"array-sort", .mk1(.variable));

    b.add1(0x43, .set, .mk1(.variable));
    b.add1(0x46, .@"file-size", .empty);
    b.add1(0x47, .@"set-array-item", .mk1(.variable));
    b.add1(0x48, .@"string-number", .empty);
    b.add1(0x4b, .@"set-array-item-2d", .mk1(.variable));

    b.add2(0x4d, 0x06, .@"read-ini-int", .empty);
    b.add2(0x4d, 0x07, .@"read-ini-string", .empty);

    b.add2(0x4e, 0x06, .@"write-ini-int", .empty);
    b.add2(0x4e, 0x07, .@"write-ini-string", .empty);

    b.add1(0x4f, .inc, .mk1(.variable));
    b.add1(0x50, .@"override-off-off", .empty);
    b.add1(0x53, .@"inc-array-item", .mk1(.variable));
    b.add1(0x54, .@"get-object-image-x", .empty);
    b.add1(0x55, .@"get-object-image-y", .empty);
    b.add1(0x57, .dec, .mk1(.variable));

    b.add2(0x58, 0x0a, .@"get-timer", .empty);

    b.add2(0x59, 0x9e, .@"set-timer", .empty);

    b.add1(0x5a, .@"sound-position", .empty);
    b.add1(0x5b, .@"dec-array-item", .mk1(.variable));
    b.add1(0x5c, .@"jump-if", .mk1(.relative_offset));
    b.add1(0x5d, .@"jump-unless", .mk1(.relative_offset));

    b.add2(0x5e, 0x01, .@"start-script", .empty);
    b.add2(0x5e, 0xc3, .@"start-script-rec", .empty);

    b.add2(0x60, 0x01, .@"start-object", .empty);
    b.add2(0x60, 0xc3, .@"start-object-rec", .empty);

    b.add2(0x61, 0x3f, .@"draw-object", .empty);
    b.add2(0x61, 0x41, .@"draw-object-at", .empty);

    b.add1(0x62, .@"print-image", .empty);

    b.add2(0x63, 0x01, .@"array-get-dim", .mk1(.variable));
    b.add2(0x63, 0x02, .@"array-get-height", .mk1(.variable));
    b.add2(0x63, 0x03, .@"array-get-width", .mk1(.variable));
    b.add2(0x63, 0x04, .@"array-get-x-start", .mk1(.variable));
    b.add2(0x63, 0x05, .@"array-get-x-end", .mk1(.variable));
    b.add2(0x63, 0x06, .@"array-get-y-start", .mk1(.variable));
    b.add2(0x63, 0x07, .@"array-get-y-end", .mk1(.variable));

    b.add1(0x64, .@"free-arrays", .empty);
    b.add1(0x65, .end2, .empty);
    b.add1(0x66, .end, .empty);

    b.add2(0x69, 0x39, .@"window-select", .empty);
    b.add2(0x69, 0x3a, .@"window-set-script", .empty);
    b.add2(0x69, 0x3f, .@"window-set-image", .empty);
    b.add2(0x69, 0xd9, .@"window-new", .empty);
    b.add2(0x69, 0xf3, .@"window-set-title-bar", .empty);
    b.add2(0x69, 0xff, .@"window-commit", .empty);

    b.add1(0x6a, .@"freeze-scripts", .empty);

    b.add2(0x6b, 0x13, .@"cursor-bw", .empty);
    b.add2(0x6b, 0x14, .@"cursor-color", .empty);
    b.add2(0x6b, 0x90, .@"cursor-on", .empty);
    b.add2(0x6b, 0x91, .@"cursor-off", .empty);
    b.add2(0x6b, 0x92, .@"userput-on", .empty);
    b.add2(0x6b, 0x93, .@"userput-off", .empty);
    b.add2(0x6b, 0x94, .@"cursor-soft-on", .empty);
    b.add2(0x6b, 0x95, .@"cursor-soft-off", .empty);
    b.add2(0x6b, 0x9c, .charset, .empty);
    b.add2(0x6b, 0x9d, .@"charset-color", .empty);

    b.add1(0x6c, .@"break-here", .empty);
    b.add1(0x6d, .@"class-of", .empty);
    b.add1(0x6e, .@"object-set-class", .empty);
    b.add1(0x6f, .@"object-get-state", .empty);
    b.add1(0x70, .@"object-set-state", .empty);
    b.add1(0x73, .jump, .mk1(.relative_offset));

    b.add2(0x74, 0x09, .@"sound-soft", .empty);
    b.add2(0x74, 0xe6, .@"sound-channel", .empty);
    b.add2(0x74, 0xe7, .@"sound-at", .empty);
    b.add2(0x74, 0xe8, .@"sound-select", .empty);
    b.add2(0x74, 0xf5, .@"sound-looping", .empty);
    b.add2(0x74, 0xff, .@"sound-start", .empty);

    b.add1(0x75, .@"stop-sound", .empty);
    b.add1(0x77, .@"stop-object", .empty);
    b.add1(0x7b, .@"current-room", .empty);
    b.add1(0x7c, .@"stop-script", .empty);
    b.add1(0x7f, .@"put-actor", .empty);
    b.add1(0x82, .@"do-animation", .empty);
    b.add1(0x87, .random, .empty);
    b.add1(0x88, .@"random-between", .empty);
    b.add1(0x8b, .@"script-running", .empty);
    b.add1(0x8c, .@"actor-room", .empty);
    b.add1(0x8d, .@"actor-x", .empty);
    b.add1(0x8e, .@"actor-y", .empty);
    b.add1(0x8f, .@"actor-facing", .empty);
    b.add1(0x91, .@"actor-get-costume", .empty);

    b.add2(0x94, 0x42, .@"palette-color", .empty);
    b.add2(0x94, 0xd9, .rgb, .empty);

    b.add2(0x95, 0x73, .override, .mk1(.relative_offset));

    b.add1(0x96, .@"override-off", .empty);
    b.add1(0x98, .@"sound-running", .empty);

    b.add2(0x9b, 0x64, .@"load-script", .empty);
    b.add2(0x9b, 0x65, .@"load-sound", .empty);
    b.add2(0x9b, 0x66, .@"load-costume", .empty);
    b.add2(0x9b, 0x67, .@"load-room", .empty);
    b.add2(0x9b, 0x69, .@"nuke-sound", .empty);
    b.add2(0x9b, 0x6a, .@"nuke-costume", .empty);
    b.add2(0x9b, 0x6c, .@"lock-script", .empty);
    b.add2(0x9b, 0x6e, .@"lock-costume", .empty);
    b.add2(0x9b, 0x72, .@"unlock-costume", .empty);
    b.add2(0x9b, 0x75, .@"load-charset", .empty);
    b.add2(0x9b, 0x78, .@"preload-script", .empty);
    b.add2(0x9b, 0x79, .@"preload-sound", .empty);
    b.add2(0x9b, 0x7a, .@"preload-costume", .empty);
    b.add2(0x9b, 0x7b, .@"preload-room", .empty);
    b.add2(0x9b, 0x9f, .@"unlock-image", .empty);
    b.add2(0x9b, 0xc0, .@"nuke-image", .empty);
    b.add2(0x9b, 0xc9, .@"load-image", .empty);
    b.add2(0x9b, 0xca, .@"lock-image", .empty);
    b.add2(0x9b, 0xcb, .@"preload-image", .empty);
    b.add2(0x9b, 0xef, .@"preload-flush", .empty);

    b.add2(0x9c, 0xaf, .@"palette-set", .empty);
    b.add2(0x9c, 0xb3, .intensity, .empty);
    b.add2(0x9c, 0xb5, .fades, .empty);
    b.add2(0x9c, 0xb6, .@"intensity-rgb", .empty);
    b.add2(0x9c, 0xd5, .palette, .empty);
    b.add2(0x9c, 0xdc, .@"copy-palette", .empty);
    b.add2(0x9c, 0xdd, .@"saveload-game", .empty);
    b.add2(0x9c, 0xea, .@"object-order", .empty);

    b.add2(0x9d, 0x15, .@"actor-set-condition", .empty);
    b.add2(0x9d, 0x2b, .@"actor-set-order", .empty);
    b.add2(0x9d, 0x40, .@"actor-set-clipped", .empty);
    b.add2(0x9d, 0x41, .@"actor-set-position", .empty);
    b.add2(0x9d, 0x43, .@"actor-set-clip", .empty);
    b.add2(0x9d, 0x44, .@"actor-erase", .empty);
    b.add2(0x9d, 0x4c, .@"actor-set-costume", .empty);
    b.add2(0x9d, 0x4e, .@"actor-set-sounds", .empty);
    b.add2(0x9d, 0x50, .@"actor-set-talk-animation", .empty);
    b.add2(0x9d, 0x54, .@"actor-set-elevation", .empty);
    b.add2(0x9d, 0x56, .@"actor-set-color", .empty);
    b.add2(0x9d, 0x57, .@"actor-set-talk-color", .empty);
    b.add2(0x9d, 0x5c, .@"actor-set-scale", .empty);
    b.add2(0x9d, 0x5d, .@"actor-never-zclip", .empty);
    b.add2(0x9d, 0x5e, .@"actor-always-zclip", .empty);
    b.add2(0x9d, 0x5f, .@"actor-ignore-boxes", .empty);
    b.add2(0x9d, 0x61, .@"actor-set-animation-speed", .empty);
    b.add2(0x9d, 0x62, .@"actor-set-shadow", .empty);
    b.add2(0x9d, 0x63, .@"actor-set-text-offset", .empty);
    b.add2(0x9d, 0xc5, .@"actor-select", .empty);
    b.add2(0x9d, 0xc6, .@"actor-set-var", .empty);
    b.add2(0x9d, 0xd9, .@"actor-new", .empty);
    b.add2(0x9d, 0xda, .@"actor-bak-on", .empty);

    b.add2(0x9e, 0x39, .@"palette-select", .empty);
    b.add2(0x9e, 0x3f, .@"palette-from-image", .empty);
    b.add2(0x9e, 0x42, .@"palette-set-rgb", .empty);
    b.add2(0x9e, 0x46, .@"palette-set-color", .empty);
    b.add2(0x9e, 0x56, .@"palette-from-palette", .empty);
    b.add2(0x9e, 0xd9, .@"palette-new", .empty);
    b.add2(0x9e, 0xff, .@"palette-commit", .empty);

    b.add1(0x9f, .@"find-actor", .empty);
    b.add1(0xa0, .@"find-object", .empty);
    b.add1(0xa2, .@"actor-get-elevation", .empty);
    b.add1(0xa3, .@"valid-verb", .empty);

    b.add2(0xa4, 0x07, .@"assign-string", .mk1(.variable));
    b.add2(0xa4, 0x7e, .@"array-assign-list", .mk1(.variable));
    b.add2(0xa4, 0x7f, .@"array-assign-slice", .mk2(.variable, .variable));
    b.add2(0xa4, 0x80, .@"array-assign-range", .mk1(.variable));
    b.add2(0xa4, 0x8a, .@"array-math", .mk3(.variable, .variable, .variable));
    b.add2(0xa4, 0xc2, .sprintf, .mk1(.variable));
    b.add2(0xa4, 0xd0, .@"array-assign", .mk1(.variable));
    b.add2(0xa4, 0xd4, .@"array-set-row", .mk1(.variable));

    b.add1(0xa6, .@"draw-box", .empty);
    b.add1(0xa7, .debug, .empty);

    b.add2(0xa9, 0xa9, .@"wait-for-message", .empty);

    b.add1(0xaa, .@"actor-get-scale", .empty);
    b.add1(0xad, .in, .empty);

    b.add2(0xae, 0x16, .@"flush-object-draw-que", .empty);
    b.add2(0xae, 0x1a, .@"update-screen", .empty);
    b.add2(0xae, 0xa0, .quit, .empty);
    b.add2(0xae, 0xf4, .@"quit-quit", .empty);

    b.add1(0xb0, .@"sleep-for", .empty);
    b.add1(0xb1, .@"sleep-for-seconds", .empty);
    b.add1(0xb3, .@"stop-sentence", .empty);

    b.add2(0xb5, 0x41, .@"print-text-position", .empty);
    b.add2(0xb5, 0x43, .@"print-text-clipped", .empty);
    b.add2(0xb5, 0x45, .@"print-text-center", .empty);
    b.add2(0xb5, 0x4b, .@"print-text-string", .mk1(.string));
    b.add2(0xb5, 0xc2, .@"print-text-printf", .mk1(.string));
    b.add2(0xb5, 0xf9, .@"print-text-color", .empty);
    b.add2(0xb5, 0xfe, .@"print-text-start", .empty);

    b.add2(0xb6, 0x4b, .@"print-debug-string", .mk1(.string));
    b.add2(0xb6, 0xc2, .@"print-debug-printf", .mk1(.string));
    b.add2(0xb6, 0xfe, .@"print-debug-start", .empty);
    b.add2(0xb6, 0xff, .@"print-debug-empty", .empty);

    b.add2(0xb7, 0x4b, .@"print-system-string", .mk1(.string));
    b.add2(0xb7, 0xc2, .@"print-system-printf", .mk1(.string));
    b.add2(0xb7, 0xfe, .@"print-system-start", .empty);

    b.add2(0xb8, 0x41, .@"say-line-position", .empty);
    b.add2(0xb8, 0x45, .@"say-line-center", .empty);
    b.add2(0xb8, 0x4b, .@"say-line-string", .mk1(.string));
    b.add2(0xb8, 0xe1, .@"say-line-talkie", .empty);
    b.add2(0xb8, 0xf9, .@"say-line-color", .empty);
    b.add2(0xb8, 0xfe, .@"say-line-start", .empty);

    b.add2(0xb9, 0xfe, .@"say-line-actor-start", .empty);

    b.add1(0xba, .@"say-line-actor", .mk1(.string));
    b.add1(0xbb, .@"say-line", .mk1(.string));

    b.add2(0xbc, 0x02, .@"dim-array.int1", .mk1(.variable));
    b.add2(0xbc, 0x04, .@"dim-array.int8", .mk1(.variable));
    b.add2(0xbc, 0x05, .@"dim-array.int16", .mk1(.variable));
    b.add2(0xbc, 0x06, .@"dim-array.int32", .mk1(.variable));
    b.add2(0xbc, 0x07, .@"dim-array.string", .mk1(.variable));
    b.add2(0xbc, 0xcc, .undim, .mk1(.variable));

    b.add1(0xbd, .@"return", .empty);
    b.add1(0xbf, .@"call-script", .empty);

    b.add2(0xc0, 0x04, .@"dim-array-2d.int8", .mk1(.variable));
    b.add2(0xc0, 0x05, .@"dim-array-2d.int16", .mk1(.variable));
    b.add2(0xc0, 0x06, .@"dim-array-2d.int32", .mk1(.variable));

    b.add1(0xc1, .@"debug-string", .empty);
    b.add1(0xc4, .abs, .empty);
    b.add1(0xc8, .@"kludge-call", .empty);
    b.add1(0xc9, .kludge, .empty);
    b.add1(0xca, .@"break-here-multi", .empty);
    b.add1(0xcb, .pick, .empty);
    b.add1(0xcd, .@"stamp-object", .empty);
    b.add1(0xcf, .@"debug-input", .empty);
    b.add1(0xd0, .@"get-time-date", .empty);
    b.add1(0xd1, .@"stop-line", .empty);
    b.add1(0xd2, .@"actor-get-var", .empty);
    b.add1(0xd4, .shuffle, .mk1(.variable));

    b.add2(0xd5, 0x01, .@"chain-script", .empty);
    b.add2(0xd5, 0xc3, .@"chain-script-rec", .empty);

    b.add1(0xd6, .band, .empty);
    b.add1(0xd7, .bor, .empty);
    b.add1(0xd9, .@"close-file", .empty);
    b.add1(0xda, .@"open-file", .empty);

    b.add2(0xdb, 0x05, .@"read-file-int16", .empty);
    b.add2(0xdb, 0x06, .@"read-file-int32", .empty);
    b.add3(0xdb, 0x08, 0x04, .@"read-file-int8", .empty);

    b.add2(0xdc, 0x05, .@"write-file-int16", .empty);
    b.add2(0xdc, 0x06, .@"write-file-int32", .empty);
    b.add3(0xdc, 0x08, 0x04, .@"write-file-int8", .empty);

    b.add1(0xdd, .@"find-all-objects2", .empty);
    b.add1(0xde, .@"delete-file", .empty);
    b.add1(0xdf, .@"rename-file", .empty);

    b.add2(0xe0, 0x42, .@"array-line-draw", .empty);

    b.add1(0xe2, .localize, .empty);
    b.add1(0xe3, .@"pick-random", .mk1(.variable));
    b.add1(0xe9, .@"seek-file", .empty);

    b.add2(0xea, 0x04, .@"redim-array.int8", .mk1(.variable));
    b.add2(0xea, 0x05, .@"redim-array.int16", .mk1(.variable));
    b.add2(0xea, 0x06, .@"redim-array.int32", .mk1(.variable));

    b.add1(0xeb, .@"tell-file", .empty);
    b.add1(0xec, .@"string-copy", .empty);
    b.add1(0xed, .@"string-width", .empty);
    b.add1(0xee, .@"string-length", .empty);
    b.add1(0xef, .@"string-substr", .empty);
    b.add1(0xf0, .@"string-concat", .empty);
    b.add1(0xf1, .@"string-compare", .empty);

    b.add2(0xf2, 0xe3, .@"costume-loaded", .empty);
    b.add2(0xf2, 0xe4, .@"sound-loaded", .empty);

    b.add2(0xf3, 0x06, .@"read-system-ini-int", .empty);
    b.add2(0xf3, 0x07, .@"read-system-ini-string", .empty);

    b.add2(0xf4, 0x06, .@"write-system-ini-int", .empty);
    b.add2(0xf4, 0x07, .@"write-system-ini-string", .empty);

    b.add1(0xf5, .@"string-margin", .empty);
    b.add1(0xf6, .@"string-search", .empty);

    b.add2(0xf8, 0x0d, .@"sound-size", .empty);

    b.add1(0xf9, .@"create-directory", .empty);

    b.add2(0xfa, 0xf3, .@"title-bar", .empty);

    b.add2(0xfb, 0xf6, .@"set-polygon-2", .empty);
    b.add2(0xfb, 0xf7, .@"delete-polygon", .empty);
    b.add2(0xfb, 0xf8, .@"set-polygon", .empty);

    b.add1(0xfc, .@"find-polygon", .empty);

    return b.vm;
}

fn builtBasketballVm() Vm {
    var b: VmBuilder = .init();

    b.add2(0x00, 0x04, .@"actor-set-animation-speed", .empty);
    b.add2(0x00, 0x06, .@"actor-set-position", .empty);
    b.add2(0x00, 0x09, .@"actor-bak-on", .empty);
    b.add2(0x00, 0x16, .@"actor-set-condition", .empty);
    b.add2(0x00, 0x19, .@"actor-set-costume", .empty);
    b.add2(0x00, 0x35, .@"actor-new", .empty);
    b.add2(0x00, 0x3b, .@"actor-set-order", .empty);
    b.add2(0x00, 0x3f, .@"actor-set-palette", .empty);
    b.add2(0x00, 0x41, .@"actor-set-scale", .empty);
    b.add2(0x00, 0x46, .@"actor-set-shadow", .empty);
    b.add2(0x00, 0x53, .@"actor-set-var", .empty);
    b.add2(0x00, 0x59, .@"actor-never-zclip", .empty);
    b.add2(0x00, 0x80, .@"actor-set-clipped", .empty);
    b.add2(0x00, 0x81, .@"actor-select", .empty);
    b.add2(0x00, 0x82, .@"actor-set-sounds", .empty);
    b.add2(0x00, 0x87, .@"actor-ignore-boxes", .empty);
    b.add2(0x00, 0x8e, .@"actor-set-talk-condition", .empty);

    b.add1(0x01, .add, .empty);

    b.add2(0x03, 0x86, .@"array-sort", .mk1(.variable));

    b.add2(0x04, 0x23, .sprintf, .mk1(.variable));
    b.add2(0x04, 0x4d, .@"assign-string", .mk1(.variable));
    b.add2(0x04, 0x81, .@"array-assign", .mk1(.variable));
    b.add2(0x04, 0x82, .@"array-assign-list", .mk1(.variable));
    b.add2(0x04, 0x83, .@"array-assign-slice", .mk2(.variable, .variable));
    b.add2(0x04, 0x85, .@"array-assign-range", .mk1(.variable));

    b.add1(0x05, .band, .empty);
    b.add1(0x06, .bor, .empty);
    b.add1(0x07, .@"break-here", .empty);
    b.add1(0x08, .@"break-here-multi", .empty);
    b.add1(0x09, .shl, .empty);
    b.add1(0x0a, .shr, .empty);
    b.add1(0x0b, .xor, .empty);
    b.add1(0x0e, .@"current-room", .empty);

    b.add2(0x11, 0x5a, .@"chain-script", .empty);
    b.add2(0x11, 0x82, .@"chain-script-rec", .empty);

    b.add1(0x12, .@"object-set-class", .empty);
    b.add1(0x13, .@"close-file", .empty);
    b.add1(0x16, .@"create-directory", .empty);
    b.add1(0x19, .debug, .empty);
    b.add1(0x1b, .dec, .mk1(.variable));
    b.add1(0x1c, .@"dec-array-item", .mk1(.variable));
    b.add1(0x1d, .@"delete-file", .empty);

    b.add2(0x1e, 0x2a, .@"dim-array-2d.int16", .mk1(.variable));
    b.add2(0x1e, 0x2b, .@"dim-array-2d.int32", .mk1(.variable));
    b.add2(0x1e, 0x2d, .@"dim-array-2d.int8", .mk1(.variable));

    b.add2(0x1f, 0x29, .@"dim-array.int1", .mk1(.variable));
    b.add2(0x1f, 0x2a, .@"dim-array.int16", .mk1(.variable));
    b.add2(0x1f, 0x2b, .@"dim-array.int32", .mk1(.variable));
    b.add2(0x1f, 0x2d, .@"dim-array.int8", .mk1(.variable));
    b.add2(0x1f, 0x4d, .@"dim-array.string", .mk1(.variable));
    b.add2(0x1f, 0x87, .undim, .mk1(.variable));

    b.add1(0x20, .div, .empty);
    b.add1(0x21, .@"do-animation", .empty);
    b.add1(0x23, .@"draw-box", .empty);

    b.add2(0x26, 0x14, .@"array-line-draw", .empty);

    b.add2(0x27, 0x28, .@"draw-object", .empty);

    b.add1(0x28, .dup, .empty);
    b.add1(0x29, .@"dup-multi", .mk1(.i16));
    b.add1(0x2b, .end2, .empty);
    b.add1(0x2c, .end, .empty);
    b.add1(0x2d, .eq, .empty);
    b.add1(0x30, .ge, .empty);
    b.add1(0x31, .@"get-time-date", .empty);

    b.add2(0x32, 0x00, .@"sprite-group-select", .empty);
    b.add2(0x32, 0x06, .@"sprite-group-set-position", .empty);
    b.add2(0x32, 0x12, .@"sprite-group-set-clip", .empty);
    b.add2(0x32, 0x26, .@"sprite-group-set-group", .empty);
    b.add2(0x32, 0x31, .@"sprite-group-move", .empty);
    b.add2(0x32, 0x35, .@"sprite-group-new", .empty);
    b.add2(0x32, 0x3b, .@"sprite-group-set-order", .empty);
    b.add2(0x32, 0x59, .@"sprite-group-never-zclip", .empty);

    b.add1(0x33, .gt, .empty);

    b.add2(0x34, 0x0e, .@"resource-charset", .empty);
    b.add2(0x34, 0x19, .@"resource-costume", .empty);
    b.add2(0x34, 0x28, .@"resource-image", .empty);
    b.add2(0x34, 0x2f, .@"resource-load", .empty);
    b.add2(0x34, 0x3e, .@"resource-room", .empty);
    b.add2(0x34, 0x42, .@"resource-script", .empty);
    b.add2(0x34, 0x48, .@"resource-sound", .empty);
    b.add2(0x34, 0x84, .@"resource-lock", .empty);
    b.add2(0x34, 0x85, .@"resource-nuke", .empty);
    b.add2(0x34, 0x86, .@"resource-off-heap", .empty);
    b.add2(0x34, 0x87, .@"resource-on-heap", .empty);
    b.add2(0x34, 0x88, .@"resource-preload", .empty);

    b.add1(0x35, .@"jump-if", .mk1(.relative_offset));
    b.add1(0x36, .@"jump-unless", .mk1(.relative_offset));

    b.add2(0x37, 0x00, .@"image-select", .empty);
    b.add2(0x37, 0x06, .@"image-set-pos", .empty);
    b.add2(0x37, 0x07, .@"image-set-source-image", .empty);
    b.add2(0x37, 0x0b, .@"image-capture", .empty);
    b.add2(0x37, 0x12, .@"image-set-clip", .empty);
    b.add2(0x37, 0x1d, .@"image-draw", .empty);
    b.add2(0x37, 0x27, .@"image-set-height", .empty);
    b.add2(0x37, 0x2f, .@"image-load-external", .empty);
    b.add2(0x37, 0x35, .@"image-new", .empty);
    b.add2(0x37, 0x39, .@"image-set-palette", .empty);
    b.add2(0x37, 0x3a, .@"image-polygon-capture", .empty);
    b.add2(0x37, 0x43, .@"image-set-flags", .empty);
    b.add2(0x37, 0x49, .@"image-set-state", .empty);
    b.add2(0x37, 0x54, .@"image-set-width", .empty);
    b.add2(0x37, 0x5c, .@"image-commit", .empty);
    b.add2(0x37, 0x80, .@"image-font-create", .empty);
    b.add2(0x37, 0x81, .@"image-font-end", .empty);
    b.add2(0x37, 0x82, .@"image-font-render", .empty);
    b.add2(0x37, 0x83, .@"image-font-start", .empty);
    b.add2(0x37, 0x84, .@"image-set-histogram", .empty);
    b.add2(0x37, 0x86, .@"image-flood-fill", .empty);
    b.add2(0x37, 0x87, .@"image-set-render-image", .empty);
    b.add2(0x37, 0x8a, .@"image-set-draw-box", .empty);

    b.add1(0x38, .@"in-list", .empty);
    b.add1(0x39, .inc, .mk1(.variable));
    b.add1(0x3a, .@"inc-array-item", .mk1(.variable));
    b.add1(0x3b, .jump, .mk1(.relative_offset));
    b.add1(0x3c, .kludge, .empty);
    b.add1(0x3d, .land, .empty);
    b.add1(0x3e, .le, .empty);
    b.add1(0x3f, .localize, .empty);
    b.add1(0x40, .@"get-array-item", .mk1(.variable));
    b.add1(0x41, .@"get-array-item-2d", .mk1(.variable));
    b.add1(0x42, .lor, .empty);
    b.add1(0x43, .lt, .empty);
    b.add1(0x44, .mod, .empty);
    b.add1(0x45, .mul, .empty);
    b.add1(0x46, .ne, .empty);

    b.add2(0x47, 0x2a, .@"dim-array-range.int16", .mk1(.variable));
    b.add2(0x47, 0x2b, .@"dim-array-range.int32", .mk1(.variable));
    b.add2(0x47, 0x2d, .@"dim-array-range.int8", .mk1(.variable));

    b.add2(0x49, 0x2a, .@"redim-array-range.int16", .mk1(.variable));
    b.add2(0x49, 0x2b, .@"redim-array-range.int32", .mk1(.variable));
    b.add2(0x49, 0x2d, .@"redim-array-range.int8", .mk1(.variable));

    b.add1(0x4a, .not, .empty);

    b.add2(0x4c, 0x3b, .override, .mk1(.relative_offset));

    b.add1(0x4d, .@"override-off", .empty);
    b.add1(0x4e, .@"override-off-off", .empty);

    b.add2(0x50, 0x00, .@"palette-select", .empty);
    b.add2(0x50, 0x14, .@"palette-set-rgb", .empty);
    b.add2(0x50, 0x19, .@"palette-from-costume", .empty);
    b.add2(0x50, 0x28, .@"palette-from-image", .empty);
    b.add2(0x50, 0x35, .@"palette-new", .empty);
    b.add2(0x50, 0x51, .@"palette-set-color", .empty);
    b.add2(0x50, 0x5c, .@"palette-commit", .empty);

    b.add2(0x52, 0x1c, .@"delete-polygon", .empty);
    b.add2(0x52, 0x44, .@"set-polygon-2", .empty);
    b.add2(0x52, 0x45, .@"set-polygon", .empty);

    b.add1(0x53, .pop, .empty);

    b.add2(0x54, 0x23, .@"print-debug-printf", .mk1(.string));
    b.add2(0x54, 0x4f, .@"print-debug-string", .mk1(.string));
    b.add2(0x54, 0x5b, .@"print-debug-start", .empty);

    b.add1(0x55, .@"print-image", .empty);

    b.add2(0x57, 0x23, .@"print-system-printf", .mk1(.string));
    b.add2(0x57, 0x4f, .@"print-system-string", .mk1(.string));
    b.add2(0x57, 0x5b, .@"print-system-start", .empty);

    b.add2(0x58, 0x06, .@"print-text-position", .empty);
    b.add2(0x58, 0x0c, .@"print-text-center", .empty);
    b.add2(0x58, 0x15, .@"print-text-color", .empty);
    b.add2(0x58, 0x23, .@"print-text-printf", .mk1(.string));
    b.add2(0x58, 0x5b, .@"print-text-start", .empty);

    b.add2(0x5a, 0x82, .@"start-script-rec-order", .empty);

    b.add1(0x5c, .@"push-u8", .mk1(.u8));
    b.add1(0x5d, .@"push-i32", .mk1(.i32));
    b.add1(0x5e, .@"push-str", .mk1(.string));
    b.add1(0x5f, .@"push-i16", .mk1(.i16));
    b.add1(0x60, .@"push-var", .mk1(.variable));
    b.add1(0x62, .@"put-actor", .empty);

    b.add2(0x64, 0x2b, .@"redim-array.int32", .mk1(.variable));
    b.add2(0x64, 0x2d, .@"redim-array.int8", .mk1(.variable));

    b.add1(0x66, .@"return", .empty);

    b.add2(0x68, 0x3f, .@"palette-set", .empty);
    b.add2(0x68, 0x83, .fades, .empty);

    b.add2(0x69, 0x06, .@"say-line-position", .empty);
    b.add2(0x69, 0x4e, .@"say-line-talkie", .empty);
    b.add2(0x69, 0x5b, .@"say-line-start", .empty);

    b.add1(0x6b, .@"say-line-actor", .mk1(.string));
    b.add1(0x6e, .@"seek-file", .empty);

    b.add2(0x72, 0x50, .@"title-bar", .empty);

    b.add1(0x73, .shuffle, .mk1(.variable));
    b.add1(0x74, .@"sleep-for", .empty);
    b.add1(0x76, .@"sleep-for-seconds", .empty);

    b.add2(0x77, 0x06, .@"sound-at", .empty);
    b.add2(0x77, 0x5c, .@"sound-start", .empty);
    b.add2(0x77, 0x81, .@"sound-channel", .empty);
    b.add2(0x77, 0x83, .@"sound-looping", .empty);
    b.add2(0x77, 0x84, .@"sound-select-modify", .empty);
    b.add2(0x77, 0x85, .@"sound-pan", .empty);
    b.add2(0x77, 0x86, .@"sound-select", .empty);
    b.add2(0x77, 0x87, .@"sound-soft", .empty);
    b.add2(0x77, 0x88, .@"sound-volume", .empty);

    b.add2(0x79, 0x00, .@"sprite-select-range", .empty);
    b.add2(0x79, 0x02, .@"sprite-set-angle", .empty);
    b.add2(0x79, 0x03, .@"sprite-set-animation-type", .empty);
    b.add2(0x79, 0x04, .@"sprite-set-animation-speed", .empty);
    b.add2(0x79, 0x06, .@"sprite-set-position", .empty);
    b.add2(0x79, 0x07, .@"sprite-set-source-image", .empty);
    b.add2(0x79, 0x10, .@"sprite-set-class", .empty);
    b.add2(0x79, 0x20, .@"sprite-erase", .empty);
    b.add2(0x79, 0x26, .@"sprite-set-group", .empty);
    b.add2(0x79, 0x28, .@"sprite-set-image", .empty);
    b.add2(0x79, 0x30, .@"sprite-mask-image", .empty);
    b.add2(0x79, 0x31, .@"sprite-move", .empty);
    b.add2(0x79, 0x35, .@"sprite-new", .empty);
    b.add2(0x79, 0x36, .@"sprite-set-property2", .empty);
    b.add2(0x79, 0x39, .@"sprite-set-palette", .empty);
    b.add2(0x79, 0x3b, .@"sprite-set-order", .empty);
    b.add2(0x79, 0x3c, .@"sprite-set-property", .empty);
    b.add2(0x79, 0x41, .@"sprite-set-scale", .empty);
    b.add2(0x79, 0x46, .@"sprite-set-shadow", .empty);
    b.add2(0x79, 0x49, .@"sprite-set-state", .empty);
    b.add2(0x79, 0x4a, .@"sprite-set-step-dist", .empty);
    b.add2(0x79, 0x52, .@"sprite-set-update-type", .empty);
    b.add2(0x79, 0x53, .@"sprite-variable-range", .empty);

    b.add2(0x7b, 0x5a, .@"start-object", .empty);
    b.add2(0x7b, 0x82, .@"start-object-rec", .empty);

    b.add2(0x7c, 0x5a, .@"start-script", .empty);
    b.add2(0x7c, 0x82, .@"start-script-rec", .empty);

    b.add1(0x7e, .@"object-set-state", .empty);
    b.add1(0x80, .@"stop-script", .empty);
    b.add1(0x81, .@"stop-sentence", .empty);
    b.add1(0x82, .@"stop-sound", .empty);
    b.add1(0x83, .@"stop-line", .empty);
    b.add1(0x84, .set, .mk1(.variable));
    b.add1(0x85, .@"set-array-item", .mk1(.variable));
    b.add1(0x86, .@"set-array-item-2d", .mk1(.variable));
    b.add1(0x87, .sub, .empty);

    b.add2(0x88, 0x84, .quit, .empty);
    b.add2(0x88, 0x85, .@"quit-quit", .empty);
    b.add2(0x88, 0x88, .@"update-screen", .empty);

    b.add2(0x89, 0x00, .@"window-select", .empty);
    b.add2(0x89, 0x06, .@"window-set-position", .empty);
    b.add2(0x89, 0x28, .@"window-set-image", .empty);
    b.add2(0x89, 0x35, .@"window-new", .empty);
    b.add2(0x89, 0x50, .@"window-set-title-bar", .empty);
    b.add2(0x89, 0x5c, .@"window-commit", .empty);

    b.add2(0x8a, 0x3d, .@"set-timer", .empty);

    b.add2(0x8b, 0x0e, .charset, .empty);
    b.add2(0x8b, 0x0f, .@"charset-color", .empty);
    b.add2(0x8b, 0x80, .@"cursor-bw", .empty);
    b.add2(0x8b, 0x81, .@"cursor-color", .empty);
    b.add2(0x8b, 0x86, .@"cursor-on", .empty);
    b.add2(0x8b, 0x87, .@"cursor-off", .empty);
    b.add2(0x8b, 0x8b, .@"userput-on", .empty);
    b.add2(0x8b, 0x8c, .@"userput-off", .empty);

    b.add2(0x8c, 0x00, .@"video-select", .empty);
    b.add2(0x8c, 0x13, .@"video-close", .empty);
    b.add2(0x8c, 0x2f, .@"video-load", .empty);
    b.add2(0x8c, 0x43, .@"video-set-flags", .empty);
    b.add2(0x8c, 0x5c, .@"video-commit", .empty);

    b.add2(0x8d, 0x82, .@"wait-for-message", .empty);

    b.add3(0x90, 0x05, 0x2d, .@"write-file-int8", .empty);
    b.add2(0x90, 0x2b, .@"write-file-int32", .empty);

    b.add2(0x91, 0x2b, .@"write-system-ini-int", .empty);
    b.add2(0x91, 0x4d, .@"write-system-ini-string", .empty);

    b.add2(0x92, 0x4d, .@"write-ini-string", .empty);
    b.add2(0x92, 0x2b, .@"write-ini-int", .empty);

    b.add1(0x93, .abs, .empty);
    b.add1(0x95, .@"actor-get-costume", .empty);
    b.add1(0x99, .@"actor-get-property", .empty);
    b.add1(0x9a, .@"actor-room", .empty);
    b.add1(0x9b, .@"actor-get-scale", .empty);
    b.add1(0x9c, .@"actor-get-var", .empty);
    b.add1(0x9e, .@"actor-x", .empty);
    b.add1(0x9f, .@"actor-y", .empty);
    b.add1(0xa0, .@"angle-from-delta", .empty);
    b.add1(0xa1, .@"angle-from-line", .empty);

    b.add2(0xa3, 0x17, .@"line-length-2d", .empty);
    b.add2(0xa3, 0x18, .@"line-length-3d", .empty);

    b.add1(0xa4, .@"class-of", .empty);
    b.add1(0xa6, .iif, .empty);
    b.add1(0xa7, .cos, .empty);
    b.add1(0xa8, .@"debug-input", .empty);
    b.add1(0xa9, .@"file-size", .empty);
    b.add1(0xaa, .@"find-actor", .empty);
    b.add1(0xac, .@"find-all-objects", .empty);
    b.add1(0xaf, .@"find-object", .empty);

    b.add2(0xb3, 0x00, .@"font-enumerate-start", .empty);
    b.add2(0xb3, 0x3c, .@"font-enumerate-property", .empty);

    b.add1(0xb4, .@"free-arrays", .empty);

    b.add2(0xb5, 0x01, .@"array-get-dim", .mk1(.variable));
    b.add2(0xb5, 0x02, .@"array-get-height", .mk1(.variable));
    b.add2(0xb5, 0x03, .@"array-get-width", .mk1(.variable));
    b.add2(0xb5, 0x04, .@"array-get-x-start", .mk1(.variable));
    b.add2(0xb5, 0x05, .@"array-get-x-end", .mk1(.variable));
    b.add2(0xb5, 0x06, .@"array-get-y-start", .mk1(.variable));
    b.add2(0xb5, 0x07, .@"array-get-y-end", .mk1(.variable));

    b.add2(0xb7, 0x48, .@"sound-size", .empty);

    b.add2(0xb8, 0x05, .@"sprite-group-get", .empty);
    b.add2(0xb8, 0x3b, .@"sprite-group-get-order", .empty);
    b.add2(0xb8, 0x55, .@"sprite-group-get-object-x", .empty);
    b.add2(0xb8, 0x56, .@"sprite-group-get-object-y", .empty);

    b.add2(0xba, 0x14, .@"image-get-color-at", .empty);
    b.add2(0xba, 0x1a, .@"image-get-state-count", .empty);
    b.add2(0xba, 0x27, .@"image-get-height", .empty);
    b.add2(0xba, 0x36, .@"image-get-property2", .empty);
    b.add2(0xba, 0x54, .@"image-get-width", .empty);
    b.add2(0xba, 0x55, .@"image-get-object-x", .empty);
    b.add2(0xba, 0x56, .@"image-get-object-y", .empty);
    b.add2(0xba, 0x83, .@"image-get-font-start", .empty);

    b.add1(0xbc, .in, .empty);
    b.add1(0xbe, .@"kludge-call", .empty);
    b.add1(0xbf, .max, .empty);
    b.add1(0xc0, .min, .empty);
    b.add1(0xc1, .@"get-object-image-x", .empty);
    b.add1(0xc2, .@"get-object-image-y", .empty);
    b.add1(0xc5, .@"open-file", .empty);
    b.add1(0xc6, .overlap, .empty);

    b.add2(0xc8, 0x14, .@"palette-color", .empty);
    b.add2(0xc8, 0x35, .rgb, .empty);
    b.add2(0xc8, 0x49, .@"palette-get-channel", .empty);

    b.add1(0xc9, .pick, .empty);
    b.add1(0xcb, .@"pick-random", .mk1(.variable));
    b.add1(0xd0, .random, .empty);
    b.add1(0xd1, .@"random-between", .empty);

    b.add3(0xd3, 0x05, 0x2d, .@"read-file-int8", .empty);
    b.add2(0xd3, 0x2b, .@"read-file-int32", .empty);

    b.add2(0xd4, 0x2b, .@"read-system-ini-int", .empty);
    b.add2(0xd4, 0x4d, .@"read-system-ini-string", .empty);

    b.add2(0xd5, 0x2b, .@"read-ini-int", .empty);
    b.add2(0xd5, 0x4d, .@"read-ini-string", .empty);

    b.add1(0xd6, .@"script-running", .empty);
    b.add1(0xd7, .sin, .empty);
    b.add1(0xd8, .@"sound-position", .empty);
    b.add1(0xd9, .@"sound-running", .empty);
    b.add1(0xdc, .sqrt, .empty);

    b.add2(0xdb, 0x07, .@"sprite-get-image-at", .empty);
    b.add2(0xdb, 0x10, .@"sprite-class", .empty);
    b.add2(0xdb, 0x1a, .@"sprite-get-state-count", .empty);
    b.add2(0xdb, 0x1e, .@"sprite-get-object-draw-x", .empty);
    b.add2(0xdb, 0x1f, .@"sprite-get-object-draw-y", .empty);
    b.add2(0xdb, 0x21, .@"find-sprite", .empty);
    b.add2(0xdb, 0x27, .@"sprite-get-height", .empty);
    b.add2(0xdb, 0x28, .@"sprite-get-image", .empty);
    b.add2(0xdb, 0x39, .@"sprite-get-palette", .empty);
    b.add2(0xdb, 0x3b, .@"sprite-get-order", .empty);
    b.add2(0xdb, 0x49, .@"sprite-get-state", .empty);
    b.add2(0xdb, 0x53, .@"sprite-get-variable", .empty);
    b.add2(0xdb, 0x54, .@"sprite-get-width", .empty);
    b.add2(0xdb, 0x55, .@"sprite-get-object-x", .empty);
    b.add2(0xdb, 0x56, .@"sprite-get-object-y", .empty);

    b.add1(0xde, .@"call-script", .empty);
    b.add1(0xe0, .@"string-compare", .empty);
    b.add1(0xe1, .@"string-copy", .empty);
    b.add1(0xe2, .@"string-substr", .empty);
    b.add1(0xe4, .@"string-length", .empty);
    b.add1(0xe5, .@"string-margin", .empty);
    b.add1(0xe6, .@"string-number", .empty);
    b.add1(0xe7, .@"string-search", .empty);
    b.add1(0xe8, .@"string-width", .empty);
    b.add1(0xe9, .@"tell-file", .empty);

    b.add2(0xea, 0x32, .@"get-timer", .empty);

    b.add1(0xeb, .@"valid-verb", .empty);

    b.add2(0xec, 0x49, .@"video-get-cur-frame", .empty);

    return b.vm;
}

pub const LangIns = struct {
    op: Op,
    opcode: utils.TinyArray(u8, 3),
    operands: LangOperands,
};

pub fn lookup(vm: *const Vm, name: []const u8) ?LangIns {
    const op = std.meta.stringToEnum(Op, name) orelse return null;
    const opcode = vm.opcodes[@intFromEnum(op)];
    if (opcode.len == 0) return null;
    const operands = vm.operands[@intFromEnum(op)];
    return .{ .op = op, .opcode = opcode, .operands = operands };
}

pub const Ins = struct {
    start: u16,
    end: u16,
    op: union(enum) { op: Op, unknown_byte },
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
    vm: *const Vm,
    reader: std.io.FixedBufferStream([]const u8),
    poison: bool,

    pub fn init(vm: *const Vm, bytecode: []const u8) Disasm {
        const reader = std.io.fixedBufferStream(bytecode);

        return .{
            .vm = vm,
            .reader = reader,
            .poison = false,
        };
    }

    pub fn next(self: *Disasm) !?Ins {
        if (self.reader.pos == self.reader.buffer.len)
            return null;

        if (self.poison)
            return unknownByte(&self.reader);

        const ins_start: u16 = @intCast(self.reader.pos);
        // Follow the nested entries until we find the op
        var group_pos: u16 = 0;
        while (true) {
            const byte = self.reader.reader().readByte() catch unreachable;
            switch (self.vm.opcode_lookup[group_pos + byte].decode()) {
                .op => |op| return try disasmIns(self.vm, &self.reader, ins_start, op),
                .unset => return self.becomePoison(1),
                .nested => |next_start| group_pos = next_start,
            }
        }
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
        .op = .unknown_byte,
        .operands = operands,
    };
}

fn disasmIns(vm: *const Vm, reader: anytype, start: u16, op: Op) !Ins {
    var lang_operands = vm.operands[@intFromEnum(op)];
    var operands: OperandArray = .{};
    for (lang_operands.items().slice()) |lang_op| {
        const operand = try disasmOperand(reader, lang_op);
        operands.appendAssumeCapacity(operand);
    }
    const end: u16 = @intCast(reader.pos);
    return .{
        .start = start,
        .end = end,
        .op = .{ .op = op },
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
