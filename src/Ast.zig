const std = @import("std");

const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const games = @import("games.zig");
const lang = @import("lang.zig");
const lexer = @import("lexer.zig");
const Precedence = @import("parser.zig").Precedence;

const Ast = @This();

root: NodeIndex,
nodes: std.ArrayListUnmanaged(Node),
node_tokens: std.ArrayListUnmanaged(lexer.TokenIndex),
extra: std.ArrayListUnmanaged(u32),
strings: StringTable,

pub fn deinit(self: *Ast, gpa: std.mem.Allocator) void {
    self.strings.deinit(gpa);
    self.extra.deinit(gpa);
    self.node_tokens.deinit(gpa);
    self.nodes.deinit(gpa);
}

pub fn getExtra(self: *const Ast, slice: ExtraSlice) []const u32 {
    return self.extra.items[slice.start..][0..slice.len];
}

pub const max_room_name_len = 255;
pub const max_mult_children = 256;
pub const max_case_branches = 320;

pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);

pub const Node = union(enum) {
    project: struct {
        children: ExtraSlice,
    },
    target: games.Target,
    index: struct {
        children: ExtraSlice,
    },
    maxs: struct {
        path: StringSlice,
    },
    index_block: enum { DIRI, DIRR, DIRS, DIRN, DIRC, DIRF, DIRM, DIRT, DLFL, DISK, RNAM },
    disk: struct {
        number: u8,
        children: ExtraSlice,
    },
    disk_room: struct {
        room_number: u8,
        name: StringSlice,
        path: StringSlice,
    },
    room_file: struct {
        children: ExtraSlice,
        variables: ExtraSlice,
    },
    raw_block: struct {
        block_id: BlockId,
        contents: RawContents,
    },
    raw_block_nested: struct {
        block_id: BlockId,
        children: ExtraSlice,
    },
    raw_glob_file: struct {
        block_id: BlockId,
        name: ?StringSlice,
        glob_number: u16,
        path: StringSlice,
    },
    raw_glob_block: struct {
        block_id: BlockId,
        name: ?StringSlice,
        glob_number: u16,
        children: ExtraSlice,
    },
    rmim: struct {
        rmih: NodeIndex,
        im: NodeIndex,
    },
    rmim_im: struct {
        children: ExtraSlice,
    },
    bmap: struct {
        compression: u8,
        path: StringSlice,
    },
    rmda: struct {
        children: ExtraSlice,
    },
    scr: struct {
        name: StringSlice,
        glob_number: u16,
        path: StringSlice,
    },
    encd: struct {
        path: StringSlice,
    },
    excd: struct {
        path: StringSlice,
    },
    lsc: struct {
        name: StringSlice,
        script_number: u16,
        path: StringSlice,
    },
    obim: struct {
        children: ExtraSlice,
    },
    obim_im: struct {
        children: ExtraSlice,
    },
    sound: struct {
        block_id: BlockId,
        name: StringSlice,
        glob_number: u16,
        children: ExtraSlice,
    },
    sdat: struct {
        path: StringSlice,
    },
    awiz: struct {
        name: StringSlice,
        glob_number: u16,
        children: ExtraSlice,
    },
    awiz_rgbs,
    awiz_two_ints: struct {
        block_id: BlockId,
        ints: [2]i32,
    },
    awiz_wizh,
    awiz_wizd: struct {
        compression: awiz.Compression,
        path: StringSlice,
    },
    mult: struct {
        name: StringSlice,
        glob_number: u16,
        raw_block: NodeIndex,
        children: ExtraSlice,
        indices: ExtraSlice,
    },
    mult_awiz: struct {
        children: ExtraSlice,
    },
    akos: struct {
        name: StringSlice,
        glob_number: u16,
        children: ExtraSlice,
    },
    akpl: struct {
        path: StringSlice,
    },
    akcd: struct {
        compression: akos.CompressionCodec,
        path: StringSlice,
    },
    talkie: struct {
        name: StringSlice,
        glob_number: u16,
        text: StringSlice,
    },
    music: struct {
        children: ExtraSlice,
    },
    constant: struct {
        name: StringSlice,
        value: i32,
    },
    variable: struct {
        name: StringSlice,
        number: u16,
    },
    script: struct {
        name: StringSlice,
        glob_number: u16,
        statements: ExtraSlice,
    },
    local_script: struct {
        name: StringSlice,
        script_number: u16,
        statements: ExtraSlice,
    },
    enter: struct {
        statements: ExtraSlice,
    },
    exit: struct {
        statements: ExtraSlice,
    },
    object: struct {
        name: StringSlice,
        number: u16,
        obna: StringSlice,
        children: ExtraSlice,
    },
    verb: struct {
        number: u8,
        body: VerbBody,
    },
    local_vars: struct {
        children: ExtraSlice,
    },
    local_var: struct {
        name: ?StringSlice,
    },
    label: StringSlice,
    integer: i32,
    string: StringSlice,
    identifier: StringSlice,
    set: struct {
        lhs: NodeIndex,
        rhs: NodeIndex,
    },
    array_get: struct {
        lhs: NodeIndex,
        index: NodeIndex,
    },
    array_get2: struct {
        lhs: NodeIndex,
        index1: NodeIndex,
        index2: NodeIndex,
    },
    binop: struct {
        op: BinOp,
        lhs: NodeIndex,
        rhs: NodeIndex,
    },
    binop_assign: struct {
        op: BinOp,
        lhs: NodeIndex,
        rhs: NodeIndex,
    },
    call: struct {
        callee: NodeIndex,
        args: ExtraSlice,
    },
    field: struct {
        lhs: NodeIndex,
        field: StringSlice,
    },
    list: struct {
        items: ExtraSlice,
    },
    @"if": struct {
        condition: NodeIndex,
        true: ExtraSlice,
        false: ExtraSlice,
    },
    @"while": struct {
        condition: NodeIndex,
        body: ExtraSlice,
    },
    do: struct {
        body: ExtraSlice,
        condition: NodeIndex,
    },
    @"for": struct {
        accumulator: NodeIndex,
        start: NodeIndex,
        end: NodeIndex,
        direction: ForDirection,
        body: ExtraSlice,
    },
    for_in: struct {
        target: NodeIndex,
        list: NodeIndex,
        backing: NodeIndex,
        body: ExtraSlice,
    },
    case: struct {
        value: NodeIndex,
        branches: ExtraSlice,
    },
    case_branch: struct {
        condition: CaseCondition,
        body: ExtraSlice,
    },
};

pub const RawContents = union(enum) {
    path: StringSlice,
    data: StringSlice,
};

pub const CaseCondition = union(enum) {
    eq: NodeIndex,
    in: ExtraSlice,
    default,
};

pub const ForDirection = enum {
    up,
    down,
};

pub const VerbBody = union(enum) {
    assembly: StringSlice,
    script: ExtraSlice,
};

pub const ExtraSlice = struct {
    start: u32,
    len: u32,

    pub const empty: ExtraSlice = .{ .start = 0, .len = 0 };
};

const StringTable = struct {
    buf: std.ArrayListUnmanaged(u8),

    pub const empty: StringTable = .{ .buf = .empty };

    fn deinit(self: *StringTable, gpa: std.mem.Allocator) void {
        self.buf.deinit(gpa);
    }

    pub fn get(self: *const StringTable, slice: StringSlice) []const u8 {
        return self.buf.items[slice.start..][0..slice.len];
    }

    pub fn add(self: *StringTable, gpa: std.mem.Allocator, str: []const u8) !StringSlice {
        const start: u32 = @intCast(self.buf.items.len);
        const len: u32 = @intCast(str.len);
        try self.buf.appendSlice(gpa, str);
        return .{ .start = start, .len = len };
    }
};

pub const StringSlice = struct {
    start: u32,
    len: u32,
};

pub const BinOp = enum {
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
    add,
    sub,
    mul,
    div,
    mod,
    shl,
    shr,
    land,
    lor,

    pub fn str(self: BinOp) []const u8 {
        return switch (self) {
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .shl => "<<",
            .shr => ">>",
            .land => "&&",
            .lor => "||",
        };
    }

    pub fn precedence(self: BinOp) Precedence {
        return switch (self) {
            .eq => .equality,
            .ne => .equality,
            .lt => .inequality,
            .le => .inequality,
            .gt => .inequality,
            .ge => .inequality,
            .add => .add,
            .sub => .add,
            .mul => .mul,
            .div => .mul,
            .mod => .mul,
            .shl => .bit,
            .shr => .bit,
            .land => .logical,
            .lor => .logical,
        };
    }

    pub fn hasEqAssign(self: BinOp) bool {
        return switch (self) {
            .eq, .ne, .lt, .le, .gt, .ge => false,
            .add, .sub, .mul, .div, .mod, .shl, .shr => true,
            .land, .lor => false,
        };
    }

    pub fn op(self: BinOp) lang.Op {
        return switch (self) {
            .eq => .eq,
            .ne => .ne,
            .lt => .lt,
            .le => .le,
            .gt => .gt,
            .ge => .ge,
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .mod => .mod,
            .shl => .shl,
            .shr => .shr,
            .land => .land,
            .lor => .lor,
        };
    }
};
