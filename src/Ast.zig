const std = @import("std");

const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const Precedence = @import("parser.zig").Precedence;

const Ast = @This();

root: NodeIndex,
nodes: std.ArrayListUnmanaged(Node),
node_tokens: std.ArrayListUnmanaged(u32),
extra: std.ArrayListUnmanaged(u32),

pub fn deinit(self: *Ast, gpa: std.mem.Allocator) void {
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
        index: ExtraSlice,
        disks: ExtraSlice,
        variables: ExtraSlice,
    },
    index_block: enum { DIRI, DIRR, DIRS, DIRN, DIRC, DIRF, DIRM, DIRT, DLFL, DISK, RNAM },
    disk: struct {
        children: ExtraSlice,
    },
    disk_room: struct {
        room_number: u8,
        name: []const u8,
        path: []const u8,
    },
    room_file: struct {
        children: ExtraSlice,
        variables: ExtraSlice,
    },
    raw_block: struct {
        block_id: BlockId,
        path: []const u8,
    },
    raw_block_nested: struct {
        block_id: BlockId,
        children: ExtraSlice,
    },
    raw_glob_file: struct {
        block_id: BlockId,
        name: ?[]const u8,
        glob_number: u16,
        path: []const u8,
    },
    raw_glob_block: struct {
        block_id: BlockId,
        name: ?[]const u8,
        glob_number: u16,
        children: ExtraSlice,
    },
    rmim: struct {
        compression: u8,
        path: []const u8,
    },
    rmda: struct {
        children: ExtraSlice,
    },
    scr: struct {
        name: []const u8,
        glob_number: u16,
        path: []const u8,
    },
    encd: struct {
        path: []const u8,
    },
    excd: struct {
        path: []const u8,
    },
    lsc: struct {
        name: []const u8,
        script_number: u16,
        path: []const u8,
    },
    obim: struct {
        children: ExtraSlice,
    },
    obim_im: struct {
        children: ExtraSlice,
    },
    awiz: struct {
        glob_number: u16,
        children: ExtraSlice,
    },
    awiz_rgbs,
    awiz_two_ints: struct {
        block_id: BlockId,
        ints: [2]i32,
    },
    awiz_wizh,
    awiz_bmp: struct {
        compression: awiz.Compression,
        path: []const u8,
    },
    mult: struct {
        glob_number: u16,
        raw_block: NodeIndex,
        children: ExtraSlice,
        indices: ExtraSlice,
    },
    mult_awiz: struct {
        children: ExtraSlice,
    },
    akos: struct {
        glob_number: u16,
        children: ExtraSlice,
    },
    akpl: struct {
        path: []const u8,
    },
    akcd: struct {
        compression: akos.CompressionCodec,
        path: []const u8,
    },
    variable: struct {
        name: []const u8,
        number: u16,
    },
    script: struct {
        name: []const u8,
        glob_number: u16,
        statements: ExtraSlice,
    },
    local_script: struct {
        name: []const u8,
        script_number: u16,
        statements: ExtraSlice,
    },
    enter: struct {
        statements: ExtraSlice,
    },
    exit: struct {
        statements: ExtraSlice,
    },
    local_vars: struct {
        children: ExtraSlice,
    },
    local_var: struct {
        name: ?[]const u8,
    },
    label: []const u8,
    integer: i32,
    string: []const u8,
    identifier: []const u8,
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
        field: []const u8,
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

pub const CaseCondition = union(enum) {
    eq: NodeIndex,
    in: ExtraSlice,
    default,
};

pub const ForDirection = enum {
    up,
    down,
};

pub const ExtraSlice = struct {
    start: u32,
    len: u32,

    pub const empty: ExtraSlice = .{ .start = 0, .len = 0 };
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
};
