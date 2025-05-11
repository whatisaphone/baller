const std = @import("std");

const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;

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
        glob_number: u16,
        path: []const u8,
    },
    raw_glob_block: struct {
        block_id: BlockId,
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
    scrp: struct {
        glob_number: u16,
        path: []const u8,
    },
    encd: struct {
        path: []const u8,
    },
    excd: struct {
        path: []const u8,
    },
    lscr: struct {
        script_number: u16,
        path: []const u8,
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
        glob_number: u16,
        statements: ExtraSlice,
    },
    local_script: struct {
        script_number: u16,
        statements: ExtraSlice,
    },
    enter: struct {
        statements: ExtraSlice,
    },
    exit: struct {
        statements: ExtraSlice,
    },
    label: []const u8,
    integer: i32,
    string: []const u8,
    identifier: []const u8,
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
};

pub const ExtraSlice = struct {
    start: u32,
    len: u32,

    pub const empty: ExtraSlice = .{ .start = 0, .len = 0 };
};
