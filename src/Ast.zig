const std = @import("std");

const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;

const Ast = @This();

root: NodeIndex,
nodes: std.ArrayListUnmanaged(Node),
extra: std.ArrayListUnmanaged(u32),

pub fn deinit(self: *Ast, gpa: std.mem.Allocator) void {
    self.extra.deinit(gpa);
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
    },
    raw_block: struct {
        block_id: BlockId,
        path: []const u8,
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
    scrp: struct {
        glob_number: u16,
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
        raw_defa_path: ?[]const u8,
        children: ExtraSlice,
        indices: ExtraSlice,
    },
    mult_awiz: struct {
        children: ExtraSlice,
    },
};

pub const ExtraSlice = struct {
    start: u32,
    len: u32,
};
