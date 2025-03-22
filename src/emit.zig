const std = @import("std");

const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const writeFixups = @import("block_writer.zig").writeFixups;
const xor_key = @import("build.zig").xor_key;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const parser = @import("parser.zig");
const pathf = @import("pathf.zig");
const utils = @import("utils.zig");

pub fn run(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    ast: *const parser.Ast,
) !void {
    var index: Index = .empty;
    defer index.deinit(gpa);

    for (0..games.numberOfDisks(game)) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try emitDisk(gpa, project_dir, output_dir, index_name, game, ast, disk_number, &index);
    }

    try emitIndex(gpa, project_dir, output_dir, index_name, ast, &index);
}

fn emitDisk(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    ast: *const parser.Ast,
    disk_number: u8,
    index: *Index,
) !void {
    const project = &ast.nodes.items[ast.root].project;
    const disks = ast.getExtra(project.disks);
    const disk_index = disk_number - 1;
    const disk_node = disks[disk_index];
    if (disk_node == parser.null_node)
        return;
    const disk = &ast.nodes.items[disk_node].disk;

    var out_name_buf: pathf.Path = .{};
    const out_name = try pathf.append(&out_name_buf, index_name);
    games.pointPathToDisk(game, out_name.full(), disk_number);

    const out_file = try output_dir.createFileZ(out_name.full(), .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    const lecf_start = try beginBlock(&out, "LECF");

    const child_nodes = ast.getExtra(disk.children);
    for (child_nodes) |child_node| {
        switch (ast.nodes.items[child_node]) {
            .room => |*node| try emitRoom(gpa, project_dir, ast, disk_number, &out, &fixups, node, index),
            .raw_block => |*node| try emitRawBlock(project_dir, &out, &fixups, node),
            else => unreachable,
        }
    }

    try endBlock(&out, &fixups, lecf_start);

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

fn emitRoom(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    ast: *const parser.Ast,
    disk_number: u8,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    room: *const @FieldType(parser.Node, "room"),
    index: *Index,
) !void {
    const lflf_start = try beginBlock(out, "LFLF");

    try utils.growMultiArrayList(Room, &index.rooms, gpa, room.room_number + 1, .zero);
    index.rooms.set(room.room_number, .{
        .offset = @intCast(out.bytes_written),
        .disk = disk_number,
    });

    for (ast.getExtra(room.children)) |child_node| {
        const node = &ast.nodes.items[child_node].raw_block;
        try emitRawBlock(project_dir, out, fixups, node);
    }

    try endBlock(out, fixups, lflf_start);
}

fn emitRawBlock(
    project_dir: std.fs.Dir,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    raw_block: *const @FieldType(parser.Node, "raw_block"),
) !void {
    const start = try beginBlockImpl(out, raw_block.block_id);

    const in_file = try project_dir.openFile(raw_block.path, .{});
    defer in_file.close();

    try io.copy(in_file, out.writer());

    try endBlock(out, fixups, start);
}

const Index = struct {
    rooms: std.MultiArrayList(Room),

    const empty: Index = .{
        .rooms = .empty,
    };

    fn deinit(self: *Index, gpa: std.mem.Allocator) void {
        self.rooms.deinit(gpa);
    }
};

const Room = struct {
    offset: u32,
    disk: u8,

    pub const zero: Room = .{ .offset = 0, .disk = 0 };
};

fn emitIndex(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    ast: *const parser.Ast,
    index: *const Index,
) !void {
    const out_file = try output_dir.createFileZ(index_name, .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    const project = &ast.nodes.items[ast.root].project;

    for (ast.getExtra(project.index)) |node| switch (ast.nodes.items[node]) {
        .index_block => |id| switch (id) {
            .DLFL => {
                const start = try beginBlock(&out, "DLFL");
                try out.writer().writeInt(u16, @intCast(index.rooms.len), .little);
                try out.writer().writeAll(std.mem.sliceAsBytes(index.rooms.items(.offset)));
                try endBlock(&out, &fixups, start);
            },
            .DISK => {
                const start = try beginBlock(&out, "DISK");
                try out.writer().writeInt(u16, @intCast(index.rooms.len), .little);
                try out.writer().writeAll(index.rooms.items(.disk));
                try endBlock(&out, &fixups, start);
            },
            .RNAM => {
                const start = try beginBlock(&out, "RNAM");
                for (0..index.rooms.len) |room_number_usize| {
                    const room_number: u8 = @intCast(room_number_usize);
                    const room_node = findRoomByNumber(ast, room_number) orelse continue;
                    const room = &ast.nodes.items[room_node].room;
                    try out.writer().writeInt(u16, room.room_number, .little);
                    try out.writer().writeAll(room.name);
                    try out.writer().writeByte(0);
                }
                try out.writer().writeInt(u16, 0, .little);
                try endBlock(&out, &fixups, start);
            },
        },
        .raw_block => |rb| {
            const start = try beginBlockImpl(&out, rb.block_id);
            const in_file = try project_dir.openFile(rb.path, .{});
            defer in_file.close();
            try io.copy(in_file, out.writer());
            try endBlock(&out, &fixups, start);
        },
        else => unreachable,
    };

    try out_buf.flush();

    try writeFixups(out_file, out_xor.writer(), fixups.items);
}

// TODO: move this to analysis phase
fn findRoomByNumber(ast: *const parser.Ast, room_number: u8) ?parser.NodeIndex {
    const project = &ast.nodes.items[ast.root].project;
    for (ast.getExtra(project.disks)) |disk_node| {
        const disk = &ast.nodes.items[disk_node].disk;
        for (ast.getExtra(disk.children)) |child_node| {
            const child = &ast.nodes.items[child_node];
            if (child.* == .room and child.room.room_number == room_number)
                return child_node;
        }
    }
    return null;
}
