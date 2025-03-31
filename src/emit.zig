const std = @import("std");

const Project = @import("Project.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const block_header_size = @import("block_reader.zig").block_header_size;
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
    project: *const Project,
) !void {
    var index: Index = try .init(gpa);
    defer index.deinit(gpa);

    for (0..games.numberOfDisks(game)) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try emitDisk(gpa, project_dir, output_dir, index_name, game, project, disk_number, &index);
    }

    const project_file = &project.files.items[0].?;
    try emitIndex(gpa, project_dir, output_dir, index_name, &project_file.ast, &index);
}

fn emitDisk(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    project: *const Project,
    disk_number: u8,
    index: *Index,
) !void {
    const project_file = &project.files.items[0].?;
    const root = &project_file.ast.nodes.items[project_file.ast.root].project;
    const disks = project_file.ast.getExtra(root.disks);
    const disk_index = disk_number - 1;
    const disk_node = disks[disk_index];
    if (disk_node == parser.null_node)
        return;
    const disk = &project_file.ast.nodes.items[disk_node].disk;

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

    const child_nodes = project_file.ast.getExtra(disk.children);
    for (child_nodes) |child_node| {
        switch (project_file.ast.nodes.items[child_node]) {
            .disk_room => |*node| try emitRoom(gpa, project_dir, project, disk_number, &out, &fixups, node, index),
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
    project: *const Project,
    disk_number: u8,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    room: *const @FieldType(parser.Node, "disk_room"),
    index: *Index,
) !void {
    const lflf_start = try beginBlock(out, "LFLF");

    try utils.growMultiArrayList(Room, &index.rooms, gpa, room.room_number + 1, .zero);
    index.rooms.set(room.room_number, .{
        .offset = @intCast(out.bytes_written),
        .disk = disk_number,
    });

    const room_file = &project.files.items[room.room_number].?;
    const root = &room_file.ast.nodes.items[room_file.ast.root].room_file;
    for (room_file.ast.getExtra(root.children)) |child_node| {
        switch (room_file.ast.nodes.items[child_node]) {
            .raw_block => |*n| try emitRawBlock(project_dir, out, fixups, n),
            .raw_glob_file => |*n| try emitRawGlobFile(gpa, project_dir, out, fixups, index, room.room_number, n),
            .raw_glob_block => |*n| try emitRawGlobBlock(gpa, project_dir, project, out, fixups, index, room.room_number, n),
            .awiz => |*n| try emitAwiz(gpa, project_dir, project, out, fixups, index, room.room_number, n),
            else => unreachable,
        }
    }

    try endBlock(out, fixups, lflf_start);
}

fn emitAwiz(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    project: *const Project,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    index: *Index,
    room_number: u8,
    awiz_node: *const @FieldType(parser.Node, "awiz"),
) !void {
    var the_awiz: awiz.Awiz = .{};
    defer the_awiz.deinit(gpa);

    const room_file = &project.files.items[room_number].?;
    for (room_file.ast.getExtra(awiz_node.children)) |node| {
        const child_node = &room_file.ast.nodes.items[node];
        switch (child_node.*) {
            .awiz_rgbs => {
                the_awiz.blocks.append(.rgbs) catch unreachable;
            },
            .awiz_two_ints => |ti| {
                the_awiz.blocks.append(.{ .two_ints = .{
                    .id = ti.block_id,
                    .ints = ti.ints,
                } }) catch unreachable;
            },
            .awiz_wizh => {
                the_awiz.blocks.append(.wizh) catch unreachable;
            },
            .awiz_bmp => |wizd| {
                const awiz_raw = try fs.readFile(gpa, project_dir, wizd.path);
                const awiz_raw_arraylist: std.ArrayListUnmanaged(u8) = .fromOwnedSlice(awiz_raw);
                the_awiz.blocks.append(.{ .wizd = .{
                    .compression = wizd.compression,
                    .bmp = awiz_raw_arraylist,
                } }) catch unreachable;
            },
            else => unreachable,
        }
    }

    const start = try beginBlock(out, "AWIZ");
    try awiz.encode(&the_awiz, out, fixups);
    try endBlock(out, fixups, start);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(gpa, index, room_number, blockId("AWIZ"), awiz_node.glob_number, start, size);
}

fn emitRawBlock(
    project_dir: std.fs.Dir,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    raw_block: *const @FieldType(parser.Node, "raw_block"),
) !void {
    try emitFileAsBlock(project_dir, out, fixups, raw_block.block_id, raw_block.path);
}

fn emitRawGlobFile(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    index: *Index,
    room_number: u8,
    raw_glob: *const @FieldType(parser.Node, "raw_glob_file"),
) !void {
    const start: u32 = @intCast(out.bytes_written);
    try emitFileAsBlock(project_dir, out, fixups, raw_glob.block_id, raw_glob.path);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(gpa, index, room_number, raw_glob.block_id, raw_glob.glob_number, start, size);
}

fn emitRawGlobBlock(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    project: *const Project,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    index: *Index,
    room_number: u8,
    raw_glob: *const @FieldType(parser.Node, "raw_glob_block"),
) !void {
    const start = try beginBlockImpl(out, raw_glob.block_id);

    const room_file = &project.files.items[room_number].?;
    for (room_file.ast.getExtra(raw_glob.children)) |node| {
        const raw_block = &room_file.ast.nodes.items[node].raw_block;
        try emitRawBlock(project_dir, out, fixups, raw_block);
    }

    try endBlock(out, fixups, start);
    const end: u32 = @intCast(out.bytes_written);
    const size = end - start;

    try addGlobToIndex(gpa, index, room_number, raw_glob.block_id, raw_glob.glob_number, start, size);
}

fn addGlobToIndex(
    gpa: std.mem.Allocator,
    index: *Index,
    room_number: u8,
    block_id: BlockId,
    glob_number: u16,
    offset_in_disk: u32,
    size: u32,
) !void {
    const directory = switch (block_id) {
        // XXX: this list is duplicated in extract
        blockId("RMIM") => &index.directories.room_images,
        blockId("RMDA") => &index.directories.rooms,
        blockId("SCRP") => &index.directories.scripts,
        blockId("DIGI"), blockId("TALK") => &index.directories.sounds,
        blockId("AKOS") => &index.directories.costumes,
        blockId("CHAR") => &index.directories.charsets,
        blockId("AWIZ"), blockId("MULT") => &index.directories.images,
        blockId("TLKE") => &index.directories.talkies,
        else => unreachable,
    };
    try utils.growMultiArrayList(DirectoryEntry, directory, gpa, glob_number + 1, .zero);
    if (directory.items(.room)[glob_number] != 0)
        @panic("TODO");
    const offset_in_room = offset_in_disk - index.rooms.items(.offset)[room_number];
    directory.set(glob_number, .{
        .room = room_number,
        .offset = offset_in_room,
        .size = size,
    });
}

fn emitFileAsBlock(
    project_dir: std.fs.Dir,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    block_id: BlockId,
    path: []const u8,
) !void {
    const start = try beginBlockImpl(out, block_id);

    const in_file = try project_dir.openFile(path, .{});
    defer in_file.close();

    try io.copy(in_file, out.writer());

    try endBlock(out, fixups, start);
}

const Index = struct {
    rooms: std.MultiArrayList(Room),
    directories: Directories,

    fn init(gpa: std.mem.Allocator) !Index {
        var index: Index = .{
            .rooms = .empty,
            .directories = .{},
        };

        // Globs start at 1, so 0 doesn't exist, so SCUMM sets the sizes in the
        // 0 entries to 0xffff_ffff.
        inline for (comptime std.meta.fieldNames(Directories)) |field| {
            // (except for DIRR, for some reason)
            comptime if (std.mem.eql(u8, field, "rooms")) continue;
            try @field(index.directories, field).append(gpa, .{
                .room = 0,
                .offset = 0,
                .size = 0xffff_ffff,
            });
        }

        return index;
    }

    fn deinit(self: *Index, gpa: std.mem.Allocator) void {
        self.directories.deinit(gpa);
        self.rooms.deinit(gpa);
    }
};

const Room = struct {
    offset: u32,
    disk: u8,

    pub const zero: Room = .{ .offset = 0, .disk = 0 };
};

const Directories = struct {
    room_images: std.MultiArrayList(DirectoryEntry) = .{},
    rooms: std.MultiArrayList(DirectoryEntry) = .{},
    scripts: std.MultiArrayList(DirectoryEntry) = .{},
    sounds: std.MultiArrayList(DirectoryEntry) = .{},
    costumes: std.MultiArrayList(DirectoryEntry) = .{},
    charsets: std.MultiArrayList(DirectoryEntry) = .{},
    images: std.MultiArrayList(DirectoryEntry) = .{},
    talkies: std.MultiArrayList(DirectoryEntry) = .{},

    fn deinit(self: *Directories, gpa: std.mem.Allocator) void {
        self.talkies.deinit(gpa);
        self.images.deinit(gpa);
        self.charsets.deinit(gpa);
        self.costumes.deinit(gpa);
        self.sounds.deinit(gpa);
        self.scripts.deinit(gpa);
        self.rooms.deinit(gpa);
        self.room_images.deinit(gpa);
    }
};

const DirectoryEntry = struct {
    room: u8,
    offset: u32,
    size: u32,

    pub const zero: DirectoryEntry = .{
        .room = 0,
        .offset = 0,
        .size = 0,
    };
};

fn emitIndex(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    output_dir: std.fs.Dir,
    index_name: [:0]const u8,
    ast: *const parser.Ast,
    index: *Index,
) !void {
    const out_file = try output_dir.createFileZ(index_name, .{});
    defer out_file.close();
    const out_xor = io.xorWriter(out_file.writer(), xor_key);
    var out_buf = std.io.bufferedWriter(out_xor.writer());
    var out = std.io.countingWriter(out_buf.writer());

    var fixups: std.ArrayList(Fixup) = .init(gpa);
    defer fixups.deinit();

    // SCUMM outputs sequential room numbers for these whether or not the room
    // actually exists.
    for (index.directories.room_images.items(.room), 0..) |*room, i|
        room.* = @intCast(i);
    for (index.directories.rooms.items(.room), 0..) |*room, i|
        room.* = @intCast(i);

    const project = &ast.nodes.items[ast.root].project;

    for (ast.getExtra(project.index)) |node| switch (ast.nodes.items[node]) {
        .index_block => |id| switch (id) {
            .DIRI => try writeDirectory(&out, &fixups, blockId("DIRI"), &index.directories.room_images),
            .DIRR => try writeDirectory(&out, &fixups, blockId("DIRR"), &index.directories.rooms),
            .DIRS => try writeDirectory(&out, &fixups, blockId("DIRS"), &index.directories.scripts),
            .DIRN => try writeDirectory(&out, &fixups, blockId("DIRN"), &index.directories.sounds),
            .DIRC => try writeDirectory(&out, &fixups, blockId("DIRC"), &index.directories.costumes),
            .DIRF => try writeDirectory(&out, &fixups, blockId("DIRF"), &index.directories.charsets),
            .DIRM => try writeDirectory(&out, &fixups, blockId("DIRM"), &index.directories.images),
            .DIRT => try writeDirectory(&out, &fixups, blockId("DIRT"), &index.directories.talkies),
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
                    const room = &ast.nodes.items[room_node].disk_room;
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

fn writeDirectory(
    out: anytype,
    fixups: *std.ArrayList(Fixup),
    block_id: BlockId,
    directory: *const std.MultiArrayList(DirectoryEntry),
) !void {
    const start = try beginBlockImpl(out, block_id);
    try out.writer().writeInt(u16, @intCast(directory.len), .little);
    const slice = directory.slice();
    try out.writer().writeAll(slice.items(.room));
    try out.writer().writeAll(std.mem.sliceAsBytes(slice.items(.offset)));
    try out.writer().writeAll(std.mem.sliceAsBytes(slice.items(.size)));
    try endBlock(out, fixups, start);
}

// TODO: move this to analysis phase
fn findRoomByNumber(ast: *const parser.Ast, room_number: u8) ?parser.NodeIndex {
    const project = &ast.nodes.items[ast.root].project;
    for (ast.getExtra(project.disks)) |disk_node| {
        const disk = &ast.nodes.items[disk_node].disk;
        for (ast.getExtra(disk.children)) |child_node| {
            const child = &ast.nodes.items[child_node];
            if (child.* == .disk_room and child.disk_room.room_number == room_number)
                return child_node;
        }
    }
    return null;
}
