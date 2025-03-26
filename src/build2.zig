const std = @import("std");

const Project = @import("Project.zig");
const cliargs = @import("cliargs.zig");
const emit = @import("emit.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var project_path_opt: ?[:0]const u8 = null;
    var index_path_opt: ?[:0]const u8 = null;

    var it: cliargs.Iterator = .init(args);
    while (it.next()) |arg| switch (arg) {
        .positional => |str| {
            if (project_path_opt == null)
                project_path_opt = str
            else if (index_path_opt == null)
                index_path_opt = str
            else
                return arg.reportUnexpected();
        },
        else => return arg.reportUnexpected(),
    };

    const project_path = project_path_opt orelse return cliargs.reportMissing("project");
    const index_path = index_path_opt orelse return cliargs.reportMissing("index");

    try run(gpa, .{
        .project_path = project_path,
        .index_path = index_path,
    });
}

const Build = struct {
    project_path: [:0]const u8,
    index_path: [:0]const u8,
};

pub fn run(gpa: std.mem.Allocator, args: Build) !void {
    const project_path_opt, const project_name = fs.splitPathZ(args.project_path);
    var project_dir = if (project_path_opt) |project_path|
        try std.fs.cwd().openDir(project_path, .{})
    else
        std.fs.cwd();
    defer if (project_path_opt) |_|
        project_dir.close();

    const output_path_opt, const index_name = fs.splitPathZ(args.index_path);
    var output_dir = if (output_path_opt) |output_path| output_dir: {
        try fs.makeDirIfNotExist(std.fs.cwd(), output_path);
        break :output_dir try std.fs.cwd().openDir(output_path, .{});
    } else std.fs.cwd();
    defer if (output_path_opt) |_|
        output_dir.close();

    const game: games.Game = .baseball_2001;

    var project: Project = .empty;
    defer project.deinit(gpa);

    const root = try addFile(gpa, project_dir, project_name, parser.parseProject);
    try project.files.append(gpa, root);

    try readRooms(gpa, &project, project_dir);

    try emit.run(gpa, project_dir, output_dir, index_name, game, &project);
}

fn addFile(
    gpa: std.mem.Allocator,
    project_dir: std.fs.Dir,
    path: []const u8,
    parseFn: anytype,
) !Project.SourceFile {
    const source = try fs.readFile(gpa, project_dir, path);
    errdefer gpa.free(source);

    var lex = try lexer.run(gpa, source);
    errdefer lex.deinit(gpa);

    var ast = try parseFn(gpa, source, &lex);
    errdefer ast.deinit(gpa);

    return .{
        .source = source,
        .lex = lex,
        .ast = ast,
    };
}

fn readRooms(gpa: std.mem.Allocator, project: *Project, project_dir: std.fs.Dir) !void {
    var room_nodes: std.BoundedArray(parser.NodeIndex, 255) = .{};

    const project_file = &project.files.items[0].?;
    const root = &project_file.ast.nodes.items[project_file.ast.root].project;
    for (project_file.ast.getExtra(root.disks)) |disk_node| {
        const disk = &project_file.ast.nodes.items[disk_node].disk;
        for (project_file.ast.getExtra(disk.children)) |disk_child_node| {
            const disk_child = &project_file.ast.nodes.items[disk_child_node];
            if (disk_child.* == .disk_room)
                try room_nodes.append(disk_child_node);
        }
    }

    for (room_nodes.slice()) |room_node| {
        const room = &project.files.items[0].?.ast.nodes.items[room_node].disk_room;
        try utils.growArrayList(?Project.SourceFile, &project.files, gpa, room.room_number + 1, null);
        if (project.files.items[room.room_number] != null)
            @panic("TODO");

        const file = try addFile(gpa, project_dir, room.path, parser.parseRoom);
        project.files.items[room.room_number] = file;
    }
}
