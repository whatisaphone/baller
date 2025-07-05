const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const awiz = @import("awiz.zig");
const cliargs = @import("cliargs.zig");
const emit = @import("emit.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const plan = @import("plan.zig");
const sync = @import("sync.zig");
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

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();

    run(gpa, &diagnostic, .{
        .project_path = project_path,
        .index_path = index_path,
        .options = .{
            .awiz_strategy = .max,
        },
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
    };
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

const Build = struct {
    project_path: [:0]const u8,
    index_path: [:0]const u8,
    options: Options,
};

const Options = struct {
    awiz_strategy: awiz.EncodingStrategy,
};

pub fn run(gpa: std.mem.Allocator, diagnostic: *Diagnostic, args: Build) !void {
    const project_path_opt, const project_name = fs.splitPathZ(args.project_path);
    var project_dir = if (project_path_opt) |project_path|
        try std.fs.cwd().openDir(project_path, .{})
    else
        std.fs.cwd();
    defer if (project_path_opt) |_|
        project_dir.close();

    const output_path_opt, const index_name = fs.splitPathZ(args.index_path);
    var output_dir = if (output_path_opt) |output_path| output_dir: {
        // Make sure tests always write to an empty dir
        if (builtin.is_test)
            fs.assertNotExists(std.fs.cwd(), output_path);

        try fs.makeDirIfNotExist(std.fs.cwd(), output_path);
        break :output_dir try std.fs.cwd().openDir(output_path, .{});
    } else std.fs.cwd();
    defer if (output_path_opt) |_|
        output_dir.close();

    var project: Project = .empty;
    defer project.deinit(gpa);

    diagnostic.trace("parsing project", .{});

    const root = try addFile(gpa, diagnostic, project_dir, project_name, parser.parseProject);
    try project.files.append(gpa, root);

    diagnostic.trace("parsing rooms", .{});

    try readRooms(gpa, diagnostic, &project, project_dir);

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = gpa });
    defer pool.deinit();

    var events: sync.Channel(plan.Event, 16) = .init;

    try pool.spawn(plan.run, .{ gpa, diagnostic, project_dir, &project, args.options.awiz_strategy, output_dir, index_name, &pool, &events });

    try emit.run(gpa, diagnostic, output_dir, index_name, &events);
}

fn addFile(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    project_dir: std.fs.Dir,
    path: []const u8,
    parseFn: anytype,
) !Project.SourceFile {
    const diag: Diagnostic.ForTextFile = .{
        .diagnostic = diagnostic,
        .path = path,
    };

    const source = try fs.readFile(gpa, project_dir, path);
    defer gpa.free(source);

    var lex = try lexer.run(gpa, &diag, source);
    errdefer lex.deinit(gpa);

    var ast = try parseFn(gpa, &diag, source, &lex);
    errdefer ast.deinit(gpa);

    return .{
        .path = path,
        .lex = lex,
        .ast = ast,
    };
}

fn readRooms(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    project: *Project,
    project_dir: std.fs.Dir,
) !void {
    var room_nodes: std.BoundedArray(Ast.NodeIndex, 255) = .{};
    var max_room_number: u8 = 0;

    var project_file = &project.files.items[0].?;
    const root = &project_file.ast.nodes.at(project_file.ast.root).project;
    for (project_file.ast.getExtra(root.children)) |child_node| {
        const child = project_file.ast.nodes.at(child_node);
        if (child.* != .disk) continue;
        for (project_file.ast.getExtra(child.disk.children)) |disk_child_node| {
            const disk_child = project_file.ast.nodes.at(disk_child_node);
            if (disk_child.* != .disk_room) continue;
            try room_nodes.append(disk_child_node);
            max_room_number = @max(max_room_number, disk_child.disk_room.room_number);
        }
    }

    try utils.growArrayList(?Project.SourceFile, &project.files, gpa, max_room_number + 1, null);
    project_file = &project.files.items[0].?; // since pointer was invalidated

    for (room_nodes.slice()) |room_node| {
        const room = &project_file.ast.nodes.at(room_node).disk_room;
        if (project.files.items[room.room_number] != null)
            @panic("TODO");

        const room_path = project_file.ast.strings.get(room.path);
        const file = try addFile(gpa, diagnostic, project_dir, room_path, parser.parseRoom);
        project.files.items[room.room_number] = file;
    }
}
