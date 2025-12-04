const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Blinkenlights = @import("Blinkenlights.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const awiz = @import("awiz.zig");
const cliargs = @import("cliargs.zig");
const emit = @import("emit.zig");
const fs = @import("fs.zig");
const fsd = @import("fsd.zig");
const games = @import("games.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const plan = @import("plan.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var project_path_opt: ?[:0]const u8 = null;
    var index_path_opt: ?[:0]const u8 = null;
    var awiz_strategy: ?awiz.EncodingStrategy = null;
    var write_version: ?YesNo = null;

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
        .long_option => |opt| {
            if (std.mem.eql(u8, opt.flag, "awiz")) {
                if (awiz_strategy != null) return arg.reportDuplicate();
                awiz_strategy = std.meta.stringToEnum(awiz.EncodingStrategy, opt.value) orelse
                    return arg.reportInvalidValue();
            } else if (std.mem.eql(u8, opt.flag, "write-version")) {
                if (write_version != null) return arg.reportDuplicate();
                write_version = std.meta.stringToEnum(YesNo, opt.value) orelse
                    return arg.reportInvalidValue();
            } else {
                return arg.reportUnexpected();
            }
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
            .awiz_strategy = awiz_strategy orelse .max,
            .write_version = (write_version orelse YesNo.yes).toBool(),
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

pub const Options = struct {
    awiz_strategy: awiz.EncodingStrategy,
    write_version: bool,
};

const YesNo = enum {
    no,
    yes,

    fn toBool(self: YesNo) bool {
        return self != .no;
    }
};

pub fn run(gpa: std.mem.Allocator, diagnostic: *Diagnostic, args: Build) !void {
    const project_path_opt, const project_name = fs.splitPathZ(args.project_path);
    var project_dir = if (project_path_opt) |project_path|
        try fsd.openDir(diagnostic, std.fs.cwd(), project_path)
    else
        std.fs.cwd();
    defer if (project_path_opt) |_|
        project_dir.close();

    if (!std.mem.endsWith(u8, args.index_path, ".he0") and
        !std.mem.endsWith(u8, args.index_path, ".HE0"))
    {
        diagnostic.err("index path doesn't end in \".he0\": {s}", .{args.index_path});
        return error.AddedToDiagnostic;
    }

    const output_path_opt, const index_name = fs.splitPathZ(args.index_path);
    var output_dir = if (output_path_opt) |output_path| output_dir: {
        // Make sure tests always write to an empty dir
        if (builtin.is_test)
            fs.assertNotExists(std.fs.cwd(), output_path);

        try fsd.makeDirIfNotExist(diagnostic, std.fs.cwd(), output_path);
        break :output_dir try fsd.openDir(diagnostic, std.fs.cwd(), output_path);
    } else std.fs.cwd();
    defer if (output_path_opt) |_|
        output_dir.close();

    var blinken: Blinkenlights = undefined;
    try blinken.initAndStart();
    defer blinken.stop();
    blinken.setText(.root, std.fs.path.stem(args.index_path));

    var project: Project = .empty;
    defer project.deinit(gpa);
    try project.files.ensureTotalCapacity(gpa, 32);

    diagnostic.trace("parsing project", .{});

    const root = try addFile(gpa, diagnostic, null, project_dir, project_name, parser.parseProject);
    try project.files.append(gpa, root);

    diagnostic.trace("parsing rooms", .{});

    var room_nodes: [256]Ast.NodeIndex.Optional = undefined;
    try readRooms(gpa, diagnostic, &blinken, &project, project_dir, &room_nodes);

    // Create these all up front so they're in the right order. The functions
    // below are responsible for removing them.
    const plan_blink = blinken.addNode(.root);
    blinken.setText(plan_blink, "plan");
    const build_blink = blinken.addNode(.root);
    blinken.setText(build_blink, "build");
    const emit_blink = blinken.addNode(.root);
    blinken.setText(emit_blink, "emit");

    // Minimum of 2 threads. 1 for the planner, and `num_cpus` for the actual
    // work (which must run concurrently with the planner).
    const n_jobs = 1 + @min(std.Thread.getCpuCount() catch 1, sync.max_concurrency);

    var pool: sync.ThreadPool = undefined;
    try pool.init(gpa, n_jobs);
    defer pool.deinit();

    var events: sync.Channel(sync.OrderedEvent(plan.Payload), sync.max_concurrency) = .init;

    try pool.spawn(plan.run, .{
        gpa,
        diagnostic,
        project_dir,
        &project,
        &room_nodes,
        args.options.awiz_strategy,
        output_dir,
        index_name,
        &blinken,
        plan_blink,
        build_blink,
        &pool,
        &events,
    });

    try emit.run(
        gpa,
        diagnostic,
        &blinken,
        emit_blink,
        &project,
        output_dir,
        index_name,
        &args.options,
        &events,
    );
}

const ParseFn = *const fn (
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
) parser.ParseError!Ast;

fn addFile(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: ?Diagnostic.Location,
    project_dir: std.fs.Dir,
    path: []const u8,
    parseFn: ParseFn,
) !Project.SourceFile {
    const diag: Diagnostic.ForTextFile = .init(diagnostic, path);

    const source = try fsd.readFile(gpa, diagnostic, loc, project_dir, path);
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
    blinken: *Blinkenlights,
    project: *Project,
    project_dir: std.fs.Dir,
    room_nodes: *[256]Ast.NodeIndex.Optional,
) !void {
    const number_of_rooms, const max_room_number =
        try collectRoomsByRoomNumber(project, diagnostic, room_nodes);

    const blink = blinken.addNode(.root);
    defer blinken.removeNode(blink);
    blinken.setText(blink, "parse");
    blinken.setMax(blink, number_of_rooms);

    try utils.growArrayList(?Project.SourceFile, &project.files, gpa, max_room_number + 1, null);

    const project_file = &project.files.items[0].?;

    for (room_nodes) |room_node_opt| {
        const room_node = room_node_opt.unwrap() orelse continue;

        const loc: Diagnostic.Location = .node(project_file, room_node);
        const room = &project_file.ast.nodes.at(room_node).disk_room;
        const room_path = project_file.ast.strings.get(room.path);
        const file = try addFile(gpa, diagnostic, loc, project_dir, room_path, parser.parseRoom);
        project.files.items[room.room_number] = file;

        blinken.addProgress(blink, 1);
    }
}

fn collectRoomsByRoomNumber(
    project: *const Project,
    diagnostic: *Diagnostic,
    out: *[256]Ast.NodeIndex.Optional,
) !struct { u8, u8 } {
    @memset(out, .null);
    var number_of_rooms: u8 = 0;
    var max_room_number: u8 = 0;

    var file = &project.files.items[0].?;
    const root = &file.ast.nodes.at(file.ast.root).project;
    for (file.ast.getExtra(root.children)) |file_child_node| {
        const file_child = file.ast.nodes.at(file_child_node);
        if (file_child.* != .disk) continue;
        for (file.ast.getExtra(file_child.disk.children)) |disk_child_node| {
            const disk_child = file.ast.nodes.at(disk_child_node);
            if (disk_child.* != .disk_room) continue;

            if (out[disk_child.disk_room.room_number] != .null) {
                diagnostic.errAt(.node(file, disk_child_node), "duplicate room number", .{});
                return error.AddedToDiagnostic;
            }
            out[disk_child.disk_room.room_number] = disk_child_node.wrap();

            number_of_rooms += 1;
            max_room_number = @max(max_room_number, disk_child.disk_room.room_number);
        }
    }

    return .{ number_of_rooms, max_room_number };
}
