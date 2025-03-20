const std = @import("std");

const cliargs = @import("cliargs.zig");
const emit = @import("emit.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

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
    var output_dir = if (output_path_opt) |output_path|
        try std.fs.cwd().openDir(output_path, .{})
    else
        std.fs.cwd();
    defer if (output_path_opt) |_|
        output_dir.close();

    const game: games.Game = .baseball_2001;

    const source = try fs.readFileZ(gpa, project_dir, project_name);
    defer gpa.free(source);

    var lex = try lexer.run(gpa, source);
    defer lex.deinit(gpa);

    var ast = try parser.run(gpa, source, &lex);
    defer ast.deinit(gpa);

    try emit.run(gpa, project_dir, output_dir, index_name, game, &ast);
}
