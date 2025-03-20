const std = @import("std");

const blockReader = @import("block_reader.zig").blockReader;
const xor_key = @import("build.zig").xor_key;
const cliargs = @import("cliargs.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var index_path_opt: ?[:0]const u8 = null;
    var output_path_opt: ?[:0]const u8 = null;

    var it: cliargs.Iterator = .init(args);
    while (it.next()) |arg| switch (arg) {
        .positional => |str| {
            if (index_path_opt == null)
                index_path_opt = str
            else if (output_path_opt == null)
                output_path_opt = str
            else
                return arg.reportUnexpected();
        },
        else => return arg.reportUnexpected(),
    };

    const index_path = index_path_opt orelse return cliargs.reportMissing("index");
    const output_path = output_path_opt orelse return cliargs.reportMissing("output");

    try run(gpa, .{
        .index_path = index_path,
        .output_path = output_path,
    });
}

const Extract = struct {
    index_path: [:0]const u8,
    output_path: [:0]const u8,
};

pub fn run(gpa: std.mem.Allocator, args: Extract) !void {
    const input_path_opt, const index_name = fs.splitPathZ(args.index_path);
    var input_dir = if (input_path_opt) |input_path|
        try std.fs.cwd().openDir(input_path, .{})
    else
        std.fs.cwd();
    defer if (input_path_opt) |_|
        input_dir.close();

    try fs.makeDirIfNotExistZ(std.fs.cwd(), args.output_path);
    var output_dir = try std.fs.cwd().openDirZ(args.output_path, .{});
    defer output_dir.close();

    const game: games.Game = .baseball_2001;

    var code: std.ArrayListUnmanaged(u8) = .empty;
    defer code.deinit(gpa);

    try extractIndex(gpa, input_dir, index_name, output_dir, &code);

    for (0..games.numberOfDisks(game)) |disk_index| {
        const disk_number: u8 = @intCast(disk_index + 1);
        try extractDisk(gpa, input_dir, index_name, game, disk_number, output_dir, &code);
    }

    try fs.writeFileZ(output_dir, "project.scu", code.items);
}

fn extractIndex(
    gpa: std.mem.Allocator,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    const in_file = try input_dir.openFileZ(index_name, .{});
    defer in_file.close();
    const in = io.xorReader(in_file.reader(), xor_key);

    const out_file = try output_dir.createFileZ("index.bin", .{});
    defer out_file.close();

    try io.copy(in.reader(), out_file.writer());

    try code.appendSlice(gpa, "index \"index.bin\"\n");
}

fn extractDisk(
    gpa: std.mem.Allocator,
    input_dir: std.fs.Dir,
    index_name: [:0]const u8,
    game: games.Game,
    disk_number: u8,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    try code.writer(gpa).print("disk {} {{\n", .{disk_number});

    var disk_name_buf: pathf.Path = .{};
    const disk_name = try pathf.append(&disk_name_buf, index_name);
    games.pointPathToDisk(game, disk_name.full(), disk_number);

    const in_file = try input_dir.openFileZ(disk_name.full(), .{});
    defer in_file.close();
    const in_xor = io.xorReader(in_file.reader(), xor_key);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in = std.io.countingReader(in_buf.reader());

    var file_blocks = blockReader(&in);

    const lecf_size = try file_blocks.expectBlock("LECF");
    const lecf_end: u32 = @intCast(in.bytes_read + lecf_size);
    var lecf_blocks = blockReader(&in);

    var room_index: u8 = 0;
    while (in.bytes_read < lecf_end) : (room_index += 1) {
        const lflf_size = try lecf_blocks.expectBlock("LFLF");
        try extractRoom(gpa, &in, disk_number, room_index, lflf_size, output_dir, code);
    }

    try lecf_blocks.finish(lecf_end);

    try file_blocks.finishEof();

    try code.appendSlice(gpa, "}\n");
}

fn extractRoom(
    gpa: std.mem.Allocator,
    in: anytype,
    disk_number: u8,
    room_index: u8,
    lflf_size: u32,
    output_dir: std.fs.Dir,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var filename_buf: ["LFLF_00_00.bin".len + 1]u8 = undefined;
    const filename = try std.fmt.bufPrintZ(
        &filename_buf,
        "{s}_{:0>2}_{:0>2}.bin",
        .{ "LFLF", disk_number, room_index },
    );
    const file = try output_dir.createFileZ(filename, .{});
    defer file.close();
    try io.copy(std.io.limitedReader(in.reader(), lflf_size), file.writer());

    try code.writer(gpa).print(
        "    raw-block \"{s}\" \"{s}\"\n",
        .{ "LFLF", filename },
    );
}
