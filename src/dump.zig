const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Block = @import("block_reader.zig").Block;
const FxbclReader = @import("block_reader.zig").FxbclReader;
const StreamingBlockReader = @import("block_reader.zig").StreamingBlockReader;
const cliargs = @import("cliargs.zig");
const fs = @import("fs.zig");
const io = @import("io.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var output_path_opt: ?[*:0]const u8 = null;
    var xor_key_opt: ?u8 = null;

    var it: cliargs.Iterator = .init(args);
    while (it.next()) |arg| switch (arg) {
        .positional => |str| {
            if (output_path_opt == null)
                output_path_opt = str
            else
                return arg.reportUnexpected();
        },
        .short_option => |opt| {
            if (opt.flag == 'x') {
                if (xor_key_opt != null) return arg.reportDuplicate();
                xor_key_opt = parseXorKey(opt.value) orelse return arg.reportInvalidValue();
            } else {
                return arg.reportUnexpected();
            }
        },
        .long_option => |opt| {
            if (std.mem.eql(u8, opt.flag, "xor")) {
                if (xor_key_opt != null) return arg.reportDuplicate();
                xor_key_opt = parseXorKey(opt.value) orelse return arg.reportInvalidValue();
            } else {
                return arg.reportUnexpected();
            }
        },
        else => return arg.reportUnexpected(),
    };

    const output_path = output_path_opt orelse return cliargs.reportMissing("output");
    const xor_key = xor_key_opt orelse 0x00;

    var in_xor = io.xorReader(std.io.getStdIn().reader(), xor_key);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in_count = std.io.countingReader(in_buf.reader());
    var in = std.io.limitedReader(in_count.reader(), std.math.maxInt(u32));

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();
    const diag: Diagnostic.ForBinaryFile = .init(&diagnostic, "-");

    _ = run(&in, &diag, output_path) catch |err| {
        if (err != error.AddedToDiagnostic) {
            const pos: u32 = @intCast(in.inner_reader.context.bytes_read);
            diag.zigErr(pos, "unexpected error: {s}", .{}, err);
        }
    };
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

fn parseXorKey(s: []const u8) ?u8 {
    if (s.len != 2) return null;
    return std.fmt.parseInt(u8, s, 16) catch null;
}

const Cx = struct {
    in: *FxbclReader,
    diag: *const Diagnostic.ForBinaryFile,
    dir: std.fs.Dir,
};

pub fn run(
    in: *FxbclReader,
    diag: *const Diagnostic.ForBinaryFile,
    output_path: [*:0]const u8,
) !void {
    try fs.makeDirIfNotExistZ(std.fs.cwd(), output_path);
    var dir = try std.fs.cwd().openDirZ(output_path, .{});
    defer dir.close();

    var cx: Cx = .{
        .in = in,
        .diag = diag,
        .dir = dir,
    };
    try dump(&cx);
}

fn dump(cx: *Cx) !void {
    var br: StreamingBlockReader = .init(cx.in, cx.diag);
    while (try br.next()) |block| {
        try dumpBlock(cx, &block);
        try br.finish(&block);
    }
    // since we're reading from stdin, we don't know the real length
    br.expectMismatchedEnd();
}

fn dumpBlock(cx: *Cx, block: *const Block) anyerror!void {
    // Try dumping it as nested, but if anything fails, fall back to raw
    if (block.size < Block.header_size)
        return dumpRaw(cx, block, null);
    var br: StreamingBlockReader = .init(cx.in, cx.diag);
    const offset, const header = try br.readHeader() orelse return error.BadData;
    const child = br.validate(offset, header) orelse
        return dumpRaw(cx, block, std.mem.toBytes(header));
    try dumpNested(cx, &br, block, &child);
    try br.end();
}

fn dumpNested(
    cx: *Cx,
    br: *StreamingBlockReader,
    parent: *const Block,
    first_child: *const Block,
) !void {
    var name_buf: ["00000000-XXXX.bin".len + 1]u8 = undefined;
    const name = std.fmt.bufPrintZ(
        &name_buf,
        "{x:0>8}-{}",
        .{ parent.offset(), parent.id },
    ) catch unreachable;

    const parent_dir = cx.dir;
    try fs.makeDirIfNotExistZ(parent_dir, name);
    cx.dir = try parent_dir.openDirZ(name, .{});
    defer {
        cx.dir.close();
        cx.dir = parent_dir;
    }

    br.commit(first_child);
    try dumpBlock(cx, first_child);
    try br.finish(first_child);

    while (try br.next()) |block| {
        try dumpBlock(cx, &block);
        try br.finish(&block);
    }
}

fn dumpRaw(cx: *Cx, block: *const Block, prefix: ?[8]u8) !void {
    var name_buf: ["00000000-XXXX.bin".len + 1]u8 = undefined;
    const name = std.fmt.bufPrintZ(
        &name_buf,
        "{x:0>8}-{}.bin",
        .{ block.offset(), block.id },
    ) catch unreachable;

    const file = try cx.dir.createFileZ(name, .{});
    defer file.close();

    if (prefix) |*b|
        try file.writeAll(b);
    try io.copy(cx.in.reader(), file);
}
