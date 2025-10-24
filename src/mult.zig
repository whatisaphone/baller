const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Symbols = @import("Symbols.zig");
const awiz = @import("awiz.zig");
const FixedBlockReader = @import("block_reader.zig").FixedBlockReader;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const utils = @import("utils.zig");

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    name: [:0]const u8,
    mult_raw: []const u8,
    room_palette: *const [0x300]u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var in: std.io.Reader = .fixed(mult_raw);
    extractMultInner(gpa, diag, name, &in, room_palette, room_dir, room_path, code) catch |err| {
        if (err != error.AddedToDiagnostic)
            diag.zigErr(@intCast(in.seek), "unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };
}

fn extractMultInner(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    name: [:0]const u8,
    in: *std.io.Reader,
    room_palette: *const [0x300]u8,
    room_dir: std.fs.Dir,
    room_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !void {
    var mult_path_buf: [Ast.max_room_name_len + 1 + Symbols.max_name_len + 1]u8 = undefined;
    const mult_path = std.fmt.bufPrintZ(&mult_path_buf, "{s}/{s}", .{ room_path, name }) catch unreachable;

    try fs.makeDirIfNotExistZ(room_dir, name);
    var mult_dir = try room_dir.openDirZ(name, .{});
    defer mult_dir.close();

    var mult_blocks: FixedBlockReader = .init(in, diag);

    var mult_palette: ?*const [0x300]u8 = null;
    if (try mult_blocks.nextIf(.DEFA)) |defa| {
        const defa_raw = try defa.bytes();
        mult_palette = try extractDefa(gpa, diag, defa_raw, mult_dir, mult_path, code);
    }

    var wrap_blocks = try mult_blocks.expect(.WRAP).nested();

    const offs_block = try wrap_blocks.expect(.OFFS).block();
    const offs_raw = try io.readInPlace(in, offs_block.size);
    const offs = std.mem.bytesAsSlice(u32, offs_raw);

    var awiz_offsets: utils.TinyArray(u32, Ast.max_mult_children) = .empty;

    while (!wrap_blocks.atEnd()) {
        const awiz_block = try wrap_blocks.expect(.AWIZ).block();
        const awiz_raw = try io.readInPlace(in, awiz_block.size);
        const awiz_offset = awiz_block.offset() - offs_block.offset();
        try awiz_offsets.append(awiz_offset);

        const first_index = for (offs, 0..) |off, i| {
            if (awiz_offset == off) break i;
        } else return error.BadData;

        var awiz_name_buf: ["0000".len]u8 = undefined;
        const awiz_name = std.fmt.bufPrint(&awiz_name_buf, "{:0>4}", .{first_index}) catch unreachable;

        try code.appendSlice(gpa, "    awiz {\n");
        try awiz.decode(gpa, diag, awiz_raw, mult_palette orelse room_palette, awiz_name, code, 8, mult_dir, mult_path);
        try code.appendSlice(gpa, "    }\n");
    }

    var off_indices: utils.TinyArray(u8, Ast.max_mult_children) = .empty;
    for (offs) |off| {
        const index = std.sort.binarySearch(u32, awiz_offsets.slice(), off, orderU32) orelse
            return error.BadData;
        try off_indices.append(@intCast(index));
    }

    try code.appendSlice(gpa, "    indices [");
    for (off_indices.slice(), 0..) |index, i| {
        if (i != 0)
            try code.append(gpa, ' ');
        try code.writer(gpa).print("{}", .{index});
    }

    try code.appendSlice(gpa, "]\n}\n");

    try wrap_blocks.finish();
    try mult_blocks.finish();
}

fn orderU32(a: u32, b: u32) std.math.Order {
    return std.math.order(a, b);
}

fn extractDefa(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    defa_raw: []const u8,
    out_dir: std.fs.Dir,
    out_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
) !?*const [0x300]u8 {
    var rgbs: ?*const [0x300]u8 = null;

    var stream: std.io.Reader = .fixed(defa_raw);
    var blocks: FixedBlockReader = .init(&stream, diag);

    try code.writer(gpa).print("    raw-block {s} {{\n", .{"DEFA"});

    while (!blocks.atEnd()) {
        const block = try blocks.next().block();
        const bytes = try io.readInPlace(&stream, block.size);

        try writeRawBlock(gpa, block.id, bytes, out_dir, out_path, 8, .block, code);

        if (block.id == .RGBS) {
            if (rgbs != null) return error.BadData;
            if (bytes.len != 0x300) return error.BadData;
            rgbs = bytes[0..0x300];
        }
    }

    try blocks.finish();

    try code.appendSlice(gpa, "    }\n");

    return rgbs;
}
