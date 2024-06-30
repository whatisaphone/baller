const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const io = @import("io.zig");

pub const xor_key = 0x69;

pub fn run(allocator: std.mem.Allocator) !void {
    if (std.os.argv.len != 1 + 1 + 2)
        return error.CommandLine;

    const project_txt_path = std.mem.sliceTo(std.os.argv[2], 0);
    const output_path = std.mem.sliceTo(std.os.argv[3], 0);

    if (!std.mem.endsWith(u8, output_path, ".he0"))
        return error.CommandLine;

    const project_txt_file = try std.fs.cwd().openFileZ(project_txt_path, .{});
    defer project_txt_file.close();

    var project_txt_reader = std.io.bufferedReader(project_txt_file.reader());
    var project_txt_line_buf: [256]u8 = undefined;

    var cur_state: ?DiskState = null;

    while (true) {
        const line = project_txt_reader.reader()
            .readUntilDelimiter(&project_txt_line_buf, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (!std.mem.startsWith(u8, line, "room "))
            return error.BadData;
        var words = std.mem.splitScalar(u8, line[5..], ' ');
        const disk_number_str = words.next() orelse return error.BadData;
        const room_number_str = words.next() orelse return error.BadData;
        const room_name = words.next() orelse return error.BadData;
        _ = room_name; // autofix
        if (words.next()) |_| return error.BadData;

        const disk_number = try std.fmt.parseInt(u8, disk_number_str, 10);
        if (disk_number < 1 or disk_number > 26) return error.BadData;

        const room_number = try std.fmt.parseInt(u8, room_number_str, 10);
        if (room_number < 1) return error.BadData;

        if (cur_state) |*state| if (state.disk_number != disk_number) {
            try finishDisk(state);
            cur_state = null;
        };

        if (cur_state == null) {
            cur_state = @as(DiskState, undefined); // TODO: is there a better way?
            try startDisk(allocator, disk_number, output_path, &cur_state.?);
        }

        const state = &cur_state.?;

        const lflf_fixup = try beginBlock(&state.writer, "LFLF");

        try endBlock(&state.writer, &state.fixups, lflf_fixup);
    }

    if (cur_state) |*state|
        try finishDisk(state);
}

fn startDisk(
    allocator: std.mem.Allocator,
    disk_number: u8,
    output_path: [:0]u8,
    state: *DiskState,
) !void {
    output_path[output_path.len - 3] = '(';
    output_path[output_path.len - 2] = 'a' - 1 + disk_number;
    output_path[output_path.len - 1] = ')';

    state.disk_number = disk_number;

    state.file = try std.fs.cwd().createFileZ(output_path, .{});
    errdefer state.file.close();

    state.xor_writer = io.xorWriter(state.file.writer(), xor_key);
    state.buf_writer = std.io.bufferedWriter(state.xor_writer.writer());
    state.writer = std.io.countingWriter(state.buf_writer.writer());

    state.fixups = std.ArrayList(Fixup).init(allocator);
    errdefer state.fixups.deinit();

    // Hardcode the fixup pos since it's always the same
    const lecf_start = try beginBlock(&state.writer, "LECF");
    std.debug.assert(lecf_start == 0);
}

fn finishDisk(state: *DiskState) !void {
    // End the LECF block
    try endBlock(&state.writer, &state.fixups, 0);

    try state.buf_writer.flush();

    for (state.fixups.items) |fixup| {
        try state.file.seekTo(fixup.offset);
        try state.xor_writer.writer().writeInt(u32, fixup.value, .big);
    }
    state.fixups.deinit();

    state.file.close();
}

const DiskState = struct {
    disk_number: u8,
    file: std.fs.File,
    xor_writer: io.XorWriter(std.fs.File.Writer),
    buf_writer: std.io.BufferedWriter(4096, io.XorWriter(std.fs.File.Writer).Writer),
    writer: std.io.CountingWriter(std.io.BufferedWriter(4096, io.XorWriter(std.fs.File.Writer).Writer).Writer),
    fixups: std.ArrayList(Fixup),
};

const Fixup = struct {
    offset: u32,
    value: u32,
};

fn beginBlock(stream: anytype, comptime block_id: []const u8) !u32 {
    const id = comptime blockId(block_id);
    return beginBlockImpl(stream, id);
}

fn beginBlockImpl(stream: anytype, id: BlockId) !u32 {
    const block_start: u32 = @intCast(stream.bytes_written);

    try stream.writer().writeInt(BlockId, id, .little);
    // Write the length as a placeholder to be filled in later
    try stream.writer().writeAll(&@as([4]u8, undefined));

    return block_start;
}

fn endBlock(stream: anytype, fixups: *std.ArrayList(Fixup), block_start: u32) !void {
    const stream_pos: u32 = @intCast(stream.bytes_written);
    try fixups.append(.{
        .offset = block_start + 4,
        .value = stream_pos - block_start,
    });
}
