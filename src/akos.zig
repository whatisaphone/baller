const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const parseBlockId = @import("block_id.zig").parseBlockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const Fixup = @import("block_writer.zig").Fixup;
const fs = @import("fs.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");
const utils = @import("utils.zig");

const Akhd = extern struct {
    version_number: u16,
    costume_flags: u16,
    chore_count: u16,
    cels_count: u16,
    cel_compression_codec: u16,
    layer_count: u16,
};

const Akof = extern struct {
    akcd: u32 align(1),
    akci: u16 align(1),
};

const Akci = extern struct {
    width: u16,
    height: u16,
};

pub fn decode(
    allocator: std.mem.Allocator,
    akos_raw: []const u8,
    cur_path: pathf.PrintedPath,
    manifest: ?*std.ArrayListUnmanaged(u8),
) !void {
    var stream = std.io.fixedBufferStream(akos_raw);
    var blocks = fixedBlockReader(&stream);

    const akhd = try blocks.expectBlockAsValue("AKHD", Akhd);
    try decodeAsRawBlock(
        allocator,
        comptime blockId("AKHD"),
        std.mem.asBytes(akhd),
        cur_path,
        manifest,
    );

    while (try blocks.peek() != comptime blockId("AKOF")) {
        const block_id, const block_raw = try blocks.nextAsSlice();
        try decodeAsRawBlock(allocator, block_id, block_raw, cur_path, manifest);
    }

    const akof_len = try blocks.assumeBlock("AKOF");
    if (akof_len != akhd.cels_count * @sizeOf(Akof))
        return error.BadData;
    const akof_raw = try io.readInPlace(&stream, akof_len);
    const akof = std.mem.bytesAsSlice(Akof, akof_raw);

    const akci_raw = try blocks.expectBlockAsSlice("AKCI");
    if (akci_raw.len != akhd.cels_count * @sizeOf(Akci))
        return error.BadData;

    const akcd_raw = try blocks.expectBlockAsSlice("AKCD");

    for (0..akhd.cels_count) |cel_index| {
        const off = &akof[cel_index];
        const cel_info: *align(1) const Akci =
            @ptrCast(akci_raw[off.akci..][0..@sizeOf(Akci)]);
        std.debug.assert(builtin.cpu.arch.endian() == .little);
        const cd_start = off.akcd;
        const cd_end = if (cel_index != akhd.cels_count - 1)
            akof[cel_index + 1].akcd
        else
            null;
        const cel_data = if (cd_end) |end|
            akcd_raw[cd_start..end]
        else
            akcd_raw[cd_start..];

        const path = try pathf.print(cur_path.buf, "cel_{:0>4}_AKCD.bin", .{cel_index});
        defer path.restore();

        try fs.writeFileZ(std.fs.cwd(), path.full(), cel_data);

        if (manifest) |m|
            try m.writer(allocator).print(
                "    cel {} {} {s}\n",
                .{ cel_info.width, cel_info.height, cur_path.relative() },
            );
    }

    while (stream.pos < akos_raw.len) {
        const block_id, const block_raw = try blocks.nextAsSlice();
        try decodeAsRawBlock(allocator, block_id, block_raw, cur_path, manifest);
    }

    try blocks.finishEof();
}

fn decodeAsRawBlock(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_raw: []const u8,
    cur_path: pathf.PrintedPath,
    manifest: ?*std.ArrayListUnmanaged(u8),
) !void {
    const block_path = try pathf.print(cur_path.buf, "{s}.bin", .{blockIdToStr(&block_id)});
    defer block_path.restore();

    try fs.writeFileZ(std.fs.cwd(), block_path.full(), block_raw);

    if (manifest) |m|
        try m.writer(allocator).print(
            "    raw-block {s} {s}\n",
            .{ blockIdToStr(&block_id), cur_path.relative() },
        );
}

const EncodeState = struct {
    cur_path: *pathf.Path,
    akci: std.ArrayListUnmanaged(Akci) = .{},
    cd_offsets: std.ArrayListUnmanaged(u32) = .{},
    akcd: std.ArrayListUnmanaged(u8) = .{},

    fn deinit(self: *EncodeState, allocator: std.mem.Allocator) void {
        self.akcd.deinit(allocator);
        self.cd_offsets.deinit(allocator);
        self.akci.deinit(allocator);
    }
};

pub fn encode(
    allocator: std.mem.Allocator,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    cur_path: *pathf.Path,
    writer: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    const akhd = akhd: {
        const room_line =
            try room_reader.reader().readUntilDelimiter(room_line_buf, '\n');
        var tokens = std.mem.tokenizeScalar(u8, room_line, ' ');
        if (!std.mem.eql(u8, tokens.next() orelse return error.BadData, "raw-block"))
            return error.BadData;
        if (!std.mem.eql(u8, tokens.next() orelse return error.BadData, "AKHD"))
            return error.BadData;
        const relative_path = tokens.next() orelse return error.BadData;
        if (tokens.next() != null)
            return error.BadData;

        const path = try pathf.append(cur_path, relative_path);
        defer path.restore();

        var akhd: Akhd = undefined;
        std.debug.assert(builtin.cpu.arch.endian() == .little);
        try fs.readFileIntoSliceZ(std.fs.cwd(), path.full(), std.mem.asBytes(&akhd));

        const start = try beginBlock(writer, "AKHD");
        try writer.writer().writeAll(std.mem.asBytes(&akhd));
        try endBlock(writer, fixups, start);

        break :akhd akhd;
    };

    var state = EncodeState{
        .cur_path = cur_path,
    };
    defer state.deinit(allocator);

    try state.akci.ensureTotalCapacityPrecise(allocator, akhd.cels_count);
    try state.cd_offsets.ensureTotalCapacityPrecise(allocator, akhd.cels_count);
    try state.akcd.ensureTotalCapacity(allocator, @as(u32, @intCast(akhd.cels_count)) * 256);

    while (true) {
        const room_line =
            try room_reader.reader().readUntilDelimiter(room_line_buf, '\n');
        var tokens = std.mem.tokenizeScalar(u8, room_line, ' ');
        const keyword = tokens.next() orelse return error.BadData;
        if (std.mem.eql(u8, keyword, "cel")) {
            try encodeCel(allocator, tokens.rest(), &state);
        } else if (std.mem.eql(u8, keyword, "raw-block")) {
            try flushCels(&state, writer, fixups);
            try encodeRawBlock(tokens.rest(), cur_path, writer, fixups);
        } else if (std.mem.eql(u8, keyword, "end-akos")) {
            try flushCels(&state, writer, fixups);
            break;
        } else {
            return error.BadData;
        }
    }
}

fn encodeCel(allocator: std.mem.Allocator, line: []const u8, state: *EncodeState) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const width_str = tokens.next() orelse return error.BadData;
    const width = try std.fmt.parseInt(u16, width_str, 10);

    const height_str = tokens.next() orelse return error.BadData;
    const height = try std.fmt.parseInt(u16, height_str, 10);

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Encode

    try state.akci.append(utils.null_allocator, .{ .width = width, .height = height });

    try state.cd_offsets.append(utils.null_allocator, @intCast(state.akcd.items.len));

    const path = try pathf.append(state.cur_path, relative_path);
    defer path.restore();

    const file = try std.fs.cwd().openFileZ(path.full(), .{});
    defer file.close();

    const data_stat = try file.stat();
    const data_len: u32 = @intCast(data_stat.size);

    try state.akcd.ensureUnusedCapacity(allocator, data_len);
    try file.reader().readNoEof(state.akcd.unusedCapacitySlice()[0..data_len]);
    state.akcd.items.len += data_len;
}

fn flushCels(state: *EncodeState, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    if (state.akci.items.len == 0)
        return;

    const akof_start = try beginBlock(out, "AKOF");
    for (0.., state.cd_offsets.items) |i, cd_off| {
        const off = Akof{
            .akci = @intCast(i * @sizeOf(Akci)),
            .akcd = cd_off,
        };
        std.debug.assert(builtin.cpu.arch.endian() == .little);
        try out.writer().writeAll(std.mem.asBytes(&off));
    }
    try endBlock(out, fixups, akof_start);

    const akci_start = try beginBlock(out, "AKCI");
    try out.writer().writeAll(std.mem.sliceAsBytes(state.akci.items));
    try endBlock(out, fixups, akci_start);

    const akcd_start = try beginBlock(out, "AKCD");
    try out.writer().writeAll(std.mem.sliceAsBytes(state.akcd.items));
    try endBlock(out, fixups, akcd_start);

    state.akci.clearRetainingCapacity();
    state.cd_offsets.clearRetainingCapacity();
    state.akcd.clearRetainingCapacity();
}

// TODO: this is duplicated
fn encodeRawBlock(
    line: []const u8,
    cur_path: *pathf.Path,
    writer: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const block_id_str = tokens.next() orelse return error.BadData;
    const block_id = parseBlockId(block_id_str) orelse return error.BadData;

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Copy block

    const start = try beginBlockImpl(writer, block_id);

    const path = try pathf.append(cur_path, relative_path);
    defer path.restore();

    try fs.readFileIntoZ(std.fs.cwd(), path.full(), writer.writer());

    try endBlock(writer, fixups, start);
}
