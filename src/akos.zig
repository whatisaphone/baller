const std = @import("std");

const Project = @import("Project.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const oldFixedBlockReader = @import("block_reader.zig").oldFixedBlockReader;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const bmp = @import("bmp.zig");
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const encodeRawBlock = @import("plan.zig").encodeRawBlock;
const report = @import("report.zig");
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

pub const CompressionCodec = enum(u8) {
    byle_rle = 0x01,
    trle = 0x20,
};

pub fn decode(
    allocator: std.mem.Allocator,
    akos_raw: []const u8,
    out_path: []const u8,
    out_dir: std.fs.Dir,
    manifest: *std.ArrayListUnmanaged(u8),
) !void {
    var stream = std.io.fixedBufferStream(akos_raw);
    var blocks = oldFixedBlockReader(&stream);

    const akhd = try blocks.expectAsValue(.AKHD, Akhd);
    try writeRawBlock(allocator, .AKHD, std.mem.asBytes(akhd), out_dir, out_path, 4, .block, manifest);

    const akpl = try blocks.expectAsSlice(.AKPL);
    try fs.writeFileZ(out_dir, "AKPL.bin", akpl);
    try manifest.writer(allocator).print("    akpl \"{s}/{s}\"\n", .{ out_path, "AKPL.bin" });

    const rgbs = try blocks.expectAsValue(.RGBS, [0x300]u8);
    try writeRawBlock(allocator, .RGBS, rgbs, out_dir, out_path, 4, .block, manifest);

    while (try blocks.peek() != .AKOF) {
        const block_id, const block_raw = try blocks.nextAsSlice();
        try writeRawBlock(allocator, block_id, block_raw, out_dir, out_path, 4, .block, manifest);
    }

    const akof_len = try blocks.assume(.AKOF);
    if (akof_len != akhd.cels_count * @sizeOf(Akof))
        return error.BadData;
    const akof_raw = try io.readInPlace(&stream, akof_len);
    const akof = std.mem.bytesAsSlice(Akof, akof_raw);

    const akci_raw = try blocks.expectAsSlice(.AKCI);
    if (akci_raw.len != akhd.cels_count * @sizeOf(Akci))
        return error.BadData;

    const akcd_raw = try blocks.expectAsSlice(.AKCD);

    for (0..akhd.cels_count) |cel_index| {
        const off = &akof[cel_index];
        const cel_info: *align(1) const Akci =
            @ptrCast(akci_raw[off.akci..][0..@sizeOf(Akci)]);
        const cd_start = off.akcd;
        const cd_end = if (cel_index != akhd.cels_count - 1)
            akof[cel_index + 1].akcd
        else
            null;
        const cel_data = if (cd_end) |end|
            akcd_raw[cd_start..end]
        else
            akcd_raw[cd_start..];

        const cel: Cel = .{
            .index = cel_index,
            .info = cel_info.*,
            .data = cel_data,
        };
        try decodeCel(allocator, akhd, akpl, rgbs, cel, out_path, out_dir, manifest);
    }

    while (stream.pos < akos_raw.len) {
        const block_id, const block_raw = try blocks.nextAsSlice();
        try writeRawBlock(allocator, block_id, block_raw, out_dir, out_path, 4, .block, manifest);
    }

    try blocks.finishEof();
}

const Cel = struct {
    index: usize,
    info: Akci,
    data: []const u8,
};

fn decodeCel(
    allocator: std.mem.Allocator,
    akhd: *align(1) const Akhd,
    akpl: []const u8,
    rgbs: *const [0x300]u8,
    cel: Cel,
    out_path: []const u8,
    out_dir: std.fs.Dir,
    manifest: *std.ArrayListUnmanaged(u8),
) !void {
    const codec = std.meta.intToEnum(CompressionCodec, akhd.cel_compression_codec) catch
        return error.CelDecode;

    const stride = bmp.calcStride(cel.info.width);
    const bmp_size = bmp.calcFileSize(cel.info.width, cel.info.height);

    const bmp_buf = try allocator.alloc(u8, bmp_size);
    defer allocator.free(bmp_buf);

    var bmp_stream = std.io.fixedBufferStream(bmp_buf);
    const bmp_writer = bmp_stream.writer();

    try bmp.writeHeader(bmp_writer, cel.info.width, cel.info.height, bmp_size);
    try bmp.writePalette(bmp_writer, rgbs);

    switch (codec) {
        .byle_rle => try decodeCelByleRle(akpl, cel, bmp_buf[bmp_stream.pos..], stride),
        .trle => try decodeCelTrle(cel, bmp_buf[bmp_stream.pos..]),
    }

    var bmp_path_buf: ["cel_0000_AKCD.bmp".len + 1]u8 = undefined;
    const bmp_path = try std.fmt.bufPrintZ(&bmp_path_buf, "cel_{:0>4}_AKCD.bmp", .{cel.index});

    try fs.writeFileZ(out_dir, bmp_path, bmp_buf);

    const codec_str = switch (codec) {
        .byle_rle => "byle",
        .trle => "trle",
    };
    try manifest.writer(allocator).print(
        "    akcd {s} \"{s}/{s}\"\n",
        .{ codec_str, out_path, bmp_path },
    );
}

// based on ScummVM's AkosRenderer::paintCelByleRLE
fn decodeCelByleRle(akpl: []const u8, cel: Cel, pixels: []u8, stride: u31) !void {
    const run_mask: u8, const color_shift: u3 = switch (akpl.len) {
        16 => .{ 0xf, 4 },
        32 => .{ 0x7, 3 },
        64 => .{ 0x3, 2 },
        else => return error.CelDecode,
    };

    var in_stream = std.io.fixedBufferStream(cel.data);
    var in = in_stream.reader();

    var i: u32 = 0;
    var x: u16 = 0;
    var y = cel.info.height;

    while (true) {
        const b = try in.readByte();
        var run = b & run_mask;
        const color = b >> color_shift;
        if (run == 0)
            run = try in.readByte();

        for (0..run) |_| {
            pixels[i] = akpl[color];
            i += stride;
            y -= 1;
            if (y == 0) {
                y = cel.info.height;
                x += 1;
                if (x == cel.info.width)
                    return;
                i = x; // Move to pixel `x` in the first row
            }
        }
    }
}

fn decodeCelTrle(cel: Cel, pixels: []u8) !void {
    var data_stream = std.io.fixedBufferStream(cel.data);

    // api quirk: decodeRle takes its buffer as an ArrayListUnmanaged, although
    // it never resizes it.
    var pixels_buf: std.ArrayListUnmanaged(u8) = .initBuffer(pixels);

    try awiz.decodeRle(cel.info.width, cel.info.height, &data_stream, &pixels_buf);

    // ensure buffer is fully initialized
    if (pixels_buf.items.len != pixels_buf.capacity)
        return error.CelDecode;
}

const EncodeState = struct {
    project_dir: std.fs.Dir,
    akci: std.ArrayListUnmanaged(Akci) = .empty,
    cd_offsets: std.ArrayListUnmanaged(u32) = .empty,
    akcd: std.ArrayListUnmanaged(u8) = .empty,

    fn deinit(self: *EncodeState, allocator: std.mem.Allocator) void {
        self.akcd.deinit(allocator);
        self.cd_offsets.deinit(allocator);
        self.akci.deinit(allocator);
    }
};

pub fn encode(
    gpa: std.mem.Allocator,
    project: *const Project,
    project_dir: std.fs.Dir,
    awiz_strategy: awiz.EncodingStrategy,
    room_number: u8,
    akos_node_index: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const file = &project.files.items[room_number].?;
    const akos = &file.ast.nodes.items[akos_node_index].akos;

    var state: EncodeState = .{
        .project_dir = project_dir,
    };
    defer state.deinit(gpa);

    var akcd_count: u32 = 0;
    for (file.ast.getExtra(akos.children)) |node_index| {
        if (file.ast.nodes.items[node_index] == .akcd)
            akcd_count += 1;
    }

    try state.akci.ensureTotalCapacityPrecise(gpa, akcd_count);
    try state.cd_offsets.ensureTotalCapacityPrecise(gpa, akcd_count);
    try state.akcd.ensureTotalCapacity(gpa, @as(u32, @intCast(akcd_count)) * 256);

    var akpl: ?std.BoundedArray(u8, 64) = null;

    for (file.ast.getExtra(akos.children)) |node_index| {
        const node = &file.ast.nodes.items[node_index];

        if (node.* != .akcd)
            try flushCels(gpa, &state, out);

        switch (node.*) {
            .raw_block => |*n| {
                try encodeRawBlock(gpa, out, n.block_id, project_dir, file.ast.strings.get(n.path));
            },
            .akpl => |*n| {
                if (akpl != null) return error.BadData;
                akpl = .{};
                try fs.readFileInto(project_dir, file.ast.strings.get(n.path), akpl.?.writer());

                const start = try beginBlockAl(gpa, out, .AKPL);
                try out.appendSlice(gpa, akpl.?.slice());
                try endBlockAl(out, start);
            },
            .akcd => |*n| {
                if (akpl == null) return error.BadData;
                try encodeCelBmp(gpa, akpl.?.slice(), file.ast.strings.get(n.path), &state, n.compression, awiz_strategy);
            },
            else => unreachable,
        }
    }

    try flushCels(gpa, &state, out);
}

fn encodeCelRaw(
    allocator: std.mem.Allocator,
    line: []const u8,
    state: *EncodeState,
) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const width_str = tokens.next() orelse return error.BadData;
    const width = try std.fmt.parseInt(u16, width_str, 10);

    const height_str = tokens.next() orelse return error.BadData;
    const height = try std.fmt.parseInt(u16, height_str, 10);

    const path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Encode

    try state.akci.append(utils.null_allocator, .{ .width = width, .height = height });

    try state.cd_offsets.append(utils.null_allocator, @intCast(state.akcd.items.len));

    const file = try state.project_dir.openFile(path, .{});
    defer file.close();

    const data_stat = try file.stat();
    const data_len: u32 = @intCast(data_stat.size);

    try state.akcd.ensureUnusedCapacity(allocator, data_len);
    try file.reader().readNoEof(state.akcd.unusedCapacitySlice()[0..data_len]);
    state.akcd.items.len += data_len;
}

fn encodeCelBmp(
    allocator: std.mem.Allocator,
    akpl: []const u8,
    path: []const u8,
    state: *EncodeState,
    codec: CompressionCodec,
    strategy: awiz.EncodingStrategy,
) !void {
    const bmp_data = try fs.readFile(allocator, state.project_dir, path);
    defer allocator.free(bmp_data);

    const bitmap = try bmp.readHeader(bmp_data, .{});
    const width = std.math.cast(u16, bitmap.width()) orelse return error.BadData;
    const height = std.math.cast(u16, bitmap.height()) orelse return error.BadData;

    try state.akci.append(utils.null_allocator, .{ .width = width, .height = height });

    try state.cd_offsets.append(utils.null_allocator, @intCast(state.akcd.items.len));

    (switch (codec) {
        .byle_rle => encodeCelByleRle(&bitmap, akpl, state.akcd.writer(allocator)),
        .trle => encodeCelTrle(&bitmap, strategy, state.akcd.writer(allocator)),
    }) catch |err| {
        report.fatal("error encoding {s}", .{path});
        return err;
    };
}

const CelEncodeState = struct {
    run_mask: u8,
    color_shift: u3,
    bitmap: *const bmp.Bmp,
    x: usize,
    y: usize,
    run: u8,
    color: u8,
};

fn encodeCelByleRle(bitmap: *const bmp.Bmp, akpl: []const u8, out: anytype) !void {
    // TODO: duplicated
    const run_mask: u8, const color_shift: u3 = switch (akpl.len) {
        16 => .{ 0xf, 4 },
        32 => .{ 0x7, 3 },
        64 => .{ 0x3, 2 },
        else => return error.BadData,
    };

    var state: CelEncodeState = .{
        .run_mask = run_mask,
        .color_shift = color_shift,
        .bitmap = bitmap,
        .x = 0,
        .y = 0,
        .run = 0,
        .color = undefined,
    };

    while (true) {
        const color = bitmap.getPixel(state.x, state.y);

        if (state.run == 0) {
            state.color = color;
            state.run += 1;
        } else if (color == state.color and state.run < 255) {
            state.run += 1;
        } else {
            try flushRun(akpl, &state, out);
            state.color = color;
            state.run = 1;
        }

        state.y += 1;
        if (state.y == state.bitmap.height()) {
            state.y = 0;
            state.x += 1;
            if (state.x == state.bitmap.width())
                break;
        }
    }
    try flushRun(akpl, &state, out);
}

fn flushRun(akpl: []const u8, state: *CelEncodeState, out: anytype) !void {
    const index: u8 = for (0.., akpl) |i, color| {
        if (color == state.color)
            break @intCast(i);
    } else {
        report.fatal("color index {} found in image but not in AKPL", .{state.color});
        return error.BadData;
    };

    if (state.run == state.run & state.run_mask) {
        try out.writeByte(state.run | @shlExact(index, state.color_shift));
    } else {
        try out.writeByte(0 | @shlExact(index, state.color_shift));
        try out.writeByte(state.run);
    }
}

fn encodeCelTrle(bitmap: *const bmp.Bmp, strategy: awiz.EncodingStrategy, out: anytype) !void {
    try awiz.encodeRle(bitmap.*, strategy, out);
}

fn flushCels(gpa: std.mem.Allocator, state: *EncodeState, out: anytype) !void {
    if (state.akci.items.len == 0)
        return;

    const akof_start = try beginBlockAl(gpa, out, .AKOF);
    for (0.., state.cd_offsets.items) |i, cd_off| {
        const off: Akof = .{
            .akci = @intCast(i * @sizeOf(Akci)),
            .akcd = cd_off,
        };
        try out.appendSlice(gpa, std.mem.asBytes(&off));
    }
    try endBlockAl(out, akof_start);

    const akci_start = try beginBlockAl(gpa, out, .AKCI);
    try out.appendSlice(gpa, std.mem.sliceAsBytes(state.akci.items));
    try endBlockAl(out, akci_start);

    const akcd_start = try beginBlockAl(gpa, out, .AKCD);
    try out.appendSlice(gpa, std.mem.sliceAsBytes(state.akcd.items));
    try endBlockAl(out, akcd_start);

    state.akci.clearRetainingCapacity();
    state.cd_offsets.clearRetainingCapacity();
    state.akcd.clearRetainingCapacity();
}
