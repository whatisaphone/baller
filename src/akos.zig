const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
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
    manifest: *std.ArrayList(u8),
) !void {
    var stream: std.io.Reader = .fixed(akos_raw);
    var blocks = oldFixedBlockReader(&stream);

    const akhd = try blocks.expectAsValue(.AKHD, Akhd);
    try writeRawBlock(allocator, .AKHD, std.mem.asBytes(akhd), out_dir, out_path, 4, .block, manifest);

    const akpl = try blocks.expectAsSlice(.AKPL);
    try fs.writeFileZ(out_dir, "AKPL.bin", akpl);
    try manifest.print(allocator, "    akpl \"{s}/{s}\"\n", .{ out_path, "AKPL.bin" });

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

    while (stream.seek < akos_raw.len) {
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
    manifest: *std.ArrayList(u8),
) !void {
    const codec = std.meta.intToEnum(CompressionCodec, akhd.cel_compression_codec) catch
        return error.CelDecode;

    const stride = bmp.calcStride(cel.info.width);
    const bmp_size = bmp.calcFileSize(cel.info.width, cel.info.height);

    const bmp_buf = try allocator.alloc(u8, bmp_size);
    defer allocator.free(bmp_buf);
    var bmp_writer: std.io.Writer = .fixed(bmp_buf);

    try bmp.writeHeader(&bmp_writer, cel.info.width, cel.info.height, bmp_size);
    try bmp.writePalette(&bmp_writer, rgbs);

    switch (codec) {
        .byle_rle => try decodeCelByleRle(akpl, cel, bmp_writer.unusedCapacitySlice(), stride),
        .trle => try decodeCelTrle(cel, bmp_writer.unusedCapacitySlice()),
    }

    var bmp_path_buf: ["cel_0000_AKCD.bmp".len + 1]u8 = undefined;
    const bmp_path = try std.fmt.bufPrintZ(&bmp_path_buf, "cel_{:0>4}_AKCD.bmp", .{cel.index});

    try fs.writeFileZ(out_dir, bmp_path, bmp_buf);

    const codec_str = switch (codec) {
        .byle_rle => "byle",
        .trle => "trle",
    };
    try manifest.print(
        allocator,
        "    akcd {s} \"{s}/{s}\"\n",
        .{ codec_str, out_path, bmp_path },
    );
}

// based on ScummVM's AkosRenderer::paintCelByleRLE
fn decodeCelByleRle(akpl: []const u8, cel: Cel, pixels: []u8, stride: u31) !void {
    const run_mask, const color_shift = byleParams(akpl.len) orelse return error.CelDecode;

    var in: io.FixedReader = .init(cel.data);

    var i: u32 = 0;
    var x: u16 = 0;
    var y = cel.info.height;

    decode: while (true) {
        const b = try in.takeByte();
        var run = b & run_mask;
        const color = b >> color_shift;
        if (run == 0)
            run = try in.takeByte();

        for (0..run) |_| {
            pixels[i] = akpl[color];
            i += stride;
            y -= 1;
            if (y == 0) {
                x += 1;
                if (x == cel.info.width)
                    break :decode;
                i = x; // Move to pixel `x` in the first row
                y = cel.info.height;
            }
        }
    }

    // Zero out the BMP padding bytes after each row so the output is deterministic
    if (cel.info.width != stride) {
        y = 0;
        while (y != cel.info.height) : (y += 1)
            @memset(pixels[y * stride ..][cel.info.width..stride], 0);
    }
}

/// returns `run_mask`, `color_shift`
fn byleParams(akpl_len: usize) ?struct { u8, u3 } {
    return switch (akpl_len) {
        16 => .{ 0xf, 4 },
        32 => .{ 0x7, 3 },
        64 => .{ 0x3, 2 },
        else => null,
    };
}

fn decodeCelTrle(cel: Cel, pixels: []u8) !void {
    var data_stream: std.io.Reader = .fixed(cel.data);
    var pixels_buf: std.ArrayList(u8) = .initBuffer(pixels);
    try awiz.decodeRle(cel.info.width, cel.info.height, &data_stream, &pixels_buf);
    // ensure buffer is fully initialized
    if (pixels_buf.items.len != pixels_buf.capacity)
        return error.CelDecode;
}

const EncodeState = struct {
    project_dir: std.fs.Dir,
    akci: std.ArrayList(Akci) = .empty,
    cd_offsets: std.ArrayList(u32) = .empty,
    akcd: std.ArrayList(u8) = .empty,

    fn deinit(self: *EncodeState, allocator: std.mem.Allocator) void {
        self.akcd.deinit(allocator);
        self.cd_offsets.deinit(allocator);
        self.akci.deinit(allocator);
    }
};

pub fn encode(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    project: *const Project,
    project_dir: std.fs.Dir,
    awiz_strategy: awiz.EncodingStrategy,
    room_number: u8,
    akos_node_index: Ast.NodeIndex,
    out: *std.ArrayList(u8),
) !void {
    const file = &project.files.items[room_number].?;
    const akos = &file.ast.nodes.at(akos_node_index).akos;

    var state: EncodeState = .{
        .project_dir = project_dir,
    };
    defer state.deinit(gpa);

    var akcd_count: u32 = 0;
    for (file.ast.getExtra(akos.children)) |node_index| {
        if (file.ast.nodes.at(node_index).* == .akcd)
            akcd_count += 1;
    }

    try state.akci.ensureTotalCapacityPrecise(gpa, akcd_count);
    try state.cd_offsets.ensureTotalCapacityPrecise(gpa, akcd_count);
    try state.akcd.ensureTotalCapacity(gpa, @as(u32, @intCast(akcd_count)) * 2048);

    var akpl_buf: [64]u8 = undefined;
    var akpl: ?std.io.Writer = null;

    for (file.ast.getExtra(akos.children)) |node_index| {
        const node = file.ast.nodes.at(node_index);

        if (node.* != .akcd)
            try flushCels(gpa, &state, out);

        switch (node.*) {
            .raw_block => |*n| {
                try encodeRawBlock(gpa, project_dir, file, n, out);
            },
            .akpl => |*n| {
                if (akpl != null) return error.BadData;
                akpl = .fixed(&akpl_buf);
                try fs.readFileInto(project_dir, file.ast.strings.get(n.path), &akpl.?);

                const start = try beginBlockAl(gpa, out, .AKPL);
                try out.appendSlice(gpa, akpl.?.buffered());
                endBlockAl(out, start);
            },
            .akcd => |*n| {
                if (akpl == null) return error.BadData;
                const loc: Diagnostic.Location = .node(file, node_index);
                try encodeCelBmp(gpa, diagnostic, loc, akpl.?.buffered(), file.ast.strings.get(n.path), &state, n.compression, awiz_strategy);
            },
            else => unreachable,
        }
    }

    try flushCels(gpa, &state, out);
}

fn encodeCelBmp(
    allocator: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    akpl: []const u8,
    path: []const u8,
    state: *EncodeState,
    codec: CompressionCodec,
    strategy: awiz.EncodingStrategy,
) !void {
    const bmp_data = try fs.readFile(allocator, state.project_dir, path);
    defer allocator.free(bmp_data);

    var bmp_err: bmp.HeaderError = undefined;
    const bmp_header = bmp.readHeader(bmp_data, &bmp_err) catch
        return bmp_err.addToDiag(diagnostic, loc);
    const bmp8 = bmp_header.as8Bit(&bmp_err) catch
        return bmp_err.addToDiag(diagnostic, loc);
    const width = std.math.cast(u16, bmp8.width) orelse return error.BadData;
    const height = std.math.cast(u16, bmp8.height) orelse return error.BadData;

    try state.akci.append(utils.null_allocator, .{ .width = width, .height = height });

    try state.cd_offsets.append(utils.null_allocator, @intCast(state.akcd.items.len));

    (switch (codec) {
        .byle_rle => encodeCelByleRle(allocator, diagnostic, loc, &bmp8, akpl, &state.akcd),
        .trle => encodeCelTrle(allocator, &bmp8, strategy, &state.akcd),
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };
}

const CelEncodeState = struct {
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    run_mask: u8,
    color_shift: u3,
    color_map: [256]u8,
    bitmap: *const bmp.Bmp8,
    x: usize,
    y: usize,
    run: u8,
    color: u8,
};

fn encodeCelByleRle(
    allocator: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    bitmap: *const bmp.Bmp8,
    akpl: []const u8,
    out: *std.ArrayList(u8),
) !void {
    const run_mask, const color_shift = byleParams(akpl.len) orelse return error.BadData;

    var state: CelEncodeState = .{
        .diagnostic = diagnostic,
        .loc = loc,
        .run_mask = run_mask,
        .color_shift = color_shift,
        .color_map = @splat(0xff),
        .bitmap = bitmap,
        .x = 0,
        .y = 0,
        .run = 0,
        .color = undefined,
    };

    for (0.., akpl) |i, color| {
        if (state.color_map[color] == 0xff)
            state.color_map[color] = @intCast(i);
    }

    try out.ensureUnusedCapacity(allocator, bitmap.width * bitmap.height / 4);

    while (true) {
        const color = bitmap.getPixel(state.x, state.y);

        if (state.run == 0) {
            state.color = color;
            state.run += 1;
        } else if (color == state.color and state.run < 255) {
            state.run += 1;
        } else {
            try flushRun(allocator, &state, out);
            state.color = color;
            state.run = 1;
        }

        state.y += 1;
        if (state.y == state.bitmap.height) {
            state.y = 0;
            state.x += 1;
            if (state.x == state.bitmap.width)
                break;
        }
    }
    try flushRun(allocator, &state, out);
}

fn flushRun(allocator: std.mem.Allocator, state: *CelEncodeState, out: *std.ArrayList(u8)) !void {
    const index = state.color_map[state.color];
    if (index == 0xff) {
        state.diagnostic.errAt(
            state.loc,
            "color index {} found in image but not in AKPL",
            .{state.color},
        );
        return error.AddedToDiagnostic;
    }

    if (state.run == state.run & state.run_mask) {
        try out.append(allocator, state.run | @shlExact(index, state.color_shift));
    } else {
        try out.append(allocator, 0 | @shlExact(index, state.color_shift));
        try out.append(allocator, state.run);
    }
}

fn encodeCelTrle(
    allocator: std.mem.Allocator,
    bitmap: *const bmp.Bmp8,
    strategy: awiz.EncodingStrategy,
    out: *std.ArrayList(u8),
) !void {
    try awiz.encodeRle(allocator, bitmap.*, strategy, out);
}

fn flushCels(gpa: std.mem.Allocator, state: *EncodeState, out: *std.ArrayList(u8)) !void {
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
    endBlockAl(out, akof_start);

    const akci_start = try beginBlockAl(gpa, out, .AKCI);
    try out.appendSlice(gpa, std.mem.sliceAsBytes(state.akci.items));
    endBlockAl(out, akci_start);

    const akcd_start = try beginBlockAl(gpa, out, .AKCD);
    try out.appendSlice(gpa, std.mem.sliceAsBytes(state.akcd.items));
    endBlockAl(out, akcd_start);

    state.akci.clearRetainingCapacity();
    state.cd_offsets.clearRetainingCapacity();
    state.akcd.clearRetainingCapacity();
}
