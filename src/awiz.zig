const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const BlockId = @import("block_id.zig").BlockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const bmp = @import("bmp.zig");
const BoundedArray = @import("bounded_array.zig").BoundedArray;
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const encodeRawBlock = @import("plan.zig").encodeRawBlock;
const utils = @import("utils.zig");

const max_supported_width = 1600;
const transparent = 5;

pub const Compression = enum(u8) {
    none = 0,
    rle = 1,
    none_16bit = 2,
};

pub fn decode(
    allocator: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    awiz_raw: []const u8,
    default_palette: *const [0x300]u8,
    name: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    indent: u8,
    out_dir: std.fs.Dir,
    out_path: []const u8,
) error{AddedToDiagnostic}!void {
    var stream = std.io.fixedBufferStream(awiz_raw);
    return decodeInner(allocator, diag, &stream, default_palette, name, code, indent, out_dir, out_path) catch |err| {
        diag.zigErr(@intCast(stream.pos), "general decode failure: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };
}

fn decodeInner(
    allocator: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    reader: anytype,
    default_palette: *const [0x300]u8,
    name: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    indent: u8,
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    var rgbs_opt: ?*const [0x300]u8 = null;
    var wizh_opt: ?struct {
        compression: Compression,
        width: u31,
        height: u31,
    } = null;

    var awiz_blocks = fixedBlockReader(reader, diag);

    while (try awiz_blocks.peek() != .WIZD) {
        try code.appendNTimes(allocator, ' ', indent);
        const block = try awiz_blocks.next().block();
        switch (block.id) {
            .RGBS => {
                const expected_rgbs_len = 0x300;
                if (block.size != expected_rgbs_len)
                    return error.BadData;
                rgbs_opt = try io.readInPlaceBytes(reader, expected_rgbs_len);

                try code.appendSlice(allocator, "rgbs\n");
            },
            .WIZH => {
                if (block.size != 12)
                    return error.BadData;
                const compression_int = try reader.reader().readInt(i32, .little);
                const compression = std.meta.intToEnum(Compression, compression_int) catch return error.BadData;
                const width_signed = try reader.reader().readInt(i32, .little);
                const width = std.math.cast(u31, width_signed) orelse return error.BadData;
                const height_signed = try reader.reader().readInt(i32, .little);
                const height = std.math.cast(u31, height_signed) orelse return error.BadData;

                wizh_opt = .{
                    .compression = compression,
                    .width = width,
                    .height = height,
                };

                try code.appendSlice(allocator, "wizh\n");
            },
            else => {
                const data = try io.readInPlace(reader, block.size);
                try writeRawBlock(allocator, block.id, data, out_dir, out_path, 0, .{ .symbol_block = name }, code);
            },
        }
    }

    const wizd = try awiz_blocks.assume(.WIZD).block();
    const wizd_end = wizd.end();

    const wizh = wizh_opt orelse return error.BadData;
    const width = wizh.width;
    const height = wizh.height;

    if (width > max_supported_width)
        return error.BadData;

    var bmp_buf = switch (wizh.compression) {
        .none => try decodeUncompressed(allocator, width, height, rgbs_opt orelse default_palette, reader),
        .rle => try decodeRleToBmp(allocator, width, height, rgbs_opt orelse default_palette, reader),
        .none_16bit => try decodeUncompressed16bit(allocator, width, height, reader),
    };
    defer bmp_buf.deinit(allocator);

    // Allow one byte of padding from the encoder
    if (reader.pos < wizd_end)
        _ = try reader.reader().readByte();

    try awiz_blocks.finish();

    var bmp_path_buf: [Symbols.max_name_len + ".bmp".len + 1]u8 = undefined;
    const bmp_path = std.fmt.bufPrintZ(&bmp_path_buf, "{s}.bmp", .{name}) catch unreachable;
    try fs.writeFileZ(out_dir, bmp_path, bmp_buf.items);

    try code.appendNTimes(allocator, ' ', indent);
    try code.writer(allocator).print(
        "wizd {} \"{s}/{s}\"\n",
        .{ @intFromEnum(wizh.compression), out_path, bmp_path },
    );
}

fn decodeUncompressed(
    allocator: std.mem.Allocator,
    width: u31,
    height: u31,
    palette: *const [0x300]u8,
    reader: anytype,
) !std.ArrayListUnmanaged(u8) {
    const bmp_file_size = bmp.calcFileSize(width, height);
    var bmp_buf: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_file_size);
    errdefer bmp_buf.deinit(allocator);

    try bmp.writeHeader(bmp_buf.fixedWriter(), width, height, bmp_file_size);
    try bmp.writePalette(bmp_buf.fixedWriter(), palette);

    for (0..height) |_| {
        const row = try io.readInPlace(reader, width);
        try bmp_buf.appendSlice(utils.null_allocator, row);

        try bmp.padRow(bmp_buf.writer(utils.null_allocator), width);
    }

    return bmp_buf;
}

pub fn decodeRleToBmp(
    allocator: std.mem.Allocator,
    width: u31,
    height: u31,
    palette: *const [0x300]u8,
    reader: anytype,
) !std.ArrayListUnmanaged(u8) {
    const bmp_file_size = bmp.calcFileSize(width, height);
    var bmp_buf: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_file_size);
    errdefer bmp_buf.deinit(allocator);

    try bmp.writeHeader(bmp_buf.fixedWriter(), width, height, bmp_file_size);
    try bmp.writePalette(bmp_buf.fixedWriter(), palette);
    try decodeRle(width, height, reader, &bmp_buf);
    return bmp_buf;
}

// based on ScummVM's auxDecompTRLEPrim
pub fn decodeRle(
    width: u31,
    height: u31,
    reader: anytype,
    bmp_buf: *std.ArrayListUnmanaged(u8),
) !void {
    for (0..height) |_| {
        const out_row_end = bmp_buf.items.len + width;

        const line_size = try reader.reader().readInt(u16, .little);
        const line_end = reader.pos + line_size;

        while (reader.pos < line_end) {
            const n = try reader.reader().readByte();
            if (n & 1 != 0) {
                const count = n >> 1;
                try bmp_buf.appendNTimes(utils.null_allocator, transparent, count);
            } else if (n & 2 != 0) {
                const count = (n >> 2) + 1;
                const color = try reader.reader().readByte();
                try bmp_buf.appendNTimes(utils.null_allocator, color, count);
            } else {
                const count = (n >> 2) + 1;
                const chunk = try io.readInPlace(reader, count);
                try bmp_buf.appendSlice(utils.null_allocator, chunk);
            }
        }

        try bmp_buf.appendNTimes(utils.null_allocator, transparent, out_row_end - bmp_buf.items.len);

        try bmp.padRow(bmp_buf.writer(utils.null_allocator), width);
    }
}

fn decodeUncompressed16bit(
    allocator: std.mem.Allocator,
    width: u31,
    height: u31,
    reader: anytype,
) !std.ArrayListUnmanaged(u8) {
    const bmp_file_size = bmp.calcFileSize15(width, height);
    var bmp_buf: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_file_size);
    errdefer bmp_buf.deinit(allocator);

    try bmp.writeHeader15(bmp_buf.fixedWriter(), width, height, bmp_file_size);

    for (0..height) |_| {
        const row = try io.readInPlace(reader, width * @sizeOf(u16));
        try bmp_buf.appendSlice(utils.null_allocator, row);

        try bmp.padRow15(bmp_buf.writer(utils.null_allocator), width);
    }

    return bmp_buf;
}

pub const EncodingStrategy = enum { original, max };

pub fn encode(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    loc: Diagnostic.Location,
    project_dir: std.fs.Dir,
    file: *const Project.SourceFile,
    children: Ast.ExtraSlice,
    strategy: EncodingStrategy,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    // First find the bitmap block so we can preload all data before we start

    const wizd = for (file.ast.getExtra(children)) |node_index| {
        const node = file.ast.nodes.at(node_index);
        if (node.* == .awiz_wizd) break &node.awiz_wizd;
    } else return error.BadData;

    const bmp_raw = try fs.readFile(gpa, project_dir, file.ast.strings.get(wizd.path));
    defer gpa.free(bmp_raw);

    var bmp_err: bmp.HeaderError = undefined;
    const bmp_header = bmp.readHeader(bmp_raw, &bmp_err) catch
        return bmp_err.addToDiag(diagnostic, loc);

    // Now write the blocks in the requested order

    for (file.ast.getExtra(children)) |node_index| {
        switch (file.ast.nodes.at(node_index).*) {
            .raw_block => |*n| {
                try encodeRawBlock(gpa, project_dir, file, n, out);
            },
            .awiz_rgbs => {
                const fixup = try beginBlockAl(gpa, out, .RGBS);
                const bmp8 = bmp_header.as8Bit(&bmp_err) catch
                    return bmp_err.addToDiag(diagnostic, loc);
                try writeRgbs(gpa, bmp8, out);
                endBlockAl(out, fixup);
            },
            .awiz_wizh => {
                const fixup = try beginBlockAl(gpa, out, .WIZH);
                try out.writer(gpa).writeInt(i32, @intFromEnum(wizd.compression), .little);
                try out.writer(gpa).writeInt(i32, bmp_header.width(), .little);
                try out.writer(gpa).writeInt(i32, bmp_header.height(), .little);
                endBlockAl(out, fixup);
            },
            .awiz_wizd => |_| {
                const fixup = try beginBlockAl(gpa, out, .WIZD);
                switch (wizd.compression) {
                    .none => {
                        const bmp8 = bmp_header.as8Bit(&bmp_err) catch
                            return bmp_err.addToDiag(diagnostic, loc);
                        try encodeUncompressed(gpa, bmp8, out);
                    },
                    .rle => {
                        const bmp8 = bmp_header.as8Bit(&bmp_err) catch
                            return bmp_err.addToDiag(diagnostic, loc);
                        try encodeRle(gpa, bmp8, strategy, out);
                    },
                    .none_16bit => {
                        const bmp15 = bmp_header.as15Bit(&bmp_err) catch
                            return bmp_err.addToDiag(diagnostic, loc);
                        try encodeUncompressed15Bit(gpa, bmp15, out);
                    },
                }
                // Pad output to a multiple of 2 bytes
                if ((out.items.len - fixup) & 1 != 0)
                    try out.append(gpa, 0);
                endBlockAl(out, fixup);
            },
            else => unreachable,
        }
    }
}

pub const placeholder_palette = placeholder_palette: {
    var result: [0x300]u8 = undefined;
    var i: usize = 0;
    for (0..4) |b| for (0..8) |g| for (0..8) |r| {
        result[i + 0] = 255 * r / 7;
        result[i + 1] = 255 * g / 7;
        result[i + 2] = 255 * b / 3;
        i += 3;
    };
    break :placeholder_palette result;
};

pub fn writeRgbs(gpa: std.mem.Allocator, header: bmp.Bmp8, out: *std.ArrayListUnmanaged(u8)) !void {
    for (header.palette) |color| {
        try out.append(gpa, color.rgbRed);
        try out.append(gpa, color.rgbGreen);
        try out.append(gpa, color.rgbBlue);
    }
}

pub fn encodeUncompressed(
    gpa: std.mem.Allocator,
    header: bmp.Bmp8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    try out.ensureUnusedCapacity(gpa, header.width * header.height);
    var rows = header.iterRows();
    while (rows.next()) |row|
        out.appendSliceAssumeCapacity(row);
}

pub fn encodeUncompressed15Bit(
    gpa: std.mem.Allocator,
    header: bmp.Bmp15,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    try out.ensureUnusedCapacity(gpa, header.width * header.height * @sizeOf(u16));
    var rows = header.iterRows();
    while (rows.next()) |row|
        out.appendSliceAssumeCapacity(row);
}

pub fn encodeRle(
    gpa: std.mem.Allocator,
    header: bmp.Bmp8,
    strategy: EncodingStrategy,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    try out.ensureUnusedCapacity(gpa, header.width * header.height / 4);
    var rows = header.iterRows();
    while (rows.next()) |row| {
        // worst-case encoding is 2 bytes for the line size, then 2 output bytes
        // for every input byte
        var line_buf: BoundedArray(u8, 2 + max_supported_width * 2) = .{};

        // reserve space for line size, to be filled in later
        line_buf.len = 2;

        switch (strategy) {
            .original => encodeRleRowOriginal(row, &line_buf),
            .max => encodeRleRowMax(row, &line_buf),
        }

        // fill in line size
        std.mem.writeInt(i16, line_buf.buffer[0..2], @intCast(line_buf.len - 2), .little);

        // flush line to output stream
        try out.appendSlice(gpa, line_buf.constSlice());
    }
}

fn encodeRleRowMax(
    row: []const u8,
    line_buf: *BoundedArray(u8, 2 + max_supported_width * 2),
) void {
    // skip encoding fully transparent rows
    if (std.mem.allEqual(u8, row, transparent))
        return;

    var i: usize = 0;
    while (i < row.len) {
        const color = row[i];
        i += 1;
        var run_len: u8 = 1;
        var blit_len: u8 = 1;
        if (i == row.len) {
            // end of line, nothing more to scan
        } else if (color == transparent) {
            while (i < row.len and run_len < 127) {
                if (row[i] != color)
                    break;
                i += 1;
                run_len += 1;
            }
        } else if (row[i] == color) {
            while (i < row.len and run_len < 64) {
                if (row[i] != color)
                    break;
                i += 1;
                run_len += 1;
            }
        } else {
            while (i < row.len and blit_len < 64) {
                if (row[i] == transparent)
                    break;
                if (i + 2 < row.len and row[i + 1] == row[i] and row[i + 2] == row[i + 1])
                    break;
                i += 1;
                blit_len += 1;
            }
        }

        if (color == transparent) {
            const n = 1 | @shlExact(run_len, 1);
            line_buf.appendAssumeCapacity(n);
        } else if (run_len != 1) {
            const n = 2 | @shlExact(run_len - 1, 2);
            line_buf.appendAssumeCapacity(n);
            line_buf.appendAssumeCapacity(color);
        } else {
            const n = @shlExact(blit_len - 1, 2);
            line_buf.appendAssumeCapacity(n);
            line_buf.appendSliceAssumeCapacity(row[i - blit_len .. i]);
        }
    }
}

fn encodeRleRowOriginal(
    row: []const u8,
    line_buf: *BoundedArray(u8, 2 + max_supported_width * 2),
) void {
    // skip encoding fully transparent rows
    if (std.mem.allEqual(u8, row, transparent))
        return;

    var i: usize = 0;
    while (i < row.len) {
        const color = row[i];
        i += 1;
        var run_len: u8 = 1;
        var blit_len: u8 = 1;
        if (i == row.len) {
            // end of line, nothing more to scan
        } else if (color == transparent) {
            while (i < row.len and run_len < 128) {
                if (row[i] != color)
                    break;
                i += 1;
                run_len += 1;
            }
        } else if (row[i] == color) {
            while (i < row.len and run_len < 128) {
                if (row[i] != color)
                    break;
                i += 1;
                run_len += 1;
            }
        } else {
            while (i < row.len and blit_len < 128) {
                if (row[i] == transparent)
                    break;
                if (i + 2 < row.len and
                    row[i] == row[i + 1] and
                    row[i + 1] == row[i + 2] and
                    blit_len < 126)
                    break;
                i += 1;
                blit_len += 1;
            }
        }

        if (color == transparent) {
            var remaining = run_len;
            while (remaining != 0) {
                const cur: u8 = @min(remaining, 127);
                remaining -= cur;

                const n = 1 | @shlExact(cur, 1);
                line_buf.appendAssumeCapacity(n);
            }
        } else if (run_len != 1) {
            var remaining = run_len;
            while (remaining != 0) {
                const cur: u8 = @min(remaining, 64);
                remaining -= cur;

                const n = 2 | @shlExact(cur - 1, 2);
                line_buf.appendAssumeCapacity(n);
                line_buf.appendAssumeCapacity(color);
            }
        } else {
            var remaining = blit_len;
            while (remaining != 0) {
                const cur: u8 = @min(remaining, 64);
                remaining -= cur;

                const n = @shlExact(cur - 1, 2);
                line_buf.appendAssumeCapacity(n);
                line_buf.appendSliceAssumeCapacity(row[i - remaining - cur .. i - remaining]);
            }
        }
    }
}
