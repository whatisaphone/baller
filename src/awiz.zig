const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const oldFixedBlockReader = @import("block_reader.zig").oldFixedBlockReader;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const bmp = @import("bmp.zig");
const fs = @import("fs.zig");
const io = @import("io.zig");
const utils = @import("utils.zig");

const max_supported_width = 1600;
const transparent = 5;

pub const Compression = enum(u8) {
    none = 0,
    rle = 1,
};

pub const Awiz = struct {
    pub const max_blocks = 5;

    blocks: std.BoundedArray(Block, max_blocks) = .{},

    pub fn deinit(self: *Awiz, allocator: std.mem.Allocator) void {
        for (self.blocks.slice()) |*block| switch (block.*) {
            .rgbs, .two_ints, .wizh, .trns => {},
            .wizd => |*wizd| wizd.bmp.deinit(allocator),
        };
    }
};

const Block = union(enum) {
    rgbs,
    two_ints: struct { id: BlockId, ints: [2]i32 },
    wizh,
    trns: i32,
    wizd: struct {
        compression: Compression,
        /// A raw BMP file
        bmp: std.ArrayListUnmanaged(u8),
    },
};

pub fn decode(
    allocator: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    awiz_raw: []const u8,
    default_palette: *const [0x300]u8,
) error{AddedToDiagnostic}!Awiz {
    var stream = std.io.fixedBufferStream(awiz_raw);
    return decodeInner(allocator, &stream, default_palette) catch |err| {
        diag.zigErr(@intCast(stream.pos), "general decode failure: {s}", .{}, err);
        return error.AddedToDiagnostic;
    };
}

fn decodeInner(
    allocator: std.mem.Allocator,
    reader: anytype,
    default_palette: *const [0x300]u8,
) !Awiz {
    var result: Awiz = .{};
    var rgbs_opt: ?*const [0x300]u8 = null;
    var wizh_opt: ?struct {
        compression: Compression,
        width: u31,
        height: u31,
    } = null;

    var awiz_blocks = oldFixedBlockReader(reader);

    while (try awiz_blocks.peek() != .WIZD) {
        const id, const len = try awiz_blocks.next();
        switch (id) {
            .RGBS => {
                const expected_rgbs_len = 0x300;
                if (len != expected_rgbs_len)
                    return error.BadData;
                rgbs_opt = try io.readInPlaceBytes(reader, expected_rgbs_len);

                try result.blocks.append(.rgbs);
            },
            .CNVS, .SPOT, .RELO => {
                if (len != 8)
                    return error.BadData;
                const int1 = try reader.reader().readInt(i32, .little);
                const int2 = try reader.reader().readInt(i32, .little);

                try result.blocks.append(.{
                    .two_ints = .{
                        .id = id,
                        .ints = .{ int1, int2 },
                    },
                });
            },
            .WIZH => {
                if (len != 12)
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

                try result.blocks.append(.wizh);
            },
            .TRNS => {
                if (len != 4)
                    return error.BadData;
                const trns = try reader.reader().readInt(i32, .little);
                try result.blocks.append(.{ .trns = trns });
            },
            else => return error.BadData,
        }
    }

    const wizd_len = try awiz_blocks.assume(.WIZD);
    const wizd_end = reader.pos + wizd_len;

    const wizh = wizh_opt orelse return error.BadData;
    const width = wizh.width;
    const height = wizh.height;

    if (width > max_supported_width)
        return error.BadData;

    const bmp_file_size = bmp.calcFileSize(width, height);
    var bmp_buf: std.ArrayListUnmanaged(u8) = try .initCapacity(allocator, bmp_file_size);
    errdefer bmp_buf.deinit(allocator);

    const bmp_writer = bmp_buf.writer(allocator);

    try bmp.writeHeader(bmp_writer, width, height, bmp_file_size);

    try bmp.writePalette(bmp_writer, rgbs_opt orelse default_palette);

    switch (wizh.compression) {
        .none => try decodeUncompressed(width, height, reader, &bmp_buf),
        .rle => try decodeRle(width, height, reader, &bmp_buf),
    }

    // Allow one byte of padding from the encoder
    if (reader.pos < wizd_end)
        _ = try reader.reader().readByte();

    try result.blocks.append(.{
        .wizd = .{
            .compression = wizh.compression,
            .bmp = bmp_buf,
        },
    });

    try awiz_blocks.finishEof();

    return result;
}

fn decodeUncompressed(
    width: u31,
    height: u31,
    reader: anytype,
    bmp_buf: *std.ArrayListUnmanaged(u8),
) !void {
    for (0..height) |_| {
        const row = try io.readInPlace(reader, width);
        try bmp_buf.appendSlice(utils.null_allocator, row);

        try bmp.padRow(bmp_buf.writer(utils.null_allocator), width);
    }
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

pub fn extractChildren(
    gpa: std.mem.Allocator,
    output_dir: std.fs.Dir,
    output_path: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    decoded: *const Awiz,
    bmp_path: [*:0]const u8,
    indent: u8,
) !void {
    for (decoded.blocks.slice()) |block| {
        try code.appendNTimes(gpa, ' ', indent);
        switch (block) {
            .rgbs => try code.appendSlice(gpa, "    rgbs\n"),
            .two_ints => |ti| {
                try code.writer(gpa).print(
                    "two-ints {} {} {}\n",
                    .{ ti.id, ti.ints[0], ti.ints[1] },
                );
            },
            .wizh => try code.appendSlice(gpa, "wizh\n"),
            .trns => return error.BadData, // TODO: unused?
            .wizd => |wizd| {
                try fs.writeFileZ(output_dir, bmp_path, wizd.bmp.items);
                try code.writer(gpa).print(
                    "bmp {} \"{s}/{s}\"\n",
                    .{ @intFromEnum(wizd.compression), output_path, bmp_path },
                );
            },
        }
    }
}

pub const EncodingStrategy = enum { original, max };

pub fn encode(
    gpa: std.mem.Allocator,
    wiz: *const Awiz,
    strategy: EncodingStrategy,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    // First find the bitmap block so we can preload all data before we start

    const wizd = for (wiz.blocks.slice()) |block| {
        if (block == .wizd)
            break block.wizd;
    } else return error.BadData;

    const header = try bmp.readHeader(wizd.bmp.items, .{});

    // Now write the blocks in the requested order

    for (wiz.blocks.slice()) |block| switch (block) {
        .rgbs => {
            const fixup = try beginBlockAl(gpa, out, .RGBS);
            try writeRgbs(gpa, header, out);
            try endBlockAl(out, fixup);
        },
        .two_ints => |b| {
            const fixup = try beginBlockAl(gpa, out, b.id);
            try out.writer(gpa).writeInt(i32, b.ints[0], .little);
            try out.writer(gpa).writeInt(i32, b.ints[1], .little);
            try endBlockAl(out, fixup);
        },
        .wizh => {
            const fixup = try beginBlockAl(gpa, out, .WIZH);
            try out.writer(gpa).writeInt(i32, @intFromEnum(wizd.compression), .little);
            try out.writer(gpa).writeInt(i32, header.width(), .little);
            try out.writer(gpa).writeInt(i32, header.height(), .little);
            try endBlockAl(out, fixup);
        },
        .trns => |trns| {
            const fixup = try beginBlockAl(gpa, out, .TRNS);
            try out.writer(gpa).writeInt(i32, trns, .little);
            try endBlockAl(out, fixup);
        },
        .wizd => |_| {
            const fixup = try beginBlockAl(gpa, out, .WIZD);
            switch (wizd.compression) {
                .none => try encodeUncompressed(header, out.writer(gpa)),
                .rle => try encodeRle(header, strategy, out.writer(gpa)),
            }
            // Pad output to a multiple of 2 bytes
            if ((out.items.len - fixup) & 1 != 0)
                try out.append(gpa, 0);
            try endBlockAl(out, fixup);
        },
    };
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

fn writeRgbs(gpa: std.mem.Allocator, header: bmp.Bmp, out: *std.ArrayListUnmanaged(u8)) !void {
    for (header.palette) |color| {
        try out.append(gpa, color.rgbRed);
        try out.append(gpa, color.rgbGreen);
        try out.append(gpa, color.rgbBlue);
    }
}

pub fn encodeUncompressed(header: bmp.Bmp, out: anytype) !void {
    var rows = try header.iterRows();
    while (rows.next()) |row|
        try out.writeAll(row);
}

pub fn encodeRle(header: bmp.Bmp, strategy: EncodingStrategy, out: anytype) !void {
    var rows = try header.iterRows();
    while (rows.next()) |row| {
        // worst-case encoding is 2 bytes for the line size, then 2 output bytes
        // for every input byte
        var line_buf: std.BoundedArray(u8, 2 + max_supported_width * 2) = .{};

        // reserve space for line size, to be filled in later
        line_buf.len = 2;

        switch (strategy) {
            .original => try encodeRleRowOriginal(row, &line_buf),
            .max => try encodeRleRowMax(row, &line_buf),
        }

        // fill in line size
        std.mem.writeInt(i16, line_buf.buffer[0..2], @intCast(line_buf.len - 2), .little);

        // flush line to output stream
        try out.writeAll(line_buf.slice());
    }
}

fn encodeRleRowMax(
    row: []const u8,
    line_buf: *std.BoundedArray(u8, 2 + max_supported_width * 2),
) !void {
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
            try line_buf.append(n);
        } else if (run_len != 1) {
            const n = 2 | @shlExact(run_len - 1, 2);
            try line_buf.append(n);
            try line_buf.append(color);
        } else {
            const n = @shlExact(blit_len - 1, 2);
            try line_buf.append(n);
            try line_buf.appendSlice(row[i - blit_len .. i]);
        }
    }
}

fn encodeRleRowOriginal(
    row: []const u8,
    line_buf: *std.BoundedArray(u8, 2 + max_supported_width * 2),
) !void {
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
                try line_buf.append(n);
            }
        } else if (run_len != 1) {
            var remaining = run_len;
            while (remaining != 0) {
                const cur: u8 = @min(remaining, 64);
                remaining -= cur;

                const n = 2 | @shlExact(cur - 1, 2);
                try line_buf.append(n);
                try line_buf.append(color);
            }
        } else {
            var remaining = blit_len;
            while (remaining != 0) {
                const cur: u8 = @min(remaining, 64);
                remaining -= cur;

                const n = @shlExact(cur - 1, 2);
                try line_buf.append(n);
                try line_buf.appendSlice(row[i - remaining - cur .. i - remaining]);
            }
        }
    }
}
