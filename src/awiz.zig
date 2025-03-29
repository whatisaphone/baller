const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const bmp = @import("bmp.zig");
const io = @import("io.zig");
const rmim = @import("rmim.zig");
const utils = @import("utils.zig");

const max_supported_width = 1600;
const transparent = 255;

pub const Compression = enum(u8) {
    none = 0,
    rle = 1,
};

pub const Awiz = struct {
    blocks: std.BoundedArray(Block, 5) = .{},

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
    awiz_raw: []const u8,
    // TODO: merge next two params
    rmda_raw: []const u8,
    defa_rgbs: ?*const [0x300]u8,
    options: struct { hack_skip_uncompressed: bool },
) !Awiz {
    var result: Awiz = .{};
    var rgbs_opt: ?*const [0x300]u8 = null;
    var wizh_opt: ?struct {
        compression: Compression,
        width: u31,
        height: u31,
    } = null;

    var reader = std.io.fixedBufferStream(awiz_raw);
    var awiz_blocks = fixedBlockReader(&reader);

    while (try awiz_blocks.peek() != blockId("WIZD")) {
        const id, const len = try awiz_blocks.next();
        switch (id) {
            blockId("RGBS") => {
                const expected_rgbs_len = 0x300;
                if (len != expected_rgbs_len)
                    return error.BadData;
                rgbs_opt = try io.readInPlaceBytes(&reader, expected_rgbs_len);

                try result.blocks.append(.rgbs);
            },
            blockId("CNVS"), blockId("SPOT"), blockId("RELO") => {
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
            blockId("WIZH") => {
                if (len != 12)
                    return error.BadData;
                const compression_int = try reader.reader().readInt(i32, .little);
                const compression = std.meta.intToEnum(Compression, compression_int) catch return error.BadData;
                const width_signed = try reader.reader().readInt(i32, .little);
                const width = std.math.cast(u31, width_signed) orelse return error.BadData;
                const height_signed = try reader.reader().readInt(i32, .little);
                const height = std.math.cast(u31, height_signed) orelse return error.BadData;

                if (options.hack_skip_uncompressed and compression == .none)
                    return error.BadData;

                wizh_opt = .{
                    .compression = compression,
                    .width = width,
                    .height = height,
                };

                try result.blocks.append(.wizh);
            },
            blockId("TRNS") => {
                if (len != 4)
                    return error.BadData;
                const trns = try reader.reader().readInt(i32, .little);
                try result.blocks.append(.{ .trns = trns });
            },
            else => return error.BadData,
        }
    }

    const wizd_len = try awiz_blocks.assumeBlock("WIZD");
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

    const palette = rgbs_opt orelse defa_rgbs orelse try rmim.findApalInRmda(rmda_raw);
    try bmp.writePalette(bmp_writer, palette);

    switch (wizh.compression) {
        .none => try decodeUncompressed(width, height, &reader, &bmp_buf),
        .rle => try decodeRle(width, height, &reader, &bmp_buf),
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
    const len = width * height;
    const data = try io.readInPlace(reader, len);
    try bmp_buf.appendSlice(utils.null_allocator, data);
}

// based on ScummVM's auxDecompTRLEPrim
pub fn decodeRle(
    width: u31,
    height: u31,
    reader: anytype,
    bmp_buf: *std.ArrayListUnmanaged(u8),
) !void {
    const bmp_writer = bmp_buf.writer(utils.null_allocator);

    for (0..height) |_| {
        const out_row_end = bmp_buf.items.len + width;

        const line_size = try reader.reader().readInt(u16, .little);
        const line_end = reader.pos + line_size;

        while (reader.pos < line_end) {
            const n = try reader.reader().readByte();
            if (n & 1 != 0) {
                const count = n >> 1;
                try bmp_writer.writeByteNTimes(transparent, count);
            } else if (n & 2 != 0) {
                const count = (n >> 2) + 1;
                const color = try reader.reader().readByte();
                try bmp_writer.writeByteNTimes(color, count);
            } else {
                const count = (n >> 2) + 1;
                try io.copy(std.io.limitedReader(reader.reader(), count), bmp_writer);
            }
        }

        try bmp_buf.appendNTimes(utils.null_allocator, transparent, out_row_end - bmp_buf.items.len);

        try bmp.padRow(bmp_writer, width);
    }
}

pub fn encode(wiz: *const Awiz, out: anytype, fixups: *std.ArrayList(Fixup)) !void {
    // First find the bitmap block so we can preload all data before we start

    const wizd = for (wiz.blocks.slice()) |block| {
        if (block == .wizd)
            break block.wizd;
    } else return error.BadData;

    const header = try bmp.readHeader(wizd.bmp.items, .{});

    // Now write the blocks in the requested order

    for (wiz.blocks.slice()) |block| switch (block) {
        .rgbs => {
            const fixup = try beginBlock(out, "RGBS");
            try writeRgbs(header, out);
            try endBlock(out, fixups, fixup);
        },
        .two_ints => |b| {
            const fixup = try beginBlockImpl(out, b.id);
            try out.writer().writeInt(i32, b.ints[0], .little);
            try out.writer().writeInt(i32, b.ints[1], .little);
            try endBlock(out, fixups, fixup);
        },
        .wizh => {
            const fixup = try beginBlock(out, "WIZH");
            try out.writer().writeInt(i32, @intFromEnum(wizd.compression), .little);
            try out.writer().writeInt(i32, header.width(), .little);
            try out.writer().writeInt(i32, header.height(), .little);
            try endBlock(out, fixups, fixup);
        },
        .trns => |trns| {
            const fixup = try beginBlock(out, "TRNS");
            try out.writer().writeInt(i32, trns, .little);
            try endBlock(out, fixups, fixup);
        },
        .wizd => |_| {
            const fixup = try beginBlock(out, "WIZD");
            switch (wizd.compression) {
                .none => try encodeUncompressed(header, out.writer()),
                .rle => try encodeRle(header, out.writer()),
            }
            // Pad output to a multiple of 2 bytes
            if ((out.bytes_written - fixup) & 1 != 0)
                try out.writer().writeByte(0);
            try endBlock(out, fixups, fixup);
        },
    };
}

fn writeRgbs(header: bmp.Bmp, out: anytype) !void {
    for (header.palette) |color| {
        try out.writer().writeByte(color.rgbRed);
        try out.writer().writeByte(color.rgbGreen);
        try out.writer().writeByte(color.rgbBlue);
    }
}

pub fn encodeUncompressed(header: bmp.Bmp, out: anytype) !void {
    var rows = try header.iterRows();
    while (rows.next()) |row|
        try out.writeAll(row);
}

pub fn encodeRle(header: bmp.Bmp, out: anytype) !void {
    var rows = try header.iterRows();
    while (rows.next()) |row| {
        // worst-case encoding is 2 bytes for the line size, then 2 output bytes
        // for every input byte
        var line_buf: std.BoundedArray(u8, 2 + max_supported_width * 2) = .{};

        // reserve space for line size, to be filled in later
        line_buf.len = 2;

        try encodeRleRow(row, &line_buf);

        // fill in line size
        std.mem.writeInt(i16, line_buf.buffer[0..2], @intCast(line_buf.len - 2), .little);

        // flush line to output stream
        try out.writeAll(line_buf.slice());
    }
}

fn encodeRleRow(
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
