const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const Block = @import("block_reader.zig").Block;
const FixedBlockReader = @import("block_reader.zig").FixedBlockReader;
const bmp = @import("bmp.zig");
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const Compression = @import("rmim.zig").Compression;
const utils = @import("utils.zig");

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    palette: *const [0x300]u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    var in: std.io.Reader = .fixed(raw);
    var blocks: FixedBlockReader = .init(&in, diag);

    try code.appendSlice(gpa, "obim {\n");

    const imhd_raw = try blocks.expect(.IMHD).bytes();
    if (imhd_raw.len < @sizeOf(Imhd)) return error.BadData;
    const imhd = std.mem.bytesAsValue(Imhd, imhd_raw[0..@sizeOf(Imhd)]);
    try writeRawBlock(gpa, .IMHD, imhd_raw, out_dir, out_path, 4, .{ .object = imhd.object_number }, code);

    var im_index: u8 = 0;
    while (!blocks.atEnd()) : (im_index += 1) {
        const im_number = im_index + 1;

        const im_block_id = makeImBlockId(im_index) orelse return error.BadData;
        var im_blocks = try blocks.expect(im_block_id).nested();

        try code.appendSlice(gpa, "    im {\n");

        const smap_raw = try im_blocks.expect(.SMAP).bytes();
        try decodeSmap(gpa, imhd, im_number, smap_raw, palette, code, out_dir, out_path);

        if (!im_blocks.atEnd()) {
            const zp01 = try im_blocks.expect(.ZP01).bytes();
            try writeRawBlock(gpa, .ZP01, zp01, out_dir, out_path, 8, .{ .object_block = .{ imhd.object_number, im_block_id } }, code);
        }

        try im_blocks.finish();

        try code.appendSlice(gpa, "    }\n");
    }

    try blocks.finish();

    try code.appendSlice(gpa, "}\n");
}

pub fn makeImBlockId(index: usize) ?BlockId {
    const chars = "123456789AB";

    if (index >= chars.len) return null;

    var result = BlockId.IM00;
    std.mem.asBytes(&result)[3] = chars[index];
    return result;
}

const Imhd = extern struct {
    object_number: u16,
    unk_02: u16,
    unk_04: u16,
    unk_06: u16,
    unk_08: u16,
    unk_0a: u16,
    width: u16,
    height: u16,
    // TODO: fill in the rest
};

// based on ScummVM's drawBitmap
fn decodeSmap(
    gpa: std.mem.Allocator,
    imhd: *align(1) const Imhd,
    im_number: u8,
    smap_raw: []const u8,
    palette: *const [0x300]u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    const bmp_size = bmp.calcFileSize(imhd.width, imhd.height);
    const bmp_raw = try gpa.alloc(u8, bmp_size);
    defer gpa.free(bmp_raw);

    var bmp_writer: std.io.Writer = .fixed(bmp_raw);
    try bmp.writeHeader(&bmp_writer, imhd.width, imhd.height, bmp_size);
    try bmp.writePalette(&bmp_writer, palette);
    const bmp_pixels = bmp_writer.unusedCapacitySlice();

    try decodeSmapData(imhd, smap_raw, bmp_pixels);

    var out_name_buf: ["object0000_00.bmp".len + 1]u8 = undefined;
    const out_name = std.fmt.bufPrintZ(&out_name_buf, "object{}_{:0>2}.bmp", .{ imhd.object_number, im_number }) catch unreachable;
    try fs.writeFileZ(out_dir, out_name, bmp_raw);

    try code.print(gpa, "        smap \"{s}/{s}\" [", .{ out_path, out_name });
    try writeCompressionTypes(gpa, code, imhd, smap_raw);
    try code.appendSlice(gpa, "]\n");
}

const strip_width = 8;

fn decodeSmapData(imhd: *align(1) const Imhd, smap_raw: []const u8, out: []u8) !void {
    // The image is split into 8px wide vertical strips, each compressed separately.
    const num_strips = try std.math.divExact(u16, imhd.width, strip_width);

    // The block starts with a table of offsets pointing to compressed data for each strip.
    const strip_offsets_size = num_strips * @sizeOf(u32);
    if (strip_offsets_size > smap_raw.len) return error.BadData;
    const strip_offsets = std.mem.bytesAsSlice(u32, smap_raw[0..strip_offsets_size]);

    // We expect the strip data to be laid out in sequence with no overlaps or extra bytes.
    var next_strip_offset: u32 = Block.header_size + strip_offsets_size;
    for (0.., strip_offsets) |strip_index, strip_offset| {
        if (strip_offset != next_strip_offset) return error.BadData;
        next_strip_offset = if (strip_index != num_strips - 1)
            strip_offsets[strip_index + 1]
        else
            @intCast(Block.header_size + smap_raw.len);
        const strip_raw = smap_raw[strip_offset - Block.header_size .. next_strip_offset - Block.header_size];
        try decodeStrip(imhd, strip_raw, out[strip_index * strip_width ..]);
    }
}

fn writeCompressionTypes(
    gpa: std.mem.Allocator,
    code: *std.ArrayListUnmanaged(u8),
    imhd: *align(1) const Imhd,
    smap_raw: []const u8,
) !void {
    // We can skip validation because it already happened once during `decodeSmapData`
    const num_strips = std.math.divExact(u16, imhd.width, strip_width) catch unreachable;
    const strip_offsets_size = num_strips * @sizeOf(u32);
    const strip_offsets = std.mem.bytesAsSlice(u32, smap_raw[0..strip_offsets_size]);
    for (0.., strip_offsets) |strip_index, strip_offset| {
        if (strip_index != 0)
            try code.append(gpa, ' ');
        const compression = smap_raw[strip_offset - Block.header_size];
        try code.print(gpa, "{}", .{compression});
    }
}

fn decodeStrip(imhd: *align(1) const Imhd, data: []const u8, out: []u8) !void {
    if (data.len == 0) return error.BadData;
    const compression = data[0];
    const compressed_data = data[1..];

    switch (compression) {
        Compression.BMCOMP_ZIGZAG_VT8,
        => try decodeZigZagV(imhd.width, imhd.height, compression, compressed_data, out),
        Compression.BMCOMP_RMAJMIN_HT4,
        Compression.BMCOMP_RMAJMIN_HT8,
        => try decodeRMajMin(imhd.width, imhd.height, compression, compressed_data, out),
        else => return error.BadData,
    }
}

// based on ScummVM's drawStripBasicV
fn decodeZigZagV(
    width: u16,
    height: u16,
    compression: u8,
    data: []const u8,
    out: []u8,
) !void {
    var stream: std.io.Reader = .fixed(data);
    var in: io.BitReader = .init(&stream);

    const shift = compression % 10;
    var color = try stream.takeByte();
    var inc: i2 = -1;

    for (0..8) |x| {
        var pos = x;
        for (0..height) |_| {
            out[pos] = color;
            pos += width; // move down 1px

            if (try in.takeBits(u1, 1) == 0) {
                // color stays the same
            } else if (try in.takeBits(u1, 1) == 0) {
                color = try in.takeBits(u8, shift);
                inc = -1;
            } else if (try in.takeBits(u1, 1) == 0) {
                color = utils.add(u8, color, inc) orelse return error.BadData;
            } else {
                inc = -inc;
                color = utils.add(u8, color, inc) orelse return error.BadData;
            }
        }
    }

    if (stream.end != stream.buffer.len or in.buf_count != 0)
        return error.BadData;
}

// based on ScummVM's drawStripComplex
fn decodeRMajMin(
    width: u16,
    height: u16,
    compression: u8,
    data: []const u8,
    out: []u8,
) !void {
    // HACK: Unless I made a mistake, the original decoder reads 1 byte past the
    // end sometimes. Just to get it working for now, let's copy the data into a
    // buffer that's bigger by 1 byte so we can decode without triggering UB.
    var hack: utils.TinyArray(u8, 1024) = .empty;
    try hack.appendSlice(data);
    try hack.append(0); // This is the pesky oob byte
    var stream: std.io.Reader = .fixed(hack.slice());

    const shift = compression % 10;
    var maj_min: MajMinCodec = try .init(&stream, shift);

    var pos: u32 = 0;
    for (0..height) |_| {
        try maj_min.decodeLine(out[pos..][0..8]);
        pos += width; // move down 1px
    }

    // Allow at most 2 unused bytes (not sure why)
    if (stream.end < stream.buffer.len - 2)
        return error.BadData;
}

const MajMinCodec = struct {
    in: io.BitReader,
    shift: u8,

    color: u8,
    repeat_mode: bool,
    repeat_count: u8,

    fn init(src: *std.io.Reader, shift: u8) !MajMinCodec {
        return .{
            .in = .init(src),
            .shift = shift,
            .color = try src.takeByte(),
            .repeat_mode = false,
            .repeat_count = undefined,
        };
    }

    fn decodeLine(self: *MajMinCodec, dest: *[strip_width]u8) !void {
        for (dest) |*pixel| {
            pixel.* = self.color;

            if (!self.repeat_mode) {
                if (try self.in.takeBits(u1, 1) == 0) {
                    // color stays the same
                } else if (try self.in.takeBits(u1, 1) != 0) {
                    const diff = try self.in.takeBits(i4, 3) - 4;
                    if (diff != 0) {
                        // A color change
                        self.color = utils.add(u8, self.color, diff) orelse return error.BadData;
                    } else {
                        // Color does not change, but rather identical pixels get repeated
                        self.repeat_mode = true;
                        self.repeat_count = try self.in.takeBits(u8, 8) - 1;
                    }
                } else {
                    self.color = try self.in.takeBits(u8, self.shift);
                }
            } else {
                self.repeat_count -= 1;
                if (self.repeat_count == 0) {
                    self.repeat_mode = false;
                }
            }
        }
    }
};

pub fn encodeSmap(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    target: games.Target,
    loc: Diagnostic.Location,
    bmp_raw: []u8,
    strip_compression: []const u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    var bmp_err: bmp.HeaderError = undefined;
    const bmp_header = bmp.readHeader(bmp_raw, &bmp_err) catch
        return bmp_err.addToDiag(diagnostic, loc);
    const bmp8 = bmp_header.as8Bit(&bmp_err) catch
        return bmp_err.addToDiag(diagnostic, loc);
    if (bmp8.width % strip_width != 0) {
        diagnostic.errAt(loc, "bitmap width must be a multiple of strip width", .{});
        return error.AddedToDiagnostic;
    }

    try out.ensureUnusedCapacity(gpa, 2 << 10);

    const starting_parity = out.items.len & 1;

    const strip_offset_origin = out.items.len - Block.header_size;

    const num_strips = bmp8.width / strip_width;
    const table_offset = out.items.len;
    _ = try out.addManyAsSlice(gpa, num_strips * @sizeOf(u32));

    if (strip_compression.len != num_strips) return error.BadData;

    for (0..num_strips) |strip_index| {
        const table_entry = out.items[table_offset + strip_index * 4 ..][0..4];
        const strip_offset: u32 = @intCast(out.items.len - strip_offset_origin);
        std.mem.writeInt(u32, table_entry, strip_offset, .little);

        const x = strip_index * strip_width;
        const comp = std.math.cast(u8, strip_compression[strip_index]) orelse return error.BadData;
        try encodeStrip(gpa, bmp8.width, bmp8.height, bmp8.pixels[x..], comp, out);
    }

    // Older versions pad the output to an even number of bytes
    if (target.le(.sputm99) and out.items.len & 1 != starting_parity)
        try out.append(gpa, 0);
}

fn encodeStrip(
    gpa: std.mem.Allocator,
    width: u31,
    height: u31,
    pixels: []const u8,
    compression: u8,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    try out.append(gpa, compression);
    switch (compression) {
        Compression.BMCOMP_ZIGZAG_VT8,
        => try encodeZigZagV(gpa, width, height, pixels, compression, out),
        Compression.BMCOMP_RMAJMIN_HT4,
        Compression.BMCOMP_RMAJMIN_HT8,
        => try encodeRMajMin(gpa, width, height, pixels, compression, out),
        else => return error.BadData,
    }
}

fn encodeZigZagV(
    gpa: std.mem.Allocator,
    stride: u31,
    height: u31,
    pixels: []const u8,
    compression: u8,
    out_al: *std.ArrayListUnmanaged(u8),
) !void {
    const bpp: u4 = @intCast(compression % 10);

    var out: std.io.Writer.Allocating = .fromArrayList(gpa, out_al);
    defer out_al.* = out.toArrayList();

    var encoder: ZigZagEncoder = undefined;
    for (0..strip_width) |x| {
        const y_start: usize = if (x == 0) blk: {
            encoder = try .begin(&out.writer, bpp, pixels[0]);
            break :blk 1;
        } else 0;
        for (y_start..height) |y|
            try encoder.write(pixels[x + y * stride]);
    }

    try encoder.finish();
}

const ZigZagEncoder = struct {
    out: io.BitWriter,
    bpp: u4,

    prev_color: u8,
    dir: i2,

    fn begin(out: *std.io.Writer, bpp: u4, initial_color: u8) !ZigZagEncoder {
        try out.writeByte(initial_color);
        return .{
            .out = .init(out),
            .bpp = bpp,
            .prev_color = initial_color,
            .dir = 1,
        };
    }

    fn write(self: *ZigZagEncoder, color: u8) !void {
        const diff = @as(i9, color) - self.prev_color;
        self.prev_color = color;

        if (diff == 0) {
            try self.out.writeBits(@as(u1, 0b0), 1);
        } else if (diff == self.dir) {
            try self.out.writeBits(@as(u3, 0b111), 3);
            self.dir = -self.dir;
        } else if (diff == -self.dir) {
            try self.out.writeBits(@as(u3, 0b011), 3);
        } else {
            try checkColorInRange(color, self.bpp);
            try self.out.writeBits(@as(u2, 0b01), 2);
            try self.out.writeBits(color, self.bpp);
            self.dir = 1;
        }
    }

    fn finish(self: *ZigZagEncoder) !void {
        try self.out.flushBits();
    }
};

fn encodeRMajMin(
    gpa: std.mem.Allocator,
    stride: u31,
    height: u31,
    pixels: []const u8,
    compression: u8,
    out_al: *std.ArrayListUnmanaged(u8),
) !void {
    const bpp: u4 = @intCast(compression % 10);

    var out: std.io.Writer.Allocating = .fromArrayList(gpa, out_al);
    defer out_al.* = out.toArrayList();

    var encoder: RMajMinEncoder = try .begin(&out.writer, bpp, pixels[0]);
    try encoder.write(pixels[1..strip_width]);

    var pos: usize = stride;
    for (1..height) |_| {
        try encoder.write(pixels[pos..][0..strip_width]);
        pos += stride;
    }
    try encoder.finish();
}

const RMajMinEncoder = struct {
    out: io.BitWriter,
    bpp: u4,

    prev_color: u8,
    repeat: u8,

    fn begin(writer: *std.io.Writer, bpp: u4, initial_color: u8) !RMajMinEncoder {
        try writer.writeByte(initial_color);
        return .{
            .out = .init(writer),
            .bpp = bpp,
            .prev_color = initial_color,
            .repeat = 0,
        };
    }

    fn write(self: *RMajMinEncoder, pixels: []const u8) !void {
        for (pixels) |color| {
            const diff = @as(i9, color) - self.prev_color;
            self.prev_color = color;

            if (diff == 0) {
                if (self.repeat == 0xff)
                    try self.flushRepeat();
                self.repeat += 1;
                continue;
            }

            try self.flushRepeat();

            if (-4 <= diff and diff < 4) {
                try self.out.writeBits(@as(u2, 0b11), 2);
                try self.out.writeBits(diff + 4, 3);
            } else {
                try checkColorInRange(color, self.bpp);
                try self.out.writeBits(@as(u2, 0b01), 2);
                try self.out.writeBits(color, self.bpp);
            }
        }
    }

    fn flushRepeat(self: *RMajMinEncoder) !void {
        if (self.repeat < 13) {
            try self.out.writeBits(@as(u16, 0), self.repeat);
        } else {
            try self.out.writeBits(@as(u5, 0b10011), 5);
            try self.out.writeBits(self.repeat, 8);
        }
        self.repeat = 0;
    }

    fn finish(self: *RMajMinEncoder) !void {
        try self.flushRepeat();
        try self.out.flushBits();
    }
};

fn checkColorInRange(color: u8, bpp: u4) !void {
    const palette_count = @shlExact(@as(u9, 1), bpp);
    if (color >= palette_count)
        return error.ColorOutOfRange;
}
