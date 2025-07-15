const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const Block = @import("block_reader.zig").Block;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const bmp = @import("bmp.zig");
const writeRawBlock = @import("extract.zig").writeRawBlock;
const fs = @import("fs.zig");
const io = @import("io.zig");
const Compression = @import("rmim.zig").Compression;
const utils = @import("utils.zig");

pub fn extract(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForBinaryFile,
    raw: []const u8,
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    var in = std.io.fixedBufferStream(raw);
    var blocks = fixedBlockReader(&in, diag);

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
        try writeRawBlock(gpa, .SMAP, smap_raw, out_dir, out_path, 8, .{ .object_block = .{ imhd.object_number, im_block_id } }, code);

        try decodeSmap(gpa, imhd, im_number, smap_raw, code, out_dir, out_path);

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
    code: *std.ArrayListUnmanaged(u8),
    out_dir: std.fs.Dir,
    out_path: []const u8,
) !void {
    const bmp_size = bmp.calcFileSize(imhd.width, imhd.height);
    const bmp_raw = try gpa.alloc(u8, bmp_size);
    defer gpa.free(bmp_raw);

    var bmp_writer = std.io.fixedBufferStream(bmp_raw);
    try bmp.writeHeader(bmp_writer.writer(), imhd.width, imhd.height, bmp_size);
    try bmp.writePlaceholderPalette(bmp_writer.writer());
    const bmp_pixels = bmp_raw[bmp_writer.pos..];

    try decodeSmapData(imhd, smap_raw, bmp_pixels);

    var out_name_buf: ["object0000_00.bmp".len + 1]u8 = undefined;
    const out_name = std.fmt.bufPrintZ(&out_name_buf, "object{}_{:0>2}.bmp", .{ imhd.object_number, im_number }) catch unreachable;
    try fs.writeFileZ(out_dir, out_name, bmp_raw);

    try code.writer(gpa).print("        ; smap \"{s}/{s}\"\n", .{ out_path, out_name });
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
    var stream = std.io.fixedBufferStream(data);
    var in = std.io.bitReader(.little, stream.reader());

    const shift = compression % 10;
    var color = try stream.reader().readByte();
    var inc: i2 = -1;

    for (0..8) |x| {
        var pos = x;
        for (0..height) |_| {
            out[pos] = color;
            pos += width; // move down 1px

            if (try in.readBitsNoEof(u1, 1) == 0) {
                // color stays the same
            } else if (try in.readBitsNoEof(u1, 1) == 0) {
                color = try in.readBitsNoEof(u8, shift);
                inc = -1;
            } else if (try in.readBitsNoEof(u1, 1) == 0) {
                color = utils.add(u8, color, inc) orelse return error.BadData;
            } else {
                inc = -inc;
                color = utils.add(u8, color, inc) orelse return error.BadData;
            }
        }
    }

    if (stream.pos != stream.buffer.len)
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
    var hack: std.BoundedArray(u8, 1024) = .{};
    try hack.appendSlice(data);
    try hack.append(0); // This is the pesky oob byte
    var stream = std.io.fixedBufferStream(hack.constSlice());

    const shift = compression % 10;
    var maj_min: MajMinCodec = try .init(stream.reader(), shift);

    var pos: u32 = 0;
    for (0..height) |_| {
        try maj_min.decodeLine(out[pos..][0..8]);
        pos += width; // move down 1px
    }

    // Allow at most 2 unused bytes (not sure why)
    if (stream.pos < stream.buffer.len - 2)
        return error.BadData;
}

const MajMinCodec = struct {
    in: std.io.BitReader(.little, std.io.FixedBufferStream([]const u8).Reader),
    shift: u8,

    color: u8,
    repeat_mode: bool,
    repeat_count: u8,

    fn init(src: std.io.FixedBufferStream([]const u8).Reader, shift: u8) !MajMinCodec {
        return .{
            .in = std.io.bitReader(.little, src),
            .shift = shift,
            .color = try src.readByte(),
            .repeat_mode = false,
            .repeat_count = undefined,
        };
    }

    fn decodeLine(self: *MajMinCodec, dest: *[strip_width]u8) !void {
        for (dest) |*pixel| {
            pixel.* = self.color;

            if (!self.repeat_mode) {
                if (try self.in.readBitsNoEof(u1, 1) != 0) {
                    if (try self.in.readBitsNoEof(u1, 1) != 0) {
                        const diff = try self.in.readBitsNoEof(i4, 3) - 4;
                        if (diff != 0) {
                            // A color change
                            self.color = utils.add(u8, self.color, diff) orelse return error.BadData;
                        } else {
                            // Color does not change, but rather identical pixels get repeated
                            self.repeat_mode = true;
                            self.repeat_count = try self.in.readBitsNoEof(u8, 8) - 1;
                        }
                    } else {
                        self.color = try self.in.readBitsNoEof(u8, self.shift);
                    }
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
