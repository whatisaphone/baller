const std = @import("std");

const bitmap_file_header_size = 14;
const bitmap_info_header_size = 40;
const magic = std.mem.bytesToValue(u16, "BM");
const num_colors = 256;
const row_align = 4;

const BI_RGB = 0;

const BITMAPFILEHEADER = packed struct {
    bfType: u16,
    bfSize: u32,
    bfReserved1: u16,
    bfReserved2: u16,
    bfOffBits: u32,
};

pub const BITMAPINFOHEADER = packed struct {
    biSize: u32,
    biWidth: i32,
    biHeight: i32,
    biPlanes: u16,
    biBitCount: u16,
    biCompression: u32,
    biSizeImage: u32,
    biXPelsPerMeter: i32,
    biYPelsPerMeter: i32,
    biClrUsed: u32,
    biClrImportant: u32,
};

pub fn readHeader(bmp: []const u8) !struct { *align(1) const BITMAPINFOHEADER, []const u8 } {
    if (bmp.len < bitmap_file_header_size + bitmap_info_header_size)
        return error.BadData;

    const file_header = std.mem.bytesAsValue(BITMAPFILEHEADER, bmp);
    if (file_header.bfType != magic)
        return error.BadData;

    const info_header = std.mem.bytesAsValue(BITMAPINFOHEADER, bmp[bitmap_file_header_size..]);

    if (info_header.biSize != bitmap_info_header_size or
        info_header.biBitCount != 8 or
        info_header.biCompression != BI_RGB or
        !(info_header.biClrUsed == 0 or info_header.biClrUsed == 256))
        return error.BadData;

    // Other code assumes width and height fit in 31 bits
    const width = std.math.cast(u31, info_header.biWidth) orelse
        return error.BadData;
    const height: u31 = std.math.cast(u31, @abs(info_header.biHeight)) orelse
        return error.BadData;
    const stride = calcStride(width);

    if (file_header.bfOffBits > bmp.len)
        return error.BadData;

    const pixels = bmp[file_header.bfOffBits..];
    // For RMIM to round-trip, we need to allow extra trailing bytes. So it must
    // be ok for the data to be too long, but at least check it's not too short.
    if (pixels.len < stride * height)
        return error.BadData;

    return .{ info_header, pixels };
}

pub const PixelIter = union(enum) {
    top_down: struct {
        pixels: []const u8,
        i: u32,
    },
    bottom_up: struct {
        pixels: []const u8,
        i: u32,
        width: u32,
        x: u32,
    },

    pub fn init(header: *align(1) const BITMAPINFOHEADER, pixels: []const u8) PixelIter {
        const width: u32 = @intCast(header.biWidth);
        const top_down = header.biHeight < 0;

        if (top_down)
            return .{ .top_down = .{
                .pixels = pixels,
                .i = 0,
            } };

        const i: u32 = @intCast(pixels.len - 1);
        return .{ .bottom_up = .{
            .pixels = pixels,
            .width = width,
            .i = i,
            .x = width - i % width,
        } };
    }

    pub fn next(self: *PixelIter) ?u8 {
        switch (self.*) {
            .top_down => |*s| {
                const result = s.pixels[s.i];

                s.i += 1;
                if (s.i >= s.pixels.len)
                    return null;

                return result;
            },
            .bottom_up => |*s| {
                const result = s.pixels[s.i];

                s.i += 1;
                s.x -= 1;
                if (s.x == 0) {
                    const feed = s.width * 2;
                    if (s.i < feed)
                        return null;
                    s.i -= feed;
                    s.x = s.width;
                }

                return result;
            },
        }
    }
};

pub const RowIter = struct {
    pixels: []const u8,
    pos: u32,
    width: u31,
    stride: i32,

    pub fn init(header: *align(1) const BITMAPINFOHEADER, pixels: []const u8) !RowIter {
        const width: u31 = @intCast(header.biWidth);
        const stride = calcStride(width);

        // Hopefully i never need to handle this
        if (pixels.len % stride != 0)
            return error.BadData;

        const top_down = header.biHeight < 0;
        if (top_down)
            return .{
                .pixels = pixels,
                .pos = 0,
                .width = width,
                .stride = stride,
            };

        return .{
            .pixels = pixels,
            .pos = @as(u32, @intCast(pixels.len)) - stride,
            .width = width,
            .stride = -@as(i32, stride),
        };
    }

    pub fn next(self: *RowIter) ?[]const u8 {
        const result = self.pixels[self.pos..][0..self.width];

        if (self.stride > 0) {
            self.pos += @intCast(self.stride);
            if (self.pos >= self.pixels.len)
                return null;
        } else {
            self.pos = std.math.sub(u32, self.pos, @intCast(-self.stride)) catch
                return null;
        }

        return result;
    }
};

fn calcStride(width: u31) u31 {
    return std.mem.alignForward(u31, width, row_align);
}

pub fn calcFileSize(width: u31, height: u31) u32 {
    const stride = calcStride(width);
    return calcDataStart() + stride * height;
}

fn calcDataStart() u32 {
    return bitmap_file_header_size + bitmap_info_header_size + 4 * num_colors;
}

pub fn writeHeader(out: anytype, width: u31, height: u31, file_size: u32) !void {
    // BITMAPFILEHEADER
    try out.writeAll("BM");
    try out.writeInt(u32, file_size, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u32, calcDataStart(), .little);

    // BITMAPINFOHEADER
    try out.writeInt(u32, bitmap_info_header_size, .little);
    try out.writeInt(i32, width, .little);
    try out.writeInt(i32, -@as(i32, height), .little);
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u16, 8, .little);
    try out.writeInt(u32, 0, .little); // BI_RGB
    try out.writeInt(u32, 0, .little);
    try out.writeInt(i32, 0, .little);
    try out.writeInt(i32, 0, .little);
    try out.writeInt(u32, 0, .little);
    try out.writeInt(u32, 0, .little);
}

pub fn writePalette(out: anytype, pal: *const [0x300]u8) !void {
    var i: usize = 0;
    while (i < 0x300) {
        // convert from RGB to BGR0
        try out.writeByte(pal[i + 2]);
        try out.writeByte(pal[i + 1]);
        try out.writeByte(pal[i]);
        try out.writeByte(0);
        i += 3;
    }
}

pub fn padRow(out: anytype, width: u31) !void {
    // Per the BMP spec, align each row to a multiple of 4 bytes
    const mask = row_align - 1;
    const bytes = mask - ((width + mask) & mask);
    for (0..bytes) |_|
        try out.writeByte(0);
}
