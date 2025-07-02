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

const RGBQUAD = extern struct {
    rgbBlue: u8,
    rgbGreen: u8,
    rgbRed: u8,
    rgbReserved: u8,
};

pub fn readHeader(bmp: []u8) !Bmp {
    if (bmp.len < @sizeOf(BITMAPFILEHEADER) + @sizeOf(BITMAPINFOHEADER))
        return error.BadData;

    const file_header = std.mem.bytesAsValue(BITMAPFILEHEADER, bmp);
    if (file_header.bfType != magic)
        return error.BadData;

    const header_size = std.mem.readInt(u32, bmp[bitmap_file_header_size..][0..4], .little);
    if (header_size != 0x28 and // BITMAPINFOHEADER
        header_size != 0x6c and // BITMAPV4HEADER
        header_size != 0x7c) // BITMAPV5HEADER
        return error.BadData;
    // BITMAPINFOHEADER is a prefix of all the above header types
    const info_header = std.mem.bytesAsValue(BITMAPINFOHEADER, bmp[bitmap_file_header_size..]);

    if (info_header.biBitCount != 8 or
        info_header.biCompression != BI_RGB or
        !(info_header.biClrUsed == 0 or info_header.biClrUsed == 256))
        return error.BadData;

    // Bmp.width/height assumes these fit in 31 bits
    const width = std.math.cast(u31, info_header.biWidth) orelse
        return error.BadData;
    const height = std.math.cast(u31, @abs(info_header.biHeight)) orelse
        return error.BadData;
    const stride = calcStride(width);

    const palette_start = bitmap_file_header_size + info_header.biSize;
    const palette_size = num_colors * @sizeOf(RGBQUAD);
    if (palette_start + palette_size > bmp.len)
        return error.BadData;
    const palette: *const [num_colors]RGBQUAD = @ptrCast(bmp[palette_start..][0..palette_size]);

    if (file_header.bfOffBits > bmp.len)
        return error.BadData;
    const pixels = bmp[file_header.bfOffBits..];
    if (pixels.len != stride * height)
        return error.BadData;

    // If the bmp is bottom-up, flip it so it's top-down
    const bottom_up = info_header.biHeight > 0;
    if (bottom_up) {
        for (0..height / 2) |y| {
            const row_a = pixels[y * stride ..][0..width];
            const flipped_y = height - 1 - y;
            const row_b = pixels[flipped_y * stride ..][0..width];
            for (row_a, row_b) |*a, *b|
                std.mem.swap(u8, a, b);
        }
    }

    return .{
        .width = width,
        .height = height,
        .palette = palette,
        .pixels = pixels,
    };
}

pub const Bmp = struct {
    width: u31,
    height: u31,
    palette: *const [num_colors]RGBQUAD,
    pixels: []const u8,

    pub fn iterRows(self: *const Bmp) RowIter {
        return .init(self.width, self.pixels);
    }

    pub fn getPixel(self: *const Bmp, x: usize, y: usize) u8 {
        std.debug.assert(x < self.width);
        std.debug.assert(y < self.height);
        const stride = calcStride(self.width);
        return self.pixels[y * stride + x];
    }
};

pub const RowIter = struct {
    pixels: []const u8,
    pos: u32,
    width: u31,
    stride: u31,

    fn init(width: u31, pixels: []const u8) RowIter {
        return .{
            .pixels = pixels,
            .pos = 0,
            .width = width,
            .stride = calcStride(width),
        };
    }

    pub fn next(self: *RowIter) ?[]const u8 {
        const pos = self.pos;

        self.pos += self.stride;
        if (self.pos > self.pixels.len)
            return null;

        return self.pixels[pos..][0..self.width];
    }
};

pub fn calcStride(width: u31) u31 {
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

pub fn writePlaceholderPalette(out: anytype) !void {
    for (0..4) |b| for (0..8) |g| for (0..8) |r|
        try out.writeAll(&.{
            @intCast(255 * b / 3),
            @intCast(255 * g / 7),
            @intCast(255 * r / 7),
            0,
        });
}

pub fn padRow(out: anytype, width: u31) !void {
    // Per the BMP spec, align each row to a multiple of 4 bytes
    const mask = row_align - 1;
    const bytes = mask - ((width + mask) & mask);
    for (0..bytes) |_|
        try out.writeByte(0);
}
