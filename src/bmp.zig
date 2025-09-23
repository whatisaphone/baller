const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const utils = @import("utils.zig");

const magic = std.mem.bytesToValue(u16, "BM");
const num_colors = 256;
const row_align = 4;

const BI_RGB = 0;
const BI_BITFIELDS = 3;

const BITMAPFILEHEADER = extern struct {
    bfType: u16,
    bfSize: u32 align(2),
    bfReserved1: u16,
    bfReserved2: u16,
    bfOffBits: u32 align(2),
};

pub const BITMAPINFOHEADER = extern struct {
    biSize: u32 align(2),
    biWidth: i32 align(2),
    biHeight: i32 align(2),
    biPlanes: u16,
    biBitCount: u16,
    biCompression: u32 align(2),
    biSizeImage: u32 align(2),
    biXPelsPerMeter: i32 align(2),
    biYPelsPerMeter: i32 align(2),
    biClrUsed: u32 align(2),
    biClrImportant: u32 align(2),
};

pub const BITMAPINFOHEADERV4 = extern struct {
    bV4Size: u32 align(2),
    bV4Width: i32 align(2),
    bV4Height: i32 align(2),
    bV4Planes: u16,
    bV4BitCount: u16,
    bV4V4Compression: u32 align(2),
    bV4SizeImage: u32 align(2),
    bV4XPelsPerMeter: i32 align(2),
    bV4YPelsPerMeter: i32 align(2),
    bV4ClrUsed: u32 align(2),
    bV4ClrImportant: u32 align(2),
    bV4RedMask: u32 align(2),
    bV4GreenMask: u32 align(2),
    bV4BlueMask: u32 align(2),
    bV4AlphaMask: u32 align(2),
    bV4CSType: u32 align(2),
    bV4Endpoints: [3]i32 align(2),
    bV4GammaRed: u32 align(2),
    bV4GammaGreen: u32 align(2),
    bV4GammaBlue: u32 align(2),
};

const RGBQUAD = extern struct {
    rgbBlue: u8,
    rgbGreen: u8,
    rgbRed: u8,
    rgbReserved: u8,
};

pub const HeaderError = union(enum) {
    not_bmp,
    wrong_bit_count: struct { expected: u16, actual: u16 },
    compressed,
    wrong_palette_size: u32,
    other,

    fn set(self: *HeaderError, err: HeaderError) error{BmpError} {
        self.* = err;
        return error.BmpError;
    }

    pub fn addToDiag(
        self: HeaderError,
        diagnostic: *Diagnostic,
        loc: Diagnostic.Location,
    ) error{AddedToDiagnostic} {
        switch (self) {
            .not_bmp => {
                diagnostic.errAt(loc, "invalid bmp file", .{});
            },
            .wrong_bit_count => |e| {
                diagnostic.errAt(loc, "expected {}-bit bmp, found {}-bit", .{ e.expected, e.actual });
            },
            .compressed => {
                diagnostic.errAt(loc, "expected uncompressed bmp, found compressed", .{});
            },
            .wrong_palette_size => |size| {
                diagnostic.errAt(loc, "expected palette with 256 colors, found {} colors", .{size});
            },
            .other => {
                diagnostic.errAt(loc, "error reading bmp", .{});
            },
        }
        return error.AddedToDiagnostic;
    }
};

pub fn readHeader(bmp: []u8, err: *HeaderError) error{BmpError}!Bmp {
    if (bmp.len < @sizeOf(BITMAPFILEHEADER) + @sizeOf(BITMAPINFOHEADER))
        return err.set(.not_bmp);

    const file_header = std.mem.bytesAsValue(BITMAPFILEHEADER, bmp);
    if (file_header.bfType != magic)
        return err.set(.not_bmp);

    const info_header_size = std.mem.readInt(u32, bmp[@sizeOf(BITMAPFILEHEADER)..][0..4], .little);
    if (info_header_size < @sizeOf(BITMAPINFOHEADER))
        return err.set(.not_bmp);
    const info_header = std.mem.bytesAsValue(BITMAPINFOHEADER, bmp[@sizeOf(BITMAPFILEHEADER)..]);

    // Make sure width/height fit in 31 bits
    _ = std.math.cast(u31, info_header.biWidth) orelse
        return err.set(.other);
    _ = std.math.cast(u31, @abs(info_header.biHeight)) orelse
        return err.set(.other);

    if (info_header.biPlanes != 1)
        return err.set(.other);

    return .{ .raw = bmp };
}

pub const Bmp = struct {
    raw: []u8,

    fn fileHeader(self: Bmp) *align(1) const BITMAPFILEHEADER {
        return std.mem.bytesAsValue(BITMAPFILEHEADER, self.raw);
    }

    fn infoHeader(self: Bmp) *align(1) const BITMAPINFOHEADER {
        return std.mem.bytesAsValue(BITMAPINFOHEADER, self.raw[@sizeOf(BITMAPFILEHEADER)..]);
    }

    fn infoHeaderV4(self: Bmp) *align(1) const BITMAPINFOHEADERV4 {
        std.debug.assert(self.infoHeader().biSize >= @sizeOf(BITMAPINFOHEADERV4));
        return @ptrCast(self.infoHeader());
    }

    pub fn width(self: Bmp) u31 {
        return @intCast(self.infoHeader().biWidth);
    }

    pub fn height(self: Bmp) u31 {
        return @intCast(@abs(self.infoHeader().biHeight));
    }

    pub fn as8Bit(self: Bmp, err: *HeaderError) error{BmpError}!Bmp8 {
        const info_header = self.infoHeader();

        if (info_header.biBitCount != 8)
            return err.set(.{ .wrong_bit_count = .{ .expected = 8, .actual = info_header.biBitCount } });
        if (info_header.biCompression != BI_RGB)
            return err.set(.compressed);
        if (!(info_header.biClrUsed == 0 or info_header.biClrUsed == 256))
            return err.set(.{ .wrong_palette_size = info_header.biClrUsed });

        const w = self.width();
        const h = self.height();
        const stride = calcStride(w);

        const palette_ptr = try self.getPalette(num_colors, err);
        const palette = palette_ptr.array(num_colors);

        const pixels = try self.getPixels(stride, err);
        self.flipToTopDown(pixels, stride);

        return .{
            .width = w,
            .height = h,
            .palette = palette,
            .pixels = pixels,
        };
    }

    pub fn as15Bit(self: *const Bmp, err: *HeaderError) error{BmpError}!Bmp15 {
        const info_header = self.infoHeader();

        if (info_header.biBitCount != 16)
            return err.set(.{ .wrong_bit_count = .{ .expected = 15, .actual = info_header.biBitCount } });

        switch (info_header.biCompression) {
            BI_RGB => {},
            BI_BITFIELDS => {
                // just make sure the bitfields match standard 555
                const bitfields = try self.getBitfields(err);
                if (bitfields[0] != 0x7c00 or
                    bitfields[1] != 0x03e0 or
                    bitfields[2] != 0x001f or
                    bitfields[3] != 0x0000)
                    // I'm not checking if it's 16-bit exactly, but this error
                    // should be close enough
                    return err.set(.{ .wrong_bit_count = .{ .expected = 15, .actual = 16 } });
            },
            else => return err.set(.compressed),
        }

        const w = self.width();
        const h = self.height();
        const stride = calcStride15(w);

        const pixels = try self.getPixels(stride, err);
        self.flipToTopDown(pixels, stride);

        return .{
            .width = w,
            .height = h,
            .pixels = pixels,
        };
    }

    fn getBitfields(self: Bmp, err: *HeaderError) ![4]u32 {
        const header = self.infoHeader();
        std.debug.assert(header.biCompression == BI_BITFIELDS);
        if (header.biSize < @sizeOf(BITMAPINFOHEADERV4)) {
            const bfs_ptr = try self.getPalette(3, err);
            const bfs: *align(1) const [3]u32 = @ptrCast(bfs_ptr.array(3));
            return .{ bfs[0], bfs[1], bfs[2], 0 };
        } else {
            const h4 = self.infoHeaderV4();
            return .{ h4.bV4RedMask, h4.bV4GreenMask, h4.bV4BlueMask, h4.bV4AlphaMask };
        }
    }

    fn getPalette(self: Bmp, len: usize, err: *HeaderError) !utils.SafeManyPointer([*]const RGBQUAD) {
        const start = @sizeOf(BITMAPFILEHEADER) + self.infoHeader().biSize;
        const size = len * @sizeOf(RGBQUAD);
        if (start + size > self.raw.len)
            return err.set(.other);
        return .init(@ptrCast(self.raw[start..][0..size]));
    }

    fn getPixels(self: Bmp, stride: u31, err: *HeaderError) ![]u8 {
        const file_header = self.fileHeader();
        if (file_header.bfOffBits > self.raw.len)
            return err.set(.other);
        const pixels = self.raw[file_header.bfOffBits..];
        if (pixels.len != stride * self.height())
            return err.set(.other);
        return pixels;
    }

    /// If the bmp is bottom-up, flip it so it's top-down
    fn flipToTopDown(self: Bmp, pixels: []u8, stride: u31) void {
        const bottom_up = self.infoHeader().biHeight > 0;
        if (!bottom_up) return;

        const h: u31 = self.height();
        for (0..h / 2) |y| {
            const row_a = pixels[y * stride ..][0..stride];
            const flipped_y = h - 1 - y;
            const row_b = pixels[flipped_y * stride ..][0..stride];
            for (row_a, row_b) |*a, *b|
                std.mem.swap(u8, a, b);
        }
    }
};

pub const Bmp8 = struct {
    width: u31,
    height: u31,
    palette: *const [num_colors]RGBQUAD,
    pixels: []const u8,

    pub fn iterRows(self: *const Bmp8) RowIter {
        return .init(self.width, self.pixels);
    }

    pub fn getPixel(self: *const Bmp8, x: usize, y: usize) u8 {
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

    // TODO: this doesn't belong here
    fn init15(width: u31, pixels: []const u8) RowIter {
        return .{
            .pixels = pixels,
            .pos = 0,
            .width = width * @sizeOf(u16),
            .stride = calcStride15(width),
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
    return @sizeOf(BITMAPFILEHEADER) + @sizeOf(BITMAPINFOHEADER) + 4 * num_colors;
}

pub const Bmp15 = struct {
    width: u31,
    height: u31,
    pixels: []const u8,

    pub fn iterRows(self: *const Bmp15) RowIter {
        return .init15(self.width, self.pixels);
    }
};

pub fn calcStride15(width: u31) u31 {
    const row_size = width * @sizeOf(u16);
    return std.mem.alignForward(u31, row_size, row_align);
}

pub fn calcFileSize15(width: u31, height: u31) u32 {
    const stride = calcStride15(width);
    return calcDataStart15() + stride * height;
}

fn calcDataStart15() u32 {
    return @sizeOf(BITMAPFILEHEADER) + @sizeOf(BITMAPINFOHEADER);
}

pub fn writeHeader(out: anytype, width: u31, height: u31, file_size: u32) !void {
    // BITMAPFILEHEADER
    try out.writeAll("BM");
    try out.writeInt(u32, file_size, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u32, calcDataStart(), .little);

    // BITMAPINFOHEADER
    try out.writeInt(u32, @sizeOf(BITMAPINFOHEADER), .little);
    try out.writeInt(i32, width, .little);
    try out.writeInt(i32, -@as(i32, height), .little);
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u16, 8, .little); // bits
    try out.writeInt(u32, 0, .little); // BI_RGB
    try out.writeInt(u32, 0, .little);
    try out.writeInt(i32, 0, .little);
    try out.writeInt(i32, 0, .little);
    try out.writeInt(u32, 0, .little);
    try out.writeInt(u32, 0, .little);
}

pub fn writeHeader15(out: anytype, width: u31, height: u31, file_size: u32) !void {
    // BITMAPFILEHEADER
    try out.writeAll("BM");
    try out.writeInt(u32, file_size, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u16, 0, .little);
    try out.writeInt(u32, calcDataStart15(), .little);

    // BITMAPINFOHEADER
    try out.writeInt(u32, @sizeOf(BITMAPINFOHEADER), .little);
    try out.writeInt(i32, width, .little);
    try out.writeInt(i32, -@as(i32, height), .little);
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u16, 16, .little); // bits
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

pub fn padRow(out: anytype, row_size: u31) !void {
    // Per the BMP spec, align each row to a multiple of 4 bytes
    const mask = row_align - 1;
    const bytes = mask - ((row_size + mask) & mask);
    for (0..bytes) |_|
        try out.writeByte(0);
}

pub fn padRow15(out: anytype, width: u31) !void {
    const row_size = width * @sizeOf(u16);
    return padRow(out, row_size);
}
