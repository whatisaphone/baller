const std = @import("std");

const bitmap_file_header_size = 14;
const bitmap_info_header_size = 40;
const num_colors = 256;
const row_align = 4;

pub fn calcFileSize(width: u31, height: u31) u32 {
    const stride = std.mem.alignForward(u31, width, row_align);
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
        // convert 24-bit colors to 32-bit
        try out.writeAll(pal[i .. i + 3]);
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
