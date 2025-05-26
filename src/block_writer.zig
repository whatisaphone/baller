const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;

pub fn beginBlock(stream: anytype, id: BlockId) !u32 {
    const block_start: u32 = @intCast(stream.bytes_written);

    try stream.writer().writeInt(BlockId.Raw, id.raw(), .little);
    // Write the length as a placeholder to be filled in later
    try stream.writer().writeAll(&@as([4]u8, undefined));

    return block_start;
}

pub fn endBlock(stream: anytype, fixups: *std.ArrayList(Fixup), block_start: u32) !void {
    const stream_pos: u32 = @intCast(stream.bytes_written);
    try fixups.append(.{
        .offset = block_start + 4,
        .bytes = Fixup.encode(stream_pos - block_start, .big),
    });
}

pub const Fixup = struct {
    offset: u32,
    bytes: [4]u8,

    pub fn encode(bytes: u32, comptime endian: std.builtin.Endian) [4]u8 {
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, bytes, endian);
        return buf;
    }
};

pub fn applyFixups(out: []u8, fixups: []const Fixup) void {
    for (fixups) |fixup|
        @memcpy(out[fixup.offset..][0..4], &fixup.bytes);
}

pub fn writeFixups(file: std.fs.File, writer: anytype, fixups: []const Fixup) !void {
    for (fixups) |fixup| {
        try file.seekTo(fixup.offset);
        try writer.writeAll(&fixup.bytes);
    }
}
