const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;

pub fn beginBlock(stream: anytype, id: BlockId) !u32 {
    const block_start: u32 = @intCast(stream.bytes_written);

    try stream.writer().writeInt(BlockId.Raw, id.raw(), .little);
    // Write the length as a placeholder to be filled in later
    try stream.writer().writeAll(&@as([4]u8, undefined));

    return block_start;
}

pub fn endBlock(stream: anytype, fixups: *std.array_list.Managed(Fixup), block_start: u32) !void {
    const stream_pos: u32 = @intCast(stream.bytes_written);
    try fixups.append(.{
        .offset = block_start + 4,
        .bytes = Fixup.encode(stream_pos - block_start, .big),
    });
}

pub fn beginBlockKnown(stream: anytype, id: BlockId, size: u32) !struct { u32, u32 } {
    const block_start: u32 = @intCast(stream.bytes_written);

    try stream.writer().writeInt(BlockId.Raw, id.raw(), .little);
    try stream.writer().writeInt(u32, size + 8, .big);

    return .{ block_start, size + 8 };
}

pub fn endBlockKnown(stream: anytype, start_and_size: struct { u32, u32 }) void {
    const start, const size = start_and_size;
    const pos: u32 = @intCast(stream.bytes_written);
    std.debug.assert(pos - start == size);
}

pub fn beginBlockAl(gpa: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), id: BlockId) !u32 {
    const block_start: u32 = @intCast(out.items.len);

    try out.writer(gpa).writeInt(BlockId.Raw, id.raw(), .little);
    // Leave the length as a placeholder to be filled in later
    _ = try out.addManyAsSlice(gpa, 4);

    return block_start;
}

pub fn endBlockAl(out: *std.ArrayListUnmanaged(u8), block_start: u32) void {
    const dest = out.items[block_start + 4 ..][0..4];
    const pos: u32 = @intCast(out.items.len);
    const value = pos - block_start;
    std.mem.writeInt(u32, dest, value, .big);
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
