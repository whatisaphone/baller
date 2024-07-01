const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;

pub fn beginBlock(stream: anytype, comptime block_id: []const u8) !u32 {
    const id = comptime blockId(block_id);
    return beginBlockImpl(stream, id);
}

pub fn beginBlockImpl(stream: anytype, id: BlockId) !u32 {
    const block_start: u32 = @intCast(stream.bytes_written);

    try stream.writer().writeInt(BlockId, id, .little);
    // Write the length as a placeholder to be filled in later
    try stream.writer().writeAll(&@as([4]u8, undefined));

    return block_start;
}

pub fn endBlock(stream: anytype, fixups: *std.ArrayList(Fixup), block_start: u32) !void {
    const stream_pos: u32 = @intCast(stream.bytes_written);
    try fixups.append(.{
        .offset = block_start + 4,
        .value = stream_pos - block_start,
    });
}

pub const Fixup = struct {
    offset: u32,
    value: u32,
};
