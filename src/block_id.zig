const std = @import("std");

pub const BlockId = u32;
pub const block_id_len = @sizeOf(BlockId);

pub inline fn blockId(comptime str: []const u8) BlockId {
    if (str.len != 4)
        @compileError("block IDs are four bytes long");
    comptime return std.mem.bytesToValue(BlockId, str);
}

pub const blockIdToStr = std.mem.asBytes;

pub fn fmtBlockId(id: *const BlockId) @TypeOf(std.fmt.fmtSliceEscapeLower("")) {
    return std.fmt.fmtSliceEscapeLower(blockIdToStr(id));
}

pub fn parseBlockId(str: []const u8) ?BlockId {
    if (str.len != 4)
        return null;
    for (str) |ch|
        if (!('A' <= ch and ch <= 'Z' or '0' <= ch and ch <= '9'))
            return null;
    return std.mem.bytesToValue(BlockId, str);
}
