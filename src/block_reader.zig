const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const io = @import("io.zig");

pub fn blockReader(stream: anytype) BlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn BlockReader(Stream: type) type {
    comptime std.debug.assert(@typeInfo(Stream) == .Pointer);

    return struct {
        const Self = @This();

        stream: Stream,
        current_block_end: ?u32 = null,

        pub fn next(self: *Self) !struct { BlockId, u32 } {
            try self.checkSync();

            const id = try self.stream.reader().readInt(BlockId, .little);

            const full_len = try self.stream.reader().readInt(u32, .big);
            // The original value includes the id and length, but the caller
            // doesn't care about those, so subtract them out.
            const len = full_len - 8;

            const current_pos: u32 = @intCast(self.stream.bytes_read);
            self.current_block_end = current_pos + len;

            return .{ id, len };
        }

        pub fn expect(self: *Self, expected_id: BlockId) !u32 {
            const id, const len = try self.next();
            if (id != expected_id) {
                std.debug.print(
                    \\expected block "{s}" but found "{s}"
                    \\
                ,
                    .{ fmtBlockId(&expected_id), fmtBlockId(&id) },
                );
                return error.BadData;
            }
            return len;
        }

        pub fn expectBlock(self: *Self, comptime expected_id: []const u8) !u32 {
            const id = comptime blockId(expected_id);
            return self.expect(id);
        }

        pub fn skipUntil(self: *Self, block_id: BlockId) !u32 {
            while (true) {
                const id, const len = try self.next();
                if (id != block_id) {
                    try self.stream.reader().skipBytes(len, .{});
                    continue;
                }
                return len;
            }
        }

        pub fn skipUntilBlock(self: *Self, comptime block_id: []const u8) !u32 {
            const id = comptime blockId(block_id);
            return self.skipUntil(id);
        }

        pub fn checkSync(self: *const Self) !void {
            const current_block_end = self.current_block_end orelse return;
            if (self.stream.bytes_read != current_block_end)
                return error.BlockDesync;
        }

        pub fn finish(self: *const Self, expected_pos: u32) !void {
            if (self.stream.bytes_read != expected_pos)
                return error.BlockDesync;
        }

        pub fn finishEof(self: *const Self) !void {
            try io.requireEof(self.stream.reader());
        }
    };
}
