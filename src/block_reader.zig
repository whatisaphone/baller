const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const io = @import("io.zig");

pub const block_header_size = 8;

pub fn blockReader(stream: anytype) BlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn BlockReader(Stream: type) type {
    comptime std.debug.assert(std.mem.startsWith(
        u8,
        @typeName(Stream),
        "*io.counting_reader.CountingReader(",
    ));

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
            const len = full_len - block_header_size;

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
            const id = blockId(expected_id);
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
            const id = blockId(block_id);
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

// This is identical to blockReader, but uses FixedBufferStream instead of
// CountingReader. Prefer this over blockReader where possible.
pub fn fixedBlockReader(stream: anytype) FixedBlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn FixedBlockReader(Stream: type) type {
    comptime std.debug.assert(std.mem.startsWith(
        u8,
        @typeName(Stream),
        "*io.fixed_buffer_stream.FixedBufferStream(",
    ));

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
            const len = full_len - block_header_size;

            const current_pos: u32 = @intCast(self.stream.pos);
            self.current_block_end = current_pos + len;

            return .{ id, len };
        }

        pub fn nextAsSlice(self: *Self) !struct { BlockId, []const u8 } {
            const id, const len = try self.next();
            const raw = try io.readInPlace(self.stream, len);
            return .{ id, raw };
        }

        pub fn peek(self: *const Self) !?BlockId {
            try self.checkSync();

            if (self.stream.pos == self.stream.buffer.len)
                return null;

            const id_buf = try io.peekInPlaceBytes(self.stream, 4);
            return std.mem.readInt(BlockId, id_buf, .little);
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
            const id = blockId(expected_id);
            return self.expect(id);
        }

        pub fn expectBlockAsSlice(self: *Self, comptime expected_id: []const u8) ![]const u8 {
            const len = try self.expectBlock(expected_id);
            return try io.readInPlace(self.stream, len);
        }

        pub fn expectBlockAsValue(
            self: *Self,
            comptime expected_id: []const u8,
            T: type,
        ) !*align(1) const T {
            const expected_len = @sizeOf(T);
            const len = try self.expectBlock(expected_id);
            if (len != expected_len)
                return error.BadData;
            return try io.readInPlaceAsValue(self.stream, T);
        }

        pub fn assume(self: *Self, expected_id: BlockId) !u32 {
            const id, const len = try self.next();
            std.debug.assert(id == expected_id);
            return len;
        }

        pub fn assumeBlock(self: *Self, comptime expected_id: []const u8) !u32 {
            const id = blockId(expected_id);
            return self.assume(id);
        }

        pub fn skipUntil(self: *Self, block_id: BlockId) !u32 {
            while (true) {
                const id, const len = try self.next();
                if (id != block_id) {
                    _ = try io.readInPlace(self.stream, len);
                    continue;
                }
                return len;
            }
        }

        pub fn skipUntilBlock(self: *Self, comptime block_id: []const u8) !u32 {
            const id = blockId(block_id);
            return self.skipUntil(id);
        }

        pub fn checkSync(self: *const Self) !void {
            const current_block_end = self.current_block_end orelse return;
            if (self.stream.pos != current_block_end)
                return error.BlockDesync;
        }

        pub fn finish(self: *const Self, expected_pos: u32) !void {
            if (self.stream.pos != expected_pos)
                return error.BlockDesync;
        }

        pub fn finishEof(self: *const Self) !void {
            try io.requireEof(self.stream.reader());
        }
    };
}
