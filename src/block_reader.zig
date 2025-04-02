const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
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

pub fn fixedBlockReader2(
    stream: anytype,
    diagnostic: *const Diagnostic,
) FixedBlockReader2(@TypeOf(stream)) {
    return .{
        .stream = stream,
        .diagnostic = diagnostic,
        .current = null,
    };
}

fn FixedBlockReader2(Stream: type) type {
    comptime std.debug.assert(std.mem.startsWith(
        u8,
        @typeName(Stream),
        "*io.fixed_buffer_stream.FixedBufferStream(",
    ));

    return struct {
        const Self = @This();

        stream: Stream,
        diagnostic: *const Diagnostic,
        current: ?Block,

        pub fn next(self: *Self) BlockResult(Stream) {
            if (!self.checkEndBlock()) return .err;

            const offset: u32 = @intCast(self.stream.pos);
            const header = self.stream.reader().readBytesNoEof(8) catch {
                self.diagnostic.err(offset, "eof during block header", .{});
                return .err;
            };
            const id = std.mem.readInt(u32, header[0..4], .little);
            const full_size = std.mem.readInt(u32, header[4..8], .big);
            // The original value includes the id and length, but the caller
            // doesn't care about those, so subtract them out.
            const size = full_size - block_header_size;
            self.current = .{
                .id = id,
                .start = @intCast(self.stream.pos),
                .size = size,
            };

            self.diagnostic.trace(offset, "start block {s}", .{fmtBlockId(&id)});

            return .{ .ok = .{
                .block = self.current.?,
                .reader = self,
            } };
        }

        fn peek(self: *Self) !BlockId {
            const offset: u32 = @intCast(self.stream.pos);
            if (offset + block_header_size > self.stream.buffer.len) {
                self.diagnostic.err(offset, "eof during block header", .{});
                return error.Reported;
            }
            const header = self.stream.buffer[offset..][0..block_header_size];
            return std.mem.readInt(u32, header[0..4], .little);
        }

        pub fn nextIf(self: *Self, comptime expected_id: *const [4]u8) !?BlockResult(Stream) {
            return self.nextIf2(blockId(expected_id));
        }

        fn nextIf2(self: *Self, comptime expected_id: BlockId) !?BlockResult(Stream) {
            const result = try self.peek();
            if (result != expected_id) return null;
            return self.next();
        }

        fn checkEndBlock(self: *Self) bool {
            const current = self.current orelse return true;

            const pos: u32 = @intCast(self.stream.pos);
            const expected_end = current.end();
            if (pos == expected_end) { // happy path
                self.diagnostic.trace(pos, "end block {s}", .{fmtBlockId(&current.id)});
                self.current = null;
                return true;
            }

            // otherwise report an error
            self.diagnostic.err(
                pos,
                "desync during block {s}; expected end 0x{x:0>8}",
                .{ fmtBlockId(&current.id), expected_end },
            );
            return false;
        }

        pub inline fn expect(self: *Self, id: anytype) BlockResult(Stream) {
            return self.next().expect(id);
        }

        pub fn finish(self: *Self, expected_pos: u32) !void {
            if (!self.checkEndBlock()) return error.Reported;

            const pos: u32 = @intCast(self.stream.pos);
            if (pos != expected_pos) {
                self.diagnostic.err(
                    pos,
                    "expected container to end at 0x{x:0>8}",
                    .{expected_pos},
                );
                return error.Reported;
            }
            self.diagnostic.trace(pos, "end of container", .{});
        }

        pub fn finishEof(self: *Self) !void {
            if (!self.checkEndBlock()) return error.Reported;

            const offset: u32 = @intCast(self.stream.pos);
            io.requireEof(self.stream.reader()) catch {
                self.diagnostic.err(offset, "expected eof", .{});
                return error.Reported;
            };
            self.diagnostic.trace(offset, "eof", .{});
        }
    };
}

fn BlockResult(Stream: type) type {
    return union(enum) {
        const Self = @This();

        ok: struct {
            block: Block,
            reader: *const FixedBlockReader2(Stream),
        },
        err,

        pub inline fn expect(self: Self, id: anytype) Self {
            return if (@typeInfo(@TypeOf(id)) == .pointer)
                self.expectBlockIdComptime(id)
            else
                self.expectBlockId(id);
        }

        fn expectBlockIdComptime(self: Self, comptime id: *const [4]u8) Self {
            return self.expectBlockId(blockId(id));
        }

        fn expectBlockId(self: Self, id: BlockId) Self {
            if (self != .ok) return .err;
            if (id == self.ok.block.id) return self;
            self.ok.reader.diagnostic.err(
                self.ok.block.start - block_header_size,
                "expected block \"{s}\" but found \"{s}\"",
                .{ fmtBlockId(&id), fmtBlockId(&self.ok.block.id) },
            );
            return .err;
        }

        pub fn block(self: *const Self) !Block {
            if (self.* != .ok) return error.Reported;
            return self.ok.block;
        }

        pub fn bytes(self: *const Self) ![]const u8 {
            if (self.* != .ok) return error.Reported;
            return io.readInPlace(self.ok.reader.stream, self.ok.block.size) catch |err| {
                self.ok.reader.diagnostic.err(
                    self.ok.block.start,
                    "error reading block data: {}",
                    .{err},
                );
                return error.Reported;
            };
        }

        pub fn value(self: *const Self, T: type) !*align(1) const T {
            if (self.* != .ok) return error.Reported;
            const expected_size = @sizeOf(T);
            if (self.ok.block.size != expected_size) {
                self.ok.reader.diagnostic.err(
                    self.ok.block.start,
                    "block size mismatch: expected {}, found {}",
                    .{ expected_size, self.ok.block.size },
                );
                return error.Reported;
            }
            const data = try self.bytes();
            return std.mem.bytesAsValue(T, data);
        }

        pub fn nested(self: *const Self) !FixedBlockReader2(Stream) {
            if (self.* != .ok) return error.Reported;
            return fixedBlockReader2(self.ok.reader.stream, self.ok.reader.diagnostic);
        }
    };
}

pub fn streamingBlockReader(
    stream: anytype,
    diagnostic: *const Diagnostic,
) StreamingBlockReader(@TypeOf(stream)) {
    return .{
        .stream = stream,
        .diagnostic = diagnostic,
        .current = null,
    };
}

fn StreamingBlockReader(Stream: type) type {
    comptime std.debug.assert(std.mem.startsWith(
        u8,
        @typeName(Stream),
        "*io.counting_reader.CountingReader(",
    ));

    return struct {
        const Self = @This();

        stream: Stream,
        diagnostic: *const Diagnostic,
        current: ?Block,

        pub fn next(self: *Self) StreamingBlockResult(Stream) {
            if (!self.checkEndBlock()) return .err;

            const offset: u32 = @intCast(self.stream.bytes_read);
            const header = self.stream.reader().readBytesNoEof(8) catch |err| {
                self.diagnostic.err(offset, "failed to read block header: {}", .{err});
                return .err;
            };
            const id = std.mem.readInt(u32, header[0..4], .little);
            const full_size = std.mem.readInt(u32, header[4..8], .big);
            // The original value includes the id and length, but the caller
            // doesn't care about those, so subtract them out.
            const size = full_size - block_header_size;
            self.current = .{
                .id = id,
                .start = @intCast(self.stream.bytes_read),
                .size = size,
            };

            self.diagnostic.trace(offset, "start block {s}", .{fmtBlockId(&id)});

            return .{ .ok = .{
                .block = self.current.?,
                .reader = self,
            } };
        }

        fn checkEndBlock(self: *Self) bool {
            const current = self.current orelse return true;

            const pos: u32 = @intCast(self.stream.bytes_read);
            const expected_end = current.end();
            if (pos == expected_end) { // happy path
                self.diagnostic.trace(pos, "end block {s}", .{fmtBlockId(&current.id)});
                self.current = null;
                return true;
            }

            // otherwise report an error
            self.diagnostic.err(
                pos,
                "desync during block {s}; expected end 0x{x:0>8}",
                .{ fmtBlockId(&current.id), expected_end },
            );
            return false;
        }

        pub inline fn expect(self: *Self, id: anytype) StreamingBlockResult(Stream) {
            return self.next().expect(id);
        }

        pub fn finish(self: *Self, expected_pos: u32) !void {
            if (!self.checkEndBlock()) return error.Reported;

            const pos: u32 = @intCast(self.stream.bytes_read);
            if (pos != expected_pos) {
                self.diagnostic.err(
                    pos,
                    "expected container to end at 0x{x:0>8}",
                    .{expected_pos},
                );
                return error.Reported;
            }
            self.diagnostic.trace(pos, "end of container", .{});
        }

        pub fn finishEof(self: *Self) !void {
            if (!self.checkEndBlock()) return error.Reported;

            const pos: u32 = @intCast(self.stream.bytes_read);
            io.requireEof(self.stream.reader()) catch {
                self.diagnostic.err(pos, "expected eof", .{});
                return error.Reported;
            };
            self.diagnostic.trace(pos, "eof", .{});
        }
    };
}

fn StreamingBlockResult(Stream: type) type {
    return union(enum) {
        const Self = @This();

        ok: struct {
            block: Block,
            reader: *const StreamingBlockReader(Stream),
        },
        err,

        pub inline fn expect(self: Self, id: anytype) Self {
            return if (@typeInfo(@TypeOf(id)) == .pointer)
                self.expectBlockIdComptime(id)
            else
                self.expectBlockId(id);
        }

        fn expectBlockIdComptime(self: Self, comptime id: *const [4]u8) Self {
            return self.expectBlockId(blockId(id));
        }

        fn expectBlockId(self: Self, id: BlockId) Self {
            if (self != .ok) return .err;
            if (id == self.ok.block.id) return self;
            self.ok.reader.diagnostic.err(
                self.ok.block.start - block_header_size,
                "expected block \"{s}\" but found \"{s}\"",
                .{ fmtBlockId(&id), fmtBlockId(&self.ok.block.id) },
            );
            return .err;
        }

        pub fn block(self: *const Self) !Block {
            if (self.* != .ok) return error.Reported;
            return self.ok.block;
        }
    };
}

pub const Block = struct {
    id: BlockId,
    start: u32,
    size: u32,

    pub fn offset(self: *const Block) u32 {
        return self.start - block_header_size;
    }

    pub fn end(self: *const Block) u32 {
        return self.start + self.size;
    }
};
