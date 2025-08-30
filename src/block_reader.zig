const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const io = @import("io.zig");
const iold = @import("iold.zig");

pub const block_header_size = 8;

pub fn oldBlockReader(stream: anytype) OldBlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn OldBlockReader(Stream: type) type {
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

            const id_raw = try self.stream.reader().readInt(BlockId.Raw, .little);
            const id = BlockId.init(id_raw) orelse return error.BadData;

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
                    \\expected block {} but found {}
                    \\
                ,
                    .{ expected_id, id },
                );
                return error.BadData;
            }
            return len;
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
pub fn oldFixedBlockReader(stream: anytype) OldFixedBlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn OldFixedBlockReader(Stream: type) type {
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

            const id_raw = try self.stream.reader().readInt(BlockId.Raw, .little);
            const id = BlockId.init(id_raw) orelse return error.BadData;

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
            const raw = std.mem.readInt(BlockId.Raw, id_buf, .little);
            return BlockId.init(raw) orelse return error.BadData;
        }

        pub fn expect(self: *Self, expected_id: BlockId) !u32 {
            const id, const len = try self.next();
            if (id != expected_id) {
                std.debug.print(
                    \\expected block {} but found {}
                    \\
                ,
                    .{ expected_id, id },
                );
                return error.BadData;
            }
            return len;
        }

        pub fn expectAsSlice(self: *Self, expected_id: BlockId) ![]const u8 {
            const len = try self.expect(expected_id);
            return try io.readInPlace(self.stream, len);
        }

        pub fn expectAsValue(self: *Self, expected_id: BlockId, T: type) !*align(1) const T {
            const expected_len = @sizeOf(T);
            const len = try self.expect(expected_id);
            if (len != expected_len)
                return error.BadData;
            return try io.readInPlaceAsValue(self.stream, T);
        }

        pub fn assume(self: *Self, expected_id: BlockId) !u32 {
            const id, const len = try self.next();
            std.debug.assert(id == expected_id);
            return len;
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

pub fn fixedBlockReader(
    stream: *std.io.FixedBufferStream([]const u8),
    diag: *const Diagnostic.ForBinaryFile,
) FixedBlockReader {
    std.debug.assert(stream.pos == 0);
    return .{
        .stream = stream,
        .end = @intCast(stream.buffer.len),
        .diag = diag,
        .current = null,
    };
}

const FixedBlockReader = struct {
    const Self = @This();

    stream: *std.io.FixedBufferStream([]const u8),
    end: u32,
    diag: *const Diagnostic.ForBinaryFile,
    current: ?Block,

    pub fn next(self: *Self) BlockResult {
        if (!self.checkEndBlock()) return .err;

        const offset: u32 = @intCast(self.stream.pos);
        const header = self.stream.reader().readBytesNoEof(8) catch {
            self.diag.err(offset, "eof during block header", .{});
            return .err;
        };
        const id_raw = std.mem.readInt(BlockId.Raw, header[0..4], .little);
        const id = self.validateId(id_raw, offset) catch return .err;
        const full_size = std.mem.readInt(u32, header[4..8], .big);
        // The original value includes the id and length, but the caller
        // doesn't care about those, so subtract them out.
        const size = full_size - block_header_size;
        self.current = .{
            .id = id,
            .start = @intCast(self.stream.pos),
            .size = size,
        };

        self.diag.trace(offset, "start block {}", .{id});

        return .{ .ok = .{
            .block = self.current.?,
            .reader = self,
        } };
    }

    pub fn peek(self: *Self) !BlockId {
        if (!self.checkEndBlock()) return error.AddedToDiagnostic;

        const offset: u32 = @intCast(self.stream.pos);
        if (offset + block_header_size > self.stream.buffer.len) {
            self.diag.err(offset, "eof during block header", .{});
            return error.AddedToDiagnostic;
        }
        const header = self.stream.buffer[offset..][0..block_header_size];
        const raw = std.mem.readInt(BlockId.Raw, header[0..4], .little);
        return self.validateId(raw, offset);
    }

    fn validateId(self: *const Self, raw: BlockId.Raw, offset: u32) !BlockId {
        return BlockId.init(raw) orelse {
            self.diag.err(offset, "invalid block id: {}", .{BlockId.fmtInvalid(raw)});
            return error.AddedToDiagnostic;
        };
    }

    pub fn nextIf(self: *Self, expected_id: BlockId) !?BlockResult {
        const result = try self.peek();
        if (result != expected_id) return null;
        return self.next();
    }

    fn checkEndBlock(self: *Self) bool {
        const current = self.current orelse return true;

        const pos: u32 = @intCast(self.stream.pos);
        const expected_end = current.end();
        if (pos == expected_end) { // happy path
            self.diag.trace(pos, "end block {}", .{current.id});
            self.current = null;
            return true;
        }

        // otherwise report an error
        self.diag.err(
            pos,
            "desync during block {}; expected end 0x{x:0>8}",
            .{ current.id, expected_end },
        );
        return false;
    }

    pub fn expect(self: *Self, id: BlockId) BlockResult {
        return self.next().expect(id);
    }

    pub fn assume(self: *Self, id: BlockId) BlockResult {
        return self.next().assume(id);
    }

    pub fn atEnd(self: *const Self) bool {
        const pos: u32 = @intCast(self.stream.pos);
        return pos == self.end;
    }

    pub fn finish(self: *Self) !void {
        if (!self.checkEndBlock()) return error.AddedToDiagnostic;

        const pos: u32 = @intCast(self.stream.pos);
        if (pos != self.end) {
            self.diag.err(
                pos,
                "expected container to end at 0x{x:0>8}",
                .{self.end},
            );
            return error.AddedToDiagnostic;
        }
        self.diag.trace(pos, "end of container", .{});
    }

    /// hint for the reader that we're throwing away the rest on purpose
    pub fn abandon(self: *Self) void {
        _ = self;
    }
};

const BlockResult = union(enum) {
    const Self = @This();

    ok: struct {
        block: Block,
        reader: *const FixedBlockReader,
    },
    err,

    pub fn expect(self: Self, id: BlockId) Self {
        if (self != .ok) return .err;
        if (id == self.ok.block.id) return self;
        self.ok.reader.diag.err(
            self.ok.block.start - block_header_size,
            "expected block {} but found {}",
            .{ id, self.ok.block.id },
        );
        return .err;
    }

    pub fn assume(self: Self, id: BlockId) Self {
        if (self != .ok) return .err;
        std.debug.assert(id == self.ok.block.id);
        return self;
    }

    pub fn block(self: *const Self) !Block {
        if (self.* != .ok) return error.AddedToDiagnostic;
        return self.ok.block;
    }

    pub fn bytes(self: *const Self) ![]const u8 {
        if (self.* != .ok) return error.AddedToDiagnostic;
        return io.readInPlace(self.ok.reader.stream, self.ok.block.size) catch |err| {
            self.ok.reader.diag.err(
                self.ok.block.start,
                "error reading block data: {}",
                .{err},
            );
            return error.AddedToDiagnostic;
        };
    }

    pub fn value(self: *const Self, T: type) !*align(1) const T {
        if (self.* != .ok) return error.AddedToDiagnostic;
        const expected_size = @sizeOf(T);
        if (self.ok.block.size != expected_size) {
            self.ok.reader.diag.err(
                self.ok.block.start - 4,
                "block size mismatch: expected {}, found {}",
                .{ expected_size, self.ok.block.size },
            );
            return error.AddedToDiagnostic;
        }
        const data = try self.bytes();
        return std.mem.bytesAsValue(T, data);
    }

    pub fn nested(self: *const Self) !FixedBlockReader {
        if (self.* != .ok) return error.AddedToDiagnostic;
        const pos: u32 = @intCast(self.ok.reader.stream.pos);
        return .{
            .stream = self.ok.reader.stream,
            .end = pos + self.ok.block.size,
            .diag = self.ok.reader.diag,
            .current = null,
        };
    }
};

pub const FxbclReader = iold.LimitedReader(std.io.CountingReader(iold.BufferedReader(4096, io.XorReader(std.fs.File.Reader).Reader).Reader).Reader);

pub fn fxbclPos(in: *const FxbclReader) u32 {
    return @intCast(in.inner_reader.context.bytes_read);
}

pub const StreamingBlockReader = struct {
    in: *FxbclReader,
    diag: *const Diagnostic.ForBinaryFile,
    state: union {
        baseline: void,
        inside_block: struct { next_limit: u32 },
    },

    pub fn init(in: *FxbclReader, diag: *const Diagnostic.ForBinaryFile) StreamingBlockReader {
        return .{
            .in = in,
            .diag = diag,
            .state = .{ .baseline = {} },
        };
    }

    pub fn next(self: *StreamingBlockReader) !?Block {
        const offset, const header = try self.readHeader() orelse return null;
        const block = self.validate(offset, header) orelse return error.BadData;
        self.commit(&block);
        return block;
    }

    pub fn readHeader(self: *const StreamingBlockReader) !?struct { u32, RawBlockHeader } {
        _ = self.state.baseline;
        const offset = fxbclPos(self.in);
        var header: RawBlockHeader = undefined;
        const len = try self.in.reader().readAll(std.mem.asBytes(&header));
        if (len == 0) return null;
        if (len == Block.header_size) return .{ offset, header };
        return error.EndOfStream;
    }

    pub fn validate(
        self: *const StreamingBlockReader,
        offset: u32,
        header: RawBlockHeader,
    ) ?Block {
        const raw_id = header.raw_id();
        const id = BlockId.init(raw_id) orelse return null;

        const raw_size = header.raw_size();
        if (raw_size < Block.header_size) return null;
        const size = raw_size - Block.header_size;
        if (size > self.in.bytes_left) return null;

        const start = offset + Block.header_size;
        return .{ .start = start, .id = id, .size = size };
    }

    pub fn commit(self: *StreamingBlockReader, block: *const Block) void {
        _ = self.state.baseline;

        self.diag.trace(block.offset(), "start block {}", .{block.id});

        std.debug.assert(fxbclPos(self.in) == block.start);

        const prev_limit: u32 = @intCast(self.in.bytes_left);
        const next_limit = prev_limit - block.size;
        self.state = .{ .inside_block = .{ .next_limit = next_limit } };

        self.in.bytes_left = block.size;
    }

    pub fn finish(self: *StreamingBlockReader, block: *const Block) !void {
        _ = self.state.inside_block;

        const pos = fxbclPos(self.in);
        self.diag.trace(pos, "end block {}", .{block.id});

        if (self.in.bytes_left != 0) return error.BadData;
        std.debug.assert(pos == block.end());

        self.in.bytes_left = self.state.inside_block.next_limit;
        self.state = .{ .baseline = {} };
    }

    pub fn end(self: *const StreamingBlockReader) !void {
        _ = self.state.baseline;
        if (self.in.bytes_left != 0) return error.BadData;
    }

    pub fn expectMismatchedEnd(self: *const StreamingBlockReader) void {
        _ = self.state.baseline;
    }

    pub fn expect(self: *StreamingBlockReader, id: BlockId) !?Block {
        const block = try self.next() orelse return null;
        if (block.id != id) {
            self.diag.err(block.offset(), "expected block {} but found {}", .{ id, block.id });
            return error.AddedToDiagnostic;
        }
        return block;
    }
};

const RawBlockHeader = extern struct {
    raw: [2]u32,

    fn raw_id(self: *const RawBlockHeader) BlockId.Raw {
        return self.raw[0];
    }

    fn raw_size(self: *const RawBlockHeader) u32 {
        // big endian
        return @byteSwap(self.raw[1]);
    }
};

pub const Block = struct {
    pub const header_size = block_header_size;

    id: BlockId,
    start: u32,
    size: u32,

    pub fn offset(self: *const Block) u32 {
        return self.start - block_header_size;
    }

    pub fn full_size(self: *const Block) u32 {
        return header_size + self.size;
    }

    pub fn end(self: *const Block) u32 {
        return self.start + self.size;
    }
};
