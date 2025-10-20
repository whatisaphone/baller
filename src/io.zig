const std = @import("std");

const utils = @import("utils.zig");

pub fn requireEof(r: *std.io.Reader) !void {
    _ = r.takeByte() catch |err| switch (err) {
        error.EndOfStream => return,
        else => return err,
    };
    return error.StreamTooLong;
}

pub fn copy(input: anytype, output: anytype) !void {
    var input_mut = input;

    var buf: [4096]u8 = undefined;
    while (true) {
        const len = try input_mut.read(&buf);
        if (len == 0)
            break;
        try output.writeAll(buf[0..len]);
    }
}

pub fn readInPlace(stream: *std.io.Reader, len: usize) ![]const u8 {
    const end = stream.seek + len;
    if (end > stream.buffer.len)
        return error.EndOfStream;
    const result = stream.buffer[stream.seek..][0..len];
    stream.seek = end;
    return result;
}

pub fn readInPlaceBytes(stream: *std.io.Reader, comptime len: usize) !*const [len]u8 {
    const result = try readInPlace(stream, len);
    return result[0..len];
}

pub fn readInPlaceAsValue(stream: *std.io.Reader, T: type) !*align(1) const T {
    std.debug.assert(utils.hasGuaranteedLayout(T));

    const data = try readInPlace(stream, @sizeOf(T));
    return std.mem.bytesAsValue(T, data);
}

pub fn peekInPlace(stream: *std.io.Reader, len: usize) ![]const u8 {
    const end = stream.seek + len;
    if (end > stream.buffer.len)
        return error.EndOfStream;
    return stream.buffer[stream.seek..][0..len];
}

pub fn peekInPlaceBytes(stream: *std.io.Reader, comptime len: usize) !*const [len]u8 {
    const result = try peekInPlace(stream, len);
    return result[0..len];
}

pub fn peekInPlaceAsValue(stream: *std.io.Reader, T: type) !*align(1) const T {
    std.debug.assert(utils.hasGuaranteedLayout(T));

    const data = try peekInPlace(stream, @sizeOf(T));
    return std.mem.bytesAsValue(T, data);
}

pub const XorReader = struct {
    inner: *std.io.Reader,
    key: u8,
    interface: std.io.Reader,

    pub fn init(inner: *std.io.Reader, key: u8, buffer: []u8) XorReader {
        return .{
            .inner = inner,
            .key = key,
            .interface = .{
                .vtable = &.{
                    .stream = stream,
                },
                .buffer = buffer,
                .seek = 0,
                .end = 0,
            },
        };
    }

    fn stream(r: *std.io.Reader, w: *std.io.Writer, limit: std.io.Limit) !usize {
        const self: *XorReader = @fieldParentPtr("interface", r);
        const buf = w.unusedCapacitySlice();
        const n = try self.inner.stream(w, limit);
        // this is a simplified implementation. it won't work unless the full
        // read fits into w's buffer. assert that's always the case in practice
        std.debug.assert(w.unusedCapacityLen() == buf.len - n);
        for (buf[0..n]) |*b|
            b.* ^= self.key;
        return n;
    }
};

pub const XorWriter = struct {
    inner: *std.io.Writer,
    key: u8,
    interface: std.io.Writer,

    pub fn init(inner: *std.io.Writer, key: u8, buffer: []u8) XorWriter {
        return .{
            .inner = inner,
            .key = key,
            .interface = .{
                .vtable = &.{
                    .drain = drain,
                },
                .buffer = buffer,
            },
        };
    }

    pub fn drain(w: *std.io.Writer, data: []const []const u8, splat: usize) !usize {
        const self: *XorWriter = @fieldParentPtr("interface", w);

        // simplified implementation only works in this simple case
        std.debug.assert(data.len == 1);
        std.debug.assert(splat == 1);
        const bytes = data[0];

        // TODO: use writer buffer instead of stack buffer to avoid extra memcpy
        var buf: [4096]u8 = undefined;
        const chunk_len = @min(bytes.len, buf.len);
        @memcpy(buf[0..chunk_len], bytes[0..chunk_len]);
        for (buf[0..chunk_len]) |*p|
            p.* ^= self.key;
        return self.inner.write(buf[0..chunk_len]);
    }
};

pub const BitReader = struct {
    inner: *std.io.Reader,
    buffer: u8,
    buf_count: u8,

    pub fn init(inner: *std.io.Reader) BitReader {
        return .{
            .inner = inner,
            .buffer = undefined,
            .buf_count = 0,
        };
    }

    pub fn takeBits(self: *BitReader, Result: type, bits: u8) !Result {
        const result_info = @typeInfo(Result);
        std.debug.assert(bits <= result_info.int.bits);

        const result = try self.takeBitsInner(bits);
        return @intCast(result);
    }

    // This is split apart to prevent generic explosion
    fn takeBitsInner(self: *BitReader, bits: u8) !u8 {
        std.debug.assert(bits <= 8);

        const count = @min(bits, self.buf_count);
        var result = try self.drainBuffer(count);
        const remaining = bits - count;
        if (remaining != 0) {
            try self.fillBuffer();
            result |= @shlExact(try self.drainBuffer(remaining), @intCast(count));
        }
        return result;
    }

    fn fillBuffer(self: *BitReader) !void {
        std.debug.assert(self.buf_count == 0);
        self.buffer = try self.inner.takeByte();
        self.buf_count = 8;
    }

    fn drainBuffer(self: *BitReader, bits: u8) !u8 {
        std.debug.assert(bits <= self.buf_count);
        if (bits == 8) {
            self.buf_count = 0;
            return self.buffer;
        }
        const mask: u8 = @intCast(@shlExact(@as(u9, 1), @intCast(bits)) - 1);
        const result = self.buffer & mask;
        self.buffer >>= @intCast(bits);
        self.buf_count -= bits;
        return result;
    }
};
