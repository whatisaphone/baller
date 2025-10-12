const std = @import("std");

const utils = @import("utils.zig");

pub fn requireEof(s: anytype) !void {
    _ = s.readByte() catch |err| switch (err) {
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

pub fn readInPlace(stream: anytype, len: usize) ![]const u8 {
    const end = stream.pos + len;
    if (end > stream.buffer.len)
        return error.EndOfStream;
    const result = stream.buffer[stream.pos..][0..len];
    stream.pos = end;
    return result;
}

pub fn readInPlaceBytes(stream: anytype, comptime len: usize) !*const [len]u8 {
    const result = try readInPlace(stream, len);
    return result[0..len];
}

pub fn readInPlaceAsValue(stream: anytype, T: type) !*align(1) const T {
    std.debug.assert(utils.hasGuaranteedLayout(T));

    const data = try readInPlace(stream, @sizeOf(T));
    return std.mem.bytesAsValue(T, data);
}

pub fn peekInPlace(stream: anytype, len: usize) ![]const u8 {
    const end = stream.pos + len;
    if (end > stream.buffer.len)
        return error.EndOfStream;
    return stream.buffer[stream.pos..][0..len];
}

pub fn peekInPlaceBytes(stream: anytype, comptime len: usize) !*const [len]u8 {
    const result = try peekInPlace(stream, len);
    return result[0..len];
}

pub fn peekInPlaceAsValue(stream: anytype, T: type) !*align(1) const T {
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

pub fn OldXorWriter(Stream: type) type {
    return struct {
        stream: Stream,
        key: u8,

        pub const Writer = std.io.GenericWriter(*const @This(), Stream.Error, write);

        pub fn writer(self: *const @This()) Writer {
            return .{ .context = self };
        }

        fn write(self: *const @This(), bytes: []const u8) Stream.Error!usize {
            var buf: [4096]u8 = undefined;
            const chunk_len = @min(bytes.len, buf.len);
            @memcpy(buf[0..chunk_len], bytes[0..chunk_len]);
            for (buf[0..chunk_len]) |*p|
                p.* ^= self.key;
            return self.stream.write(buf[0..chunk_len]);
        }
    };
}

pub fn oldXorWriter(stream: anytype, key: u8) OldXorWriter(@TypeOf(stream)) {
    return .{ .stream = stream, .key = key };
}
