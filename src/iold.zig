// Copy/paste of some zig 0.14 streams as a stopgap during migration to 0.15

const std = @import("std");

const assert = std.debug.assert;
const io = std.io;
const mem = std.mem;
const testing = std.testing;

pub fn BufferedReader(comptime buffer_size: usize, comptime ReaderType: type) type {
    return struct {
        unbuffered_reader: ReaderType,
        buf: [buffer_size]u8 = undefined,
        start: usize = 0,
        end: usize = 0,

        pub const Error = ReaderType.Error;
        pub const Reader = io.Reader(*Self, Error, read);

        const Self = @This();

        pub fn read(self: *Self, dest: []u8) Error!usize {
            // First try reading from the already buffered data onto the destination.
            const current = self.buf[self.start..self.end];
            if (current.len != 0) {
                const to_transfer = @min(current.len, dest.len);
                @memcpy(dest[0..to_transfer], current[0..to_transfer]);
                self.start += to_transfer;
                return to_transfer;
            }

            // If dest is large, read from the unbuffered reader directly into the destination.
            if (dest.len >= buffer_size) {
                return self.unbuffered_reader.read(dest);
            }

            // If dest is small, read from the unbuffered reader into our own internal buffer,
            // and then transfer to destination.
            self.end = try self.unbuffered_reader.read(&self.buf);
            const to_transfer = @min(self.end, dest.len);
            @memcpy(dest[0..to_transfer], self.buf[0..to_transfer]);
            self.start = to_transfer;
            return to_transfer;
        }

        pub fn reader(self: *Self) Reader {
            return .{ .context = self };
        }
    };
}

pub fn bufferedReader(reader: anytype) BufferedReader(4096, @TypeOf(reader)) {
    return .{ .unbuffered_reader = reader };
}

pub fn bufferedReaderSize(comptime size: usize, reader: anytype) BufferedReader(size, @TypeOf(reader)) {
    return .{ .unbuffered_reader = reader };
}

pub fn BufferedWriter(comptime buffer_size: usize, comptime WriterType: type) type {
    return struct {
        unbuffered_writer: WriterType,
        buf: [buffer_size]u8 = undefined,
        end: usize = 0,

        pub const Error = WriterType.Error;
        pub const Writer = io.Writer(*Self, Error, write);

        const Self = @This();

        pub fn flush(self: *Self) !void {
            try self.unbuffered_writer.writeAll(self.buf[0..self.end]);
            self.end = 0;
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            if (self.end + bytes.len > self.buf.len) {
                try self.flush();
                if (bytes.len > self.buf.len)
                    return self.unbuffered_writer.write(bytes);
            }

            const new_end = self.end + bytes.len;
            @memcpy(self.buf[self.end..new_end], bytes);
            self.end = new_end;
            return bytes.len;
        }
    };
}

pub fn bufferedWriter(underlying_stream: anytype) BufferedWriter(4096, @TypeOf(underlying_stream)) {
    return .{ .unbuffered_writer = underlying_stream };
}

/// A Writer that counts how many bytes has been written to it.
pub fn CountingWriter(comptime WriterType: type) type {
    return struct {
        bytes_written: u64,
        child_stream: WriterType,

        pub const Error = WriterType.Error;
        pub const Writer = io.Writer(*Self, Error, write);

        const Self = @This();

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            const amt = try self.child_stream.write(bytes);
            self.bytes_written += amt;
            return amt;
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }
    };
}

pub fn countingWriter(child_stream: anytype) CountingWriter(@TypeOf(child_stream)) {
    return .{ .bytes_written = 0, .child_stream = child_stream };
}

test CountingWriter {
    var counting_stream = countingWriter(std.io.null_writer);
    const stream = counting_stream.writer();

    const bytes = "yay" ** 100;
    stream.writeAll(bytes) catch unreachable;
    try testing.expect(counting_stream.bytes_written == bytes.len);
}

pub fn LimitedReader(comptime ReaderType: type) type {
    return struct {
        inner_reader: ReaderType,
        bytes_left: u64,

        pub const Error = ReaderType.Error;
        pub const Reader = io.Reader(*Self, Error, read);

        const Self = @This();

        pub fn read(self: *Self, dest: []u8) Error!usize {
            const max_read = @min(self.bytes_left, dest.len);
            const n = try self.inner_reader.read(dest[0..max_read]);
            self.bytes_left -= n;
            return n;
        }

        pub fn reader(self: *Self) Reader {
            return .{ .context = self };
        }
    };
}

/// Returns an initialised `LimitedReader`.
/// `bytes_left` is a `u64` to be able to take 64 bit file offsets
pub fn limitedReader(inner_reader: anytype, bytes_left: u64) LimitedReader(@TypeOf(inner_reader)) {
    return .{ .inner_reader = inner_reader, .bytes_left = bytes_left };
}

test "LimitedReader" {
    const data = "hello world";
    var fbs = std.io.fixedBufferStream(data);
    var early_stream = limitedReader(fbs.reader(), 3);

    var buf: [5]u8 = undefined;
    try testing.expectEqual(@as(usize, 3), try early_stream.reader().read(&buf));
    try testing.expectEqualSlices(u8, data[0..3], buf[0..3]);
    try testing.expectEqual(@as(usize, 0), try early_stream.reader().read(&buf));
    try testing.expectError(error.EndOfStream, early_stream.reader().skipBytes(10, .{}));
}
