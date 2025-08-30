// Copy/paste of some zig 0.14 streams as a stopgap during migration to 0.15

const std = @import("std");

const io = std.io;
const mem = std.mem;
const testing = std.testing;

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
