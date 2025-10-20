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
        pub const Reader = io.GenericReader(*Self, Error, read);

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
        pub const Writer = io.GenericWriter(*Self, Error, write);

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
        pub const Writer = io.GenericWriter(*Self, Error, write);

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
        pub const Reader = io.GenericReader(*Self, Error, read);

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

//General note on endianess:
//Big endian is packed starting in the most significant part of the byte and subsequent
// bytes contain less significant bits. Thus we write out bits from the high end
// of our input first.
//Little endian is packed starting in the least significant part of the byte and
// subsequent bytes contain more significant bits. Thus we write out bits from
// the low end of our input first.
//Regardless of endianess, within any given byte the bits are always in most
// to least significant order.
//Also regardless of endianess, the buffer always aligns bits to the low end
// of the byte.

/// Creates a bit writer which allows for writing bits to an underlying standard writer
pub fn BitWriter(comptime endian: std.builtin.Endian, comptime Writer: type) type {
    return struct {
        writer: Writer,
        bits: u8 = 0,
        count: u4 = 0,

        const low_bit_mask = [9]u8{
            0b00000000,
            0b00000001,
            0b00000011,
            0b00000111,
            0b00001111,
            0b00011111,
            0b00111111,
            0b01111111,
            0b11111111,
        };

        /// Write the specified number of bits to the writer from the least significant bits of
        ///  the specified value. Bits will only be written to the writer when there
        ///  are enough to fill a byte.
        pub fn writeBits(self: *@This(), value: anytype, num: u16) !void {
            const T = @TypeOf(value);
            const UT = std.meta.Int(.unsigned, @bitSizeOf(T));
            const U = if (@bitSizeOf(T) < 8) u8 else UT; //<u8 is a pain to work with

            var in: U = @as(UT, @bitCast(value));
            var in_count: u16 = num;

            if (self.count > 0) {
                //if we can't fill the buffer, add what we have
                const bits_free = 8 - self.count;
                if (num < bits_free) {
                    self.addBits(@truncate(in), @intCast(num));
                    return;
                }

                //finish filling the buffer and flush it
                if (num == bits_free) {
                    self.addBits(@truncate(in), @intCast(num));
                    return self.flushBits();
                }

                switch (endian) {
                    .big => {
                        const bits = in >> @intCast(in_count - bits_free);
                        self.addBits(@truncate(bits), bits_free);
                    },
                    .little => {
                        self.addBits(@truncate(in), bits_free);
                        in >>= @intCast(bits_free);
                    },
                }
                in_count -= bits_free;
                try self.flushBits();
            }

            //write full bytes while we can
            const full_bytes_left = in_count / 8;
            for (0..full_bytes_left) |_| {
                switch (endian) {
                    .big => {
                        const bits = in >> @intCast(in_count - 8);
                        try self.writer.writeByte(@truncate(bits));
                    },
                    .little => {
                        try self.writer.writeByte(@truncate(in));
                        if (U == u8) in = 0 else in >>= 8;
                    },
                }
                in_count -= 8;
            }

            //save the remaining bits in the buffer
            self.addBits(@truncate(in), @intCast(in_count));
        }

        //convenience funciton for adding bits to the buffer
        //in the appropriate position based on endianess
        fn addBits(self: *@This(), bits: u8, num: u4) void {
            if (num == 8) self.bits = bits else switch (endian) {
                .big => {
                    self.bits <<= @intCast(num);
                    self.bits |= bits & low_bit_mask[num];
                },
                .little => {
                    const pos = bits << @intCast(self.count);
                    self.bits |= pos;
                },
            }
            self.count += num;
        }

        /// Flush any remaining bits to the writer, filling
        /// unused bits with 0s.
        pub fn flushBits(self: *@This()) !void {
            if (self.count == 0) return;
            if (endian == .big) self.bits <<= @intCast(8 - self.count);
            try self.writer.writeByte(self.bits);
            self.bits = 0;
            self.count = 0;
        }
    };
}

pub fn bitWriter(comptime endian: std.builtin.Endian, writer: anytype) BitWriter(endian, @TypeOf(writer)) {
    return .{ .writer = writer };
}

///////////////////////////////

test "BitWriter api coverage" {
    var mem_be = [_]u8{0} ** 2;
    var mem_le = [_]u8{0} ** 2;

    var mem_out_be = std.io.fixedBufferStream(&mem_be);
    var bit_stream_be = bitWriter(.big, mem_out_be.writer());

    try bit_stream_be.writeBits(@as(u2, 1), 1);
    try bit_stream_be.writeBits(@as(u5, 2), 2);
    try bit_stream_be.writeBits(@as(u128, 3), 3);
    try bit_stream_be.writeBits(@as(u8, 4), 4);
    try bit_stream_be.writeBits(@as(u9, 5), 5);
    try bit_stream_be.writeBits(@as(u1, 1), 1);

    try testing.expect(mem_be[0] == 0b11001101 and mem_be[1] == 0b00001011);

    mem_out_be.pos = 0;

    try bit_stream_be.writeBits(@as(u15, 0b110011010000101), 15);
    try bit_stream_be.flushBits();
    try testing.expect(mem_be[0] == 0b11001101 and mem_be[1] == 0b00001010);

    mem_out_be.pos = 0;
    try bit_stream_be.writeBits(@as(u32, 0b110011010000101), 16);
    try testing.expect(mem_be[0] == 0b01100110 and mem_be[1] == 0b10000101);

    try bit_stream_be.writeBits(@as(u0, 0), 0);

    var mem_out_le = std.io.fixedBufferStream(&mem_le);
    var bit_stream_le = bitWriter(.little, mem_out_le.writer());

    try bit_stream_le.writeBits(@as(u2, 1), 1);
    try bit_stream_le.writeBits(@as(u5, 2), 2);
    try bit_stream_le.writeBits(@as(u128, 3), 3);
    try bit_stream_le.writeBits(@as(u8, 4), 4);
    try bit_stream_le.writeBits(@as(u9, 5), 5);
    try bit_stream_le.writeBits(@as(u1, 1), 1);

    try testing.expect(mem_le[0] == 0b00011101 and mem_le[1] == 0b10010101);

    mem_out_le.pos = 0;
    try bit_stream_le.writeBits(@as(u15, 0b110011010000101), 15);
    try bit_stream_le.flushBits();
    try testing.expect(mem_le[0] == 0b10000101 and mem_le[1] == 0b01100110);

    mem_out_le.pos = 0;
    try bit_stream_le.writeBits(@as(u32, 0b1100110100001011), 16);
    try testing.expect(mem_le[0] == 0b00001011 and mem_le[1] == 0b11001101);

    try bit_stream_le.writeBits(@as(u0, 0), 0);
}
