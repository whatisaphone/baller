const std = @import("std");

pub fn requireEof(s: anytype) !void {
    _ = s.reader().readByte() catch |err| switch (err) {
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

pub fn XorReader(Stream: type) type {
    return struct {
        stream: Stream,
        key: u8,

        const Reader = std.io.GenericReader(*const @This(), Stream.Error, read);

        pub fn reader(self: *const @This()) Reader {
            return .{ .context = self };
        }

        fn read(self: *const @This(), dest: []u8) Stream.Error!usize {
            const len = try self.stream.read(dest);
            for (dest[0..len]) |*p|
                p.* ^= self.key;
            return len;
        }
    };
}

pub fn xorReader(stream: anytype, key: u8) XorReader(@TypeOf(stream)) {
    return .{ .stream = stream, .key = key };
}
