const std = @import("std");

const io = @import("io.zig");

pub fn main() !void {
    if (std.os.argv.len != 1 + 2)
        return error.CommandLine;

    const input_path = std.os.argv[1];
    const output_path = std.mem.sliceTo(std.os.argv[2], 0);

    const input_file = try std.fs.cwd().openFileZ(input_path, .{});
    defer input_file.close();

    std.fs.cwd().makeDirZ(output_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const input_xor_reader = io.xorReader(input_file.reader(), 0x69);
    const input_buf_reader = std.io.bufferedReader(input_xor_reader.reader());
    var input_reader = std.io.countingReader(input_buf_reader);

    var state = try State.init(output_path);
    try extract(&input_reader, &state);
}

const State = struct {
    cur_path: std.BoundedArray(u8, 4095) = .{},

    fn init(output_path: []const u8) !State {
        var result = State{
            .cur_path = .{},
        };
        try result.cur_path.appendSlice(output_path);
        try result.cur_path.append('/');
        return result;
    }
};

fn extract(stream: anytype, state: *State) !void {
    var blocks = blockReader(stream);

    while (true) {
        const id, const len = try blocks.next() orelse break;

        const orig_path_len = state.cur_path.len;
        defer state.cur_path.len = orig_path_len;

        try state.cur_path.appendSlice(&blockIdToStr(id));
        try state.cur_path.appendSlice(".bin\x00");
        const cur_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];

        const output_file = try std.fs.cwd().createFileZ(cur_path, .{});
        defer output_file.close();

        try io.copy(std.io.limitedReader(stream.reader(), len), output_file);
    }

    try blocks.checkSync();
}

const BlockId = u32;
const block_id_len = @sizeOf(BlockId);

const blockIdToStr = std.mem.toBytes;

fn blockReader(stream: anytype) BlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn BlockReader(Stream: type) type {
    return struct {
        const Self = @This();

        stream: Stream,
        current_block_end: ?u32 = null,

        fn next(self: *Self) !?struct { BlockId, u32 } {
            try self.checkSync();

            var id_bytes: [block_id_len]u8 = undefined;
            switch (try self.stream.reader().readAll(&id_bytes)) {
                0 => return null,
                block_id_len => {},
                else => return error.EndOfStream,
            }
            const id = std.mem.bytesToValue(BlockId, &id_bytes);

            const full_len = try self.stream.reader().readInt(u32, .big);
            // The original value includes the id and length, but the caller
            // doesn't care about those, so subtract them out.
            const len = full_len - 8;

            const current_pos: u32 = @intCast(self.stream.bytes_read);
            self.current_block_end = current_pos + len;

            return .{ id, len };
        }

        fn checkSync(self: *const Self) !void {
            const current_block_end = self.current_block_end orelse return;
            if (self.stream.bytes_read != current_block_end)
                return error.BlockDesync;
        }
    };
}
