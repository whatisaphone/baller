const std = @import("std");

const blockIdToStr = @import("block_id.zig").blockIdToStr;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const fs = @import("fs.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");

pub fn decode(
    allocator: std.mem.Allocator,
    akos_raw: []const u8,
    cur_path: pathf.PrintedPath,
    manifest: ?*std.ArrayListUnmanaged(u8),
) !void {
    var stream = std.io.fixedBufferStream(akos_raw);
    var blocks = fixedBlockReader(&stream);

    while (stream.pos < akos_raw.len) {
        const block_id, const block_len = try blocks.next();
        const block_raw = try io.readInPlace(&stream, block_len);

        const block_path = try pathf.print(cur_path.buf, "{s}.bin", .{blockIdToStr(&block_id)});
        defer block_path.restore();

        try fs.writeFileZ(std.fs.cwd(), block_path.full(), block_raw);

        if (manifest) |m|
            try m.writer(allocator).print(
                "    raw-block {s} {s}\n",
                .{ blockIdToStr(&block_id), cur_path.relative() },
            );
    }

    try blocks.finishEof();
}
