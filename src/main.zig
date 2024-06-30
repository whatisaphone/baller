const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const fmtBlockId = @import("block_id.zig").fmtBlockId;
const build = @import("build.zig");
const fs = @import("fs.zig");
const io = @import("io.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    if (std.os.argv.len < 1 + 1)
        return error.CommandLine;

    const command = std.mem.sliceTo(std.os.argv[1], 0);
    if (std.mem.eql(u8, command, "build")) {
        try build.run(allocator);
    } else if (std.mem.eql(u8, command, "extract")) {
        try extract(allocator);
    } else {
        return error.CommandLine;
    }
}

fn extract(allocator: std.mem.Allocator) !void {
    if (std.os.argv.len != 1 + 1 + 2)
        return error.CommandLine;

    const input_path = std.mem.sliceTo(std.os.argv[2], 0);
    const output_path = std.mem.sliceTo(std.os.argv[3], 0);

    if (!std.mem.endsWith(u8, input_path, ".he0"))
        return error.CommandLine;

    try fs.makeDirIfNotExistZ(std.fs.cwd(), output_path);

    var index = try readIndex(allocator, input_path);
    defer index.deinit(allocator);

    const project_txt_path = try std.fmt.allocPrintZ(
        allocator,
        "{s}/project.txt",
        .{output_path},
    );
    defer allocator.free(project_txt_path);

    const project_txt_file = try std.fs.cwd().createFileZ(project_txt_path, .{});
    defer project_txt_file.close();

    var project_txt = std.io.bufferedWriter(project_txt_file.writer());

    // input_path will be modified to point to each disk file
    // e.g. "baseball 2001.(a)"
    input_path[input_path.len - 3] = '(';
    input_path[input_path.len - 1] = ')';

    for (1..2 + 1) |disk_number_usize| {
        const disk_number: u8 = @intCast(disk_number_usize);
        input_path[input_path.len - 2] = 'a' - 1 + disk_number;

        try extractDisk(
            allocator,
            input_path,
            output_path,
            disk_number,
            &project_txt,
            &index,
        );
    }

    try project_txt.flush();
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

const Index = struct {
    lfl_offsets: []u32,
    lfl_disks: []u8,
    room_name_buf: []u8,
    room_name_starts: []u16,
    room_name_lens: []u8,

    fn deinit(self: *const Index, allocator: std.mem.Allocator) void {
        allocator.free(self.room_name_lens);
        allocator.free(self.room_name_starts);
        allocator.free(self.room_name_buf);
        allocator.free(self.lfl_disks);
        allocator.free(self.lfl_offsets);
    }

    fn roomName(self: *const Index, room_number: u8) ![]const u8 {
        const room_index = room_number - 1;
        if (room_index >= self.room_name_starts.len)
            return error.NotFound;

        const len = self.room_name_lens[room_index];
        // Missing room name is indicated with len == 0
        if (len == 0)
            return error.NotFound;
        const start = self.room_name_starts[room_index];
        return self.room_name_buf[start .. start + len];
    }
};

const Maxs = extern struct {
    variables: u16,
    unknown1: u16,
    room_variables: u16,
    objects_in_room: u16,
    arrays: u16,
    unknown2: u16,
    unknown3: u16,
    flobjects: u16,
    inventory_objects: u16,
    rooms: u16,
    scripts: u16,
    sounds: u16,
    charsets: u16,
    costumes: u16,
    objects: u16,
    images: u16,
    sprites: u16,
    local_scripts: u16,
    heap: u16,
    palettes: u16,
    unknown4: u16,
    talkies: u16,
};

fn readIndex(allocator: std.mem.Allocator, path: [*:0]u8) !Index {
    const file = try std.fs.cwd().openFileZ(path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), build.xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);
    const in = reader.reader();

    var blocks = blockReader(&reader);

    // MAXS

    const maxs_len = try blocks.expectBlock("MAXS");
    if (maxs_len != @sizeOf(Maxs))
        return error.BadData;
    var maxs: Maxs = undefined;
    try in.readNoEof(std.mem.asBytes(&maxs));
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    // DLFL

    const dlfl_len = try blocks.skipUntilBlock("DLFL");
    if (dlfl_len != 2 + maxs.rooms * 4)
        return error.BadData;
    const dlfl_count = try in.readInt(u16, .little);
    if (dlfl_count != maxs.rooms)
        return error.BadData;

    const dlfl = try allocator.alloc(u32, dlfl_count);
    errdefer allocator.free(dlfl);

    try in.readNoEof(std.mem.sliceAsBytes(dlfl));
    std.debug.assert(builtin.cpu.arch.endian() == .little);

    // DISK

    const disk_len = try blocks.expectBlock("DISK");
    if (disk_len != 2 + maxs.rooms * 1)
        return error.BadData;
    const disk_count = try in.readInt(u16, .little);
    if (disk_count != maxs.rooms)
        return error.BadData;

    const disk = try allocator.alloc(u8, disk_count);
    errdefer allocator.free(disk);

    try in.readNoEof(disk);

    // RNAM

    const rnam_len = try blocks.expectBlock("RNAM");

    var room_name_buf = try allocator.alloc(u8, rnam_len);
    errdefer allocator.free(room_name_buf);

    var room_name_starts = try allocator.alloc(u16, maxs.rooms);
    errdefer allocator.free(room_name_starts);

    var room_name_lens = try allocator.alloc(u8, maxs.rooms);
    errdefer allocator.free(room_name_lens);

    var room_name_buf_pos: u16 = 0;
    // Missing room name is indicated with len == 0
    @memset(room_name_lens, 0);

    while (true) {
        const room_number = try in.readInt(u16, .little);
        if (room_number == 0) // terminator
            break;

        const room_index_u16 = try std.math.sub(u16, room_number, 1);
        const room_index = std.math.cast(u8, room_index_u16) orelse
            return error.Overflow;

        room_name_starts[room_index] = room_name_buf_pos;

        while (true) {
            const n = try in.readByte();
            if (n == 0) // null terminated
                break;
            if (room_name_buf_pos >= room_name_buf.len)
                return error.BadData;
            room_name_buf[room_name_buf_pos] = n;
            room_name_buf_pos += 1;
        }

        const name_len = room_name_buf_pos - room_name_starts[room_index];
        room_name_lens[room_index] = std.math.cast(u8, name_len) orelse
            return error.Overflow;
    }

    return Index{
        .lfl_offsets = dlfl,
        .lfl_disks = disk,
        .room_name_buf = room_name_buf,
        .room_name_starts = room_name_starts,
        .room_name_lens = room_name_lens,
    };
}

fn extractDisk(
    allocator: std.mem.Allocator,
    input_path: [*:0]u8,
    output_path: []const u8,
    disk_number: u8,
    project_txt: anytype,
    index: *const Index,
) !void {
    const file = try std.fs.cwd().openFileZ(input_path, .{});
    defer file.close();

    const xor_reader = io.xorReader(file.reader(), build.xor_key);
    const buf_reader = std.io.bufferedReader(xor_reader.reader());
    var reader = std.io.countingReader(buf_reader);
    const in = reader.reader();

    var state = try State.init(output_path);

    var file_blocks = blockReader(&reader);

    const lecf_len = try file_blocks.expectBlock("LECF");
    const lecf_end = reader.bytes_read + lecf_len;

    var lecf_blocks = blockReader(&reader);

    while (reader.bytes_read < lecf_end) {
        const lflf_len = try lecf_blocks.expectBlock("LFLF");
        const lflf_end = reader.bytes_read + lflf_len;

        const room_number =
            for (0.., index.lfl_disks, index.lfl_offsets) |i, disk, offset|
        {
            if (disk == disk_number and offset == reader.bytes_read)
                break @as(u8, @intCast(i));
        } else return error.BadData;

        const room_name = index.roomName(room_number) catch
            return error.BadData;

        try project_txt.writer().print("room {} {} {s}\n", .{ disk_number, room_number, room_name });

        const before_room_path_len = state.cur_path.len;
        defer state.cur_path.len = before_room_path_len;

        try state.cur_path.appendSlice(room_name);
        try state.cur_path.append('\x00');
        const room_dir_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];
        try fs.makeDirIfNotExistZ(std.fs.cwd(), room_dir_path);
        state.cur_path.buffer[state.cur_path.len - 1] = '/';

        var lflf_blocks = blockReader(&reader);

        var block_numbers = std.AutoArrayHashMapUnmanaged(BlockId, u16){};
        defer block_numbers.deinit(allocator);

        while (reader.bytes_read < lflf_end) {
            const id, const len = try lflf_blocks.next();

            const block_number_entry = try block_numbers.getOrPutValue(allocator, id, 0);
            block_number_entry.value_ptr.* += 1;
            const block_number = block_number_entry.value_ptr.*;

            const before_child_path_len = state.cur_path.len;
            defer state.cur_path.len = before_child_path_len;

            try state.cur_path.writer().print(
                "{s}_{:0>4}.bin\x00",
                .{ blockIdToStr(&id), block_number },
            );
            const cur_path = state.cur_path.buffer[0 .. state.cur_path.len - 1 :0];

            const output_file = try std.fs.cwd().createFileZ(cur_path, .{});
            defer output_file.close();

            try io.copy(std.io.limitedReader(in, len), output_file);
        }

        try lflf_blocks.checkSync();
    }

    try lecf_blocks.checkSync();

    try file_blocks.checkSync();

    try io.requireEof(&reader);
}

fn blockReader(stream: anytype) BlockReader(@TypeOf(stream)) {
    return .{ .stream = stream };
}

fn BlockReader(Stream: type) type {
    return struct {
        const Self = @This();

        stream: Stream,
        current_block_end: ?u32 = null,

        fn next(self: *Self) !struct { BlockId, u32 } {
            try self.checkSync();

            const id = try self.stream.reader().readInt(BlockId, .little);

            const full_len = try self.stream.reader().readInt(u32, .big);
            // The original value includes the id and length, but the caller
            // doesn't care about those, so subtract them out.
            const len = full_len - 8;

            const current_pos: u32 = @intCast(self.stream.bytes_read);
            self.current_block_end = current_pos + len;

            return .{ id, len };
        }

        fn expect(self: *Self, expected_id: BlockId) !u32 {
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

        fn expectBlock(self: *Self, comptime expected_id: []const u8) !u32 {
            const id = comptime blockId(expected_id);
            return self.expect(id);
        }

        fn skipUntil(self: *Self, block_id: BlockId) !u32 {
            while (true) {
                const id, const len = try self.next();
                if (id != block_id) {
                    try self.stream.reader().skipBytes(len, .{});
                    continue;
                }
                return len;
            }
        }

        fn skipUntilBlock(self: *Self, comptime block_id: []const u8) !u32 {
            const id = comptime blockId(block_id);
            return self.skipUntil(id);
        }

        fn checkSync(self: *const Self) !void {
            const current_block_end = self.current_block_end orelse return;
            if (self.stream.bytes_read != current_block_end)
                return error.BlockDesync;
        }
    };
}
