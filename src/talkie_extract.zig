const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const blockReader = @import("block_reader.zig").blockReader;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const fs = @import("fs.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");
const wav = @import("wav.zig");

pub fn runCli(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len != 2)
        return error.CommandLine;

    const input_path = args[0];
    const output_path = args[1];

    try run(allocator, &.{
        .input_path = input_path,
        .output_path = output_path,
    });
}

const Extract = struct {
    input_path: [:0]const u8,
    output_path: [:0]const u8,
};

pub fn run(allocator: std.mem.Allocator, args: *const Extract) !void {
    const in_file = try std.fs.cwd().openFileZ(args.input_path, .{});
    defer in_file.close();
    var buf_reader = std.io.bufferedReader(in_file.reader());
    var reader = std.io.countingReader(buf_reader.reader());

    var cur_path_buf = std.BoundedArray(u8, 4095){};
    const out_dir = try pathf.append(&cur_path_buf, args.output_path);
    try fs.makeDirIfNotExistZ(std.fs.cwd(), out_dir.full());
    try cur_path_buf.append('/');

    var block_seqs = std.AutoArrayHashMapUnmanaged(BlockId, u32){};
    defer block_seqs.deinit(allocator);

    var state = State{
        .reader = &reader,
        .reader_pos = &reader.bytes_read,
        .block_seqs = &block_seqs,
        .cur_path = &cur_path_buf,
        .path_rel_start = cur_path_buf.len,
        .manifest = .{},
        .indent = 0,
        .block_buf = .{},
    };
    defer state.block_buf.deinit(allocator);
    defer state.manifest.deinit(allocator);

    var file_blocks = blockReader(&reader);

    const tlkb_len = try file_blocks.expectBlock("TLKB");
    try parseTlkb(allocator, tlkb_len, &state);

    try file_blocks.finishEof();

    const path = try pathf.append(&cur_path_buf, "talkies.txt");
    defer path.restore();
    try fs.writeFileZ(std.fs.cwd(), path.full(), state.manifest.items);
}

const State = struct {
    reader: *std.io.CountingReader(std.io.BufferedReader(4096, std.fs.File.Reader).Reader),
    reader_pos: *const u64,
    block_seqs: *std.AutoArrayHashMapUnmanaged(BlockId, u32),
    cur_path: *std.BoundedArray(u8, 4095),
    path_rel_start: u32,
    manifest: std.ArrayListUnmanaged(u8),
    indent: u8,
    block_buf: std.ArrayListUnmanaged(u8),

    fn readerPos(self: *const State) u32 {
        return @intCast(self.reader_pos.*);
    }

    fn fillBlockBuf(self: *State, allocator: std.mem.Allocator, block_len: u32) ![]u8 {
        std.debug.assert(self.block_buf.items.len == 0);
        try self.block_buf.ensureTotalCapacity(allocator, block_len);
        const buf = self.block_buf.addManyAsSliceAssumeCapacity(block_len);
        try self.reader.reader().readNoEof(buf);
        return buf;
    }

    fn doneWithBlockBuf(self: *State) void {
        self.block_buf.clearRetainingCapacity();
    }

    fn nextSeq(self: *State, allocator: std.mem.Allocator, block_id: BlockId) !u32 {
        const seq_entry = try self.block_seqs.getOrPutValue(allocator, block_id, 0);
        seq_entry.value_ptr.* += 1;
        return seq_entry.value_ptr.*;
    }

    fn curPathRelative(self: *const State) [:0]const u8 {
        return self.cur_path.buffer[self.path_rel_start..self.cur_path.len :0];
    }

    fn writeIndent(self: *State, allocator: std.mem.Allocator) !void {
        try self.writeIndentTo(self.manifest.writer(allocator));
    }

    fn writeIndentTo(self: *const State, writer: anytype) !void {
        for (0..self.indent * 4) |_|
            try writer.writeByte(' ');
    }
};

const StreamingBlockParser = *const fn (
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_len: u32,
    state: *State,
) anyerror!void;

fn FixedBlockParser(Cx: type) type {
    return *const fn (
        allocator: std.mem.Allocator,
        block_id: BlockId,
        block_raw: []const u8,
        cx: Cx,
        state: *State,
    ) anyerror!void;
}

const StreamingBlockParserForId = struct {
    block_id: BlockId,
    parser: StreamingBlockParser,
};

/// Try decoding the block, and if that fails, dump it as raw data instead.
fn parseFixedFallback(
    Cx: type,
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_raw: []const u8,
    parser: FixedBlockParser(Cx),
    cx: Cx,
    state: *State,
) !void {
    const prev_manifest_len = state.manifest.items.len;
    const prev_indent = state.indent;

    if (parser(allocator, block_id, block_raw, cx, state))
        return
    else |err| if (err != error.BlockFallbackToRaw)
        return err;

    // Rollback
    state.manifest.shrinkRetainingCapacity(prev_manifest_len);
    state.indent = prev_indent;

    try parseFixedRaw(allocator, block_id, block_raw, state);
}

fn parseChildBlocks(
    allocator: std.mem.Allocator,
    parent_len: u32,
    parsers: []const StreamingBlockParserForId,
    state: *State,
) !void {
    const parent_end = state.readerPos() + parent_len;

    var blocks = blockReader(state.reader);

    while (state.readerPos() < parent_end) {
        const block_id, const block_len = try blocks.next();

        const parser = for (parsers) |pair| {
            if (pair.block_id == block_id)
                break pair.parser;
        } else parseStreamingRaw;

        try parser(allocator, block_id, block_len, state);
    }

    try blocks.finish(parent_end);
}

fn parseStreamingRaw(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_len: u32,
    state: *State,
) !void {
    const seq = try state.nextSeq(allocator, block_id);

    const path = try pathf.appendBlockPath(state.cur_path, block_id, seq, "bin");
    defer path.restore();

    const file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer file.close();
    try io.copy(std.io.limitedReader(state.reader.reader(), block_len), file);

    try state.writeIndent(allocator);
    try state.manifest.writer(allocator).print(
        "raw-block {s} {s}\n",
        .{ blockIdToStr(&block_id), state.curPathRelative() },
    );
}

fn parseFixedRaw(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    block_raw: []const u8,
    state: *State,
) !void {
    const seq = try state.nextSeq(allocator, block_id);

    const path = try pathf.appendBlockPath(state.cur_path, block_id, seq, "bin");
    defer path.restore();

    try fs.writeFileZ(std.fs.cwd(), path.full(), block_raw);

    try state.writeIndent(allocator);
    try state.manifest.writer(allocator).print(
        "raw-block {s} {s}\n",
        .{ blockIdToStr(&block_id), state.curPathRelative() },
    );
}

fn parseTlkb(allocator: std.mem.Allocator, tlkb_len: u32, state: *State) !void {
    try parseChildBlocks(allocator, tlkb_len, tlkb_children, state);
}

const tlkb_children: []const StreamingBlockParserForId = &.{
    .{ .block_id = blockId("TALK"), .parser = parseTalk },
};

fn parseTalk(
    allocator: std.mem.Allocator,
    block_id: BlockId,
    talk_len: u32,
    state: *State,
) !void {
    // Subtract 8 to point at block header again
    const offset = state.readerPos() - 8;

    const raw = try state.fillBlockBuf(allocator, talk_len);
    defer state.doneWithBlockBuf();

    try parseFixedFallback(u32, allocator, block_id, raw, parseTalkFixed, offset, state);
}

fn parseTalkFixed(
    allocator: std.mem.Allocator,
    _: BlockId,
    talk_raw: []const u8,
    talk_offset: u32,
    state: *State,
) !void {
    var talk_stream = std.io.fixedBufferStream(talk_raw);
    var talk_blocks = fixedBlockReader(&talk_stream);

    try state.writeIndent(allocator);
    try state.manifest.writer(allocator).print("talk ; T{},{}\n", .{
        talk_offset,
        talk_raw.len + 8, // Add 8 so len includes block header
    });
    state.indent += 1;

    const talk_seq = try state.nextSeq(allocator, comptime blockId("TALK"));

    const path = try pathf.print(state.cur_path, "TALK_{:0>4}_", .{talk_seq});
    defer path.restore();

    while (try talk_blocks.peek() != comptime blockId("SDAT")) {
        const block_id, const block_len = try talk_blocks.next();
        // Soccer has one TALK block with some weird corrupt(?) data
        if (block_len > 0x00ff_ffff)
            return error.BlockFallbackToRaw;

        const block_raw = try io.readInPlace(&talk_stream, block_len);
        try parseFixedRaw(allocator, block_id, block_raw, state);
    }

    const sdat_len = try talk_blocks.assumeBlock("SDAT");
    const sdat_raw = try io.readInPlace(&talk_stream, sdat_len);

    const path2 = try pathf.append(state.cur_path, "SDAT.wav");
    defer path2.restore();
    const wav_file = try std.fs.cwd().createFileZ(path.full(), .{});
    defer wav_file.close();
    var wav_stream = std.io.bufferedWriter(wav_file.writer());

    try wav.writeHeader(sdat_len, wav_stream.writer());
    try wav_stream.writer().writeAll(sdat_raw);
    try wav_stream.flush();

    try state.writeIndent(allocator);
    try state.manifest.writer(allocator).print(
        "wav-sdat {} {s}\n",
        .{ sdat_raw.len, state.curPathRelative() },
    );

    try talk_blocks.finishEof();

    state.indent -= 1;
    try state.writeIndent(allocator);
    try state.manifest.appendSlice(allocator, "end-talk\n");
}
