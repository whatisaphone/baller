const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const parseBlockId = @import("block_id.zig").parseBlockId;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const writeFixups = @import("block_writer.zig").writeFixups;
const fs = @import("fs.zig");
const io = @import("io.zig");
const pathf = @import("pathf.zig");
const report = @import("report.zig");
const wav = @import("wav.zig");

pub fn runCli(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len != 2)
        return error.CommandLine;

    const manifest_path = args[0];
    const output_path = args[1];

    try run(allocator, &.{
        .manifest_path = manifest_path,
        .output_path = output_path,
    });
}

const Build = struct {
    manifest_path: [:0]const u8,
    output_path: [:0]const u8,
};

pub fn run(allocator: std.mem.Allocator, args: *const Build) !void {
    const manifest_file = try std.fs.cwd().openFileZ(args.manifest_path, .{});
    defer manifest_file.close();
    var manifest_reader = std.io.bufferedReader(manifest_file.reader());
    var line_buf: [255]u8 = undefined;

    var cur_path = std.BoundedArray(u8, 4095){};
    try cur_path.appendSlice(args.manifest_path);
    pathf.popFile(&cur_path);

    const output_file = try std.fs.cwd().createFileZ(args.output_path, .{});
    defer output_file.close();
    var output_buf = std.io.bufferedWriter(output_file.writer());
    var output_writer = std.io.countingWriter(output_buf.writer());

    var state = State{
        .manifest_reader = &manifest_reader,
        .line_buf = &line_buf,
        .cur_path = &cur_path,
        .output_writer = &output_writer,
        .fixups = std.ArrayList(Fixup).init(allocator),
    };
    defer state.fixups.deinit();

    const tlkb_start = try beginBlock(state.output_writer, "TLKB");

    while (true) {
        const line = manifest_reader.reader()
            .readUntilDelimiter(&line_buf, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (std.mem.startsWith(u8, line, "raw-block "))
            try buildRawBlock(&state, line[10..])
        else if (std.mem.eql(u8, line, "talk"))
            try buildTalk(&state)
        else
            return error.BadData;
    }

    try endBlock(state.output_writer, &state.fixups, tlkb_start);

    try output_buf.flush();

    try writeFixups(output_file, output_file.writer(), state.fixups.items);
}

const State = struct {
    manifest_reader: *std.io.BufferedReader(4096, std.fs.File.Reader),
    line_buf: *[255]u8,
    cur_path: *std.BoundedArray(u8, 4095),
    output_writer: *std.io.CountingWriter(std.io.BufferedWriter(4096, std.fs.File.Writer).Writer),
    fixups: std.ArrayList(Fixup),
};

fn buildRawBlock(state: *State, line: []const u8) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const block_id_str = tokens.next() orelse return error.BadData;
    const block_id = parseBlockId(block_id_str) orelse return error.BadData;

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Write block

    const start = try beginBlockImpl(state.output_writer, block_id);

    const path = try pathf.append(state.cur_path, relative_path);
    defer path.restore();

    try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.output_writer.writer());

    try endBlock(state.output_writer, &state.fixups, start);
}

fn buildTalk(state: *State) !void {
    const RawBlock = struct {
        id: BlockId,
        path: std.BoundedArray(u8, 255),
    };

    const Sdat = struct {
        expected_len: u32,
        path: std.BoundedArray(u8, 255),
    };

    // Parse lines

    var raw_blocks: std.BoundedArray(RawBlock, 2) = .{};
    var sdat_opt: ?Sdat = null;

    while (true) {
        const line =
            try state.manifest_reader.reader().readUntilDelimiter(state.line_buf, '\n');
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        const token = tokens.next() orelse return error.BadData;
        if (std.mem.eql(u8, token, "raw-block")) {
            const block_id_str = tokens.next() orelse return error.BadData;
            const block_id = parseBlockId(block_id_str) orelse return error.BadData;

            const path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            try raw_blocks.append(.{
                .id = block_id,
                .path = try std.BoundedArray(u8, 255).fromSlice(path),
            });
        } else if (std.mem.eql(u8, token, "wav-sdat") and sdat_opt == null) {
            const expected_len_str = tokens.next() orelse return error.BadData;
            const expected_len = try std.fmt.parseInt(u32, expected_len_str, 10);

            const path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            sdat_opt = .{
                .expected_len = expected_len,
                .path = try std.BoundedArray(u8, 255).fromSlice(path),
            };
        } else if (std.mem.eql(u8, token, "end-talk")) {
            break;
        } else {
            return error.BadData;
        }
    }

    const sdat = sdat_opt orelse return error.BadData;

    // Write block

    const talk_start = try beginBlock(state.output_writer, "TALK");

    for (raw_blocks.slice()) |raw_block| {
        const block_start = try beginBlockImpl(state.output_writer, raw_block.id);

        const raw_path = try pathf.append(state.cur_path, raw_block.path.slice());
        defer raw_path.restore();

        try fs.readFileIntoZ(std.fs.cwd(), raw_path.full(), state.output_writer.writer());

        try endBlock(state.output_writer, &state.fixups, block_start);
    }

    {
        const wav_path = try pathf.append(state.cur_path, sdat.path.slice());
        defer wav_path.restore();
        const wav_file = try std.fs.cwd().openFileZ(wav_path.full(), .{});
        defer wav_file.close();
        var wav_reader = std.io.bufferedReader(wav_file.reader());

        const wav_header = try wav.readHeader(wav_reader.reader());
        if (wav_header.channels != 1 or
            wav_header.samples_per_sec != 11025 or
            wav_header.bits_per_sample != 8)
        {
            report.fatal("{s} must be 11025 KHz 8-bit mono", .{wav_path.full()});
            return error.BadData;
        }

        const data_size = try wav.findData(wav_reader.reader());
        if (data_size != sdat.expected_len) {
            report.fatal(
                "{s} must be {} samples long",
                .{ wav_path.full(), sdat.expected_len },
            );
            return error.BadData;
        }

        const sdat_start = try beginBlock(state.output_writer, "SDAT");
        try io.copy(
            std.io.limitedReader(wav_reader, data_size),
            state.output_writer.writer(),
        );
        try endBlock(state.output_writer, &state.fixups, sdat_start);
    }

    try endBlock(state.output_writer, &state.fixups, talk_start);
}
