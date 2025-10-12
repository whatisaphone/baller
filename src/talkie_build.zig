const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const BlockId = @import("block_id.zig").BlockId;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const writeFixups = @import("block_writer.zig").writeFixups;
const BoundedArray = @import("bounded_array.zig").BoundedArray;
const fs = @import("fs.zig");
const io = @import("io.zig");
const iold = @import("iold.zig");
const pathf = @import("pathf.zig");
const wav = @import("wav.zig");

pub fn runCli(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len != 2)
        return error.CommandLine;

    const manifest_path = args[0];
    const output_path = args[1];

    var diagnostic: Diagnostic = .init(allocator);
    defer diagnostic.deinit();

    run(allocator, &diagnostic, &.{
        .manifest_path = manifest_path,
        .output_path = output_path,
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
    };
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

const Build = struct {
    manifest_path: [:0]const u8,
    output_path: [:0]const u8,
};

pub fn run(allocator: std.mem.Allocator, diagnostic: *Diagnostic, args: *const Build) !void {
    const manifest_file = try std.fs.cwd().openFileZ(args.manifest_path, .{});
    defer manifest_file.close();
    var buf: [4096]u8 = undefined;
    var reader = manifest_file.reader(&buf);

    var cur_path: pathf.Path = .{};
    try cur_path.appendSlice(args.manifest_path);
    try pathf.popFile(&cur_path);

    const output_file = try std.fs.cwd().createFileZ(args.output_path, .{});
    defer output_file.close();
    var output_buf = iold.bufferedWriter(output_file.deprecatedWriter());
    var output_writer = iold.countingWriter(output_buf.writer());

    var state: State = .{
        .diagnostic = diagnostic,
        .manifest_reader = &reader.interface,
        .cur_path = &cur_path,
        .output_writer = &output_writer,
        .fixups = .init(allocator),
    };
    defer state.fixups.deinit();

    const tlkb_start = try beginBlock(state.output_writer, .TLKB);

    while (true) {
        const line = try reader.interface.takeDelimiter('\n') orelse break;
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        const keyword = tokens.next() orelse return error.BadData;
        if (std.mem.eql(u8, keyword, "raw-block"))
            try buildRawBlock(&state, tokens.rest())
        else if (std.mem.eql(u8, keyword, "talk"))
            try buildTalk(&state)
        else
            return error.BadData;
    }

    try endBlock(state.output_writer, &state.fixups, tlkb_start);

    try output_buf.flush();

    try writeFixups(output_file, output_file.deprecatedWriter(), state.fixups.items);
}

const State = struct {
    diagnostic: *Diagnostic,
    manifest_reader: *std.io.Reader,
    cur_path: *pathf.Path,
    output_writer: *iold.CountingWriter(iold.BufferedWriter(4096, std.fs.File.DeprecatedWriter).Writer),
    fixups: std.array_list.Managed(Fixup),
};

fn buildRawBlock(state: *State, line: []const u8) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const block_id_str = tokens.next() orelse return error.BadData;
    const block_id = BlockId.parse(block_id_str) orelse return error.BadData;

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Write block

    const start = try beginBlock(state.output_writer, block_id);

    const path = try pathf.append(state.cur_path, relative_path);
    defer path.restore();

    try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.output_writer.writer());

    try endBlock(state.output_writer, &state.fixups, start);
}

fn buildTalk(state: *State) !void {
    const RawBlock = struct {
        id: BlockId,
        path: BoundedArray(u8, 255),
    };

    const Sdat = struct {
        expected_len: u32,
        path: BoundedArray(u8, 255),
    };

    // Parse lines

    var raw_blocks: BoundedArray(RawBlock, 2) = .{};
    var sdat_opt: ?Sdat = null;

    while (true) {
        const line = try state.manifest_reader.takeDelimiterExclusive('\n');
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        const token = tokens.next() orelse return error.BadData;
        if (std.mem.eql(u8, token, "raw-block")) {
            const block_id_str = tokens.next() orelse return error.BadData;
            const block_id = BlockId.parse(block_id_str) orelse return error.BadData;

            const path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            try raw_blocks.append(.{
                .id = block_id,
                .path = try .fromSlice(path),
            });
        } else if (std.mem.eql(u8, token, "wav-sdat") and sdat_opt == null) {
            const expected_len_str = tokens.next() orelse return error.BadData;
            const expected_len = try std.fmt.parseInt(u32, expected_len_str, 10);

            const path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            sdat_opt = .{
                .expected_len = expected_len,
                .path = try .fromSlice(path),
            };
        } else if (std.mem.eql(u8, token, "end-talk")) {
            break;
        } else {
            return error.BadData;
        }
    }

    const sdat = sdat_opt orelse return error.BadData;

    // Write block

    const talk_start = try beginBlock(state.output_writer, .TALK);

    for (raw_blocks.slice()) |raw_block| {
        const block_start = try beginBlock(state.output_writer, raw_block.id);

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
        var wav_reader = iold.bufferedReader(wav_file.deprecatedReader());

        const wav_header = try wav.readHeader(wav_reader.reader());
        if (wav_header.channels != 1 or
            wav_header.samples_per_sec != 11025 or
            wav_header.bits_per_sample != 8)
        {
            state.diagnostic.err("{s} must be 11025 KHz 8-bit mono", .{wav_path.full()});
            return error.AddedToDiagnostic;
        }

        const data_size = try wav.findData(wav_reader.reader());
        if (data_size != sdat.expected_len) {
            state.diagnostic.err(
                "{s} must be {} samples long",
                .{ wav_path.full(), sdat.expected_len },
            );
            return error.AddedToDiagnostic;
        }

        const sdat_start = try beginBlock(state.output_writer, .SDAT);
        try io.copy(
            iold.limitedReader(wav_reader, data_size),
            state.output_writer.writer(),
        );
        try endBlock(state.output_writer, &state.fixups, sdat_start);
    }

    try endBlock(state.output_writer, &state.fixups, talk_start);
}
