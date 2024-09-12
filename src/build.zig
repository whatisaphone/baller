const std = @import("std");

const Symbols = @import("Symbols.zig");
const akos = @import("akos.zig");
const assemble = @import("assemble.zig");
const audio = @import("audio.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const parseBlockId = @import("block_id.zig").parseBlockId;
const Fixup = @import("block_writer.zig").Fixup;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const writeFixups = @import("block_writer.zig").writeFixups;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const lang = @import("lang.zig");
const pathf = @import("pathf.zig");
const rmim_encode = @import("rmim_encode.zig");
const utils = @import("utils.zig");

pub const xor_key = 0x69;

pub fn runCli(allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len != 2)
        return error.CommandLine;

    const project_txt_path = args[0];
    const output_path = args[1];

    try run(allocator, &.{
        .project_txt_path = project_txt_path,
        .output_path = output_path,
        .attribution = true,
    });
}

const Build = struct {
    project_txt_path: [:0]const u8,
    output_path: [:0]const u8,
    attribution: bool,
};

pub fn run(allocator: std.mem.Allocator, args: *const Build) !void {
    const project_txt_path = args.project_txt_path;

    var output_path_buf = pathf.Path{};
    try output_path_buf.appendSlice(args.output_path);
    try output_path_buf.append(0);
    const output_path = output_path_buf.buffer[0 .. output_path_buf.len - 1 :0];

    const game = try games.detectGameOrFatal(output_path);

    try fs.makeParentDirIfNotExist(std.fs.cwd(), output_path);

    const project_txt_file = try std.fs.cwd().openFileZ(project_txt_path, .{});
    defer project_txt_file.close();

    var project_txt_reader = std.io.bufferedReader(project_txt_file.reader());
    var project_txt_line_buf: [256]u8 = undefined;

    var prst = try ProjectState.init(allocator, game);
    defer prst.deinit(allocator);

    try prst.cur_path.appendSlice(project_txt_path);
    try pathf.popFile(&prst.cur_path);

    if (games.hasDisk(game))
        prst.index.lfl_disks = .{};

    // Room numbers start at 1, so zero out the first room.
    try prst.index.directories.rooms.append(allocator, .{
        .room = 0,
        .offset = 0,
        .len = 0,
    });

    // Globs start at 1, so 0 doesn't exist, so set the sizes to 0xffff_ffff.
    inline for (std.meta.fields(Directories)) |field| {
        // (except for DIRR, for some reason)
        if (!std.meta.eql(field.name, "rooms")) {
            try @field(prst.index.directories, field.name).append(allocator, .{
                .room = 0,
                .offset = 0,
                .len = 0xffff_ffff,
            });
        }
    }

    try readIndexBlobs(allocator, &prst);

    var cur_state: ?DiskState = null;
    defer if (cur_state) |*state|
        state.deinit();

    while (true) {
        const project_line = project_txt_reader.reader()
            .readUntilDelimiter(&project_txt_line_buf, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (std.mem.startsWith(u8, project_line, "symbols "))
            try loadSymbols(allocator, game, project_line[8..], &prst)
        else if (std.mem.startsWith(u8, project_line, "room "))
            try buildRoom(allocator, project_line[5..], &prst, &cur_state, output_path)
        else
            return error.BadData;
    }

    if (cur_state) |*state| {
        try finishDisk(state);
        state.deinit();
        cur_state = null;
    }

    try writeIndex(allocator, &prst, output_path, args.attribution);
}

fn loadSymbols(
    allocator: std.mem.Allocator,
    game: games.Game,
    relative_path: []const u8,
    prst: *ProjectState,
) !void {
    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();

    // don't load symbols more than once
    if (prst.symbols_text.len != 0)
        return error.BadData;

    prst.symbols_text = try fs.readFileZ(allocator, std.fs.cwd(), path.full());
    prst.symbols = try Symbols.parse(allocator, game, prst.symbols_text);
}

fn buildRoom(
    allocator: std.mem.Allocator,
    project_line: []const u8,
    prst: *ProjectState,
    cur_state: *?DiskState,
    output_path: [:0]u8,
) !void {
    var project_line_words = std.mem.splitScalar(u8, project_line, ' ');
    const disk_number_str = project_line_words.next() orelse return error.BadData;
    const room_number_str = project_line_words.next() orelse return error.BadData;
    const room_name = project_line_words.next() orelse return error.BadData;
    if (project_line_words.next()) |_| return error.BadData;

    const disk_number = try std.fmt.parseInt(u8, disk_number_str, 10);
    if (disk_number < 1 or disk_number > 26) return error.BadData;

    const room_number = try std.fmt.parseInt(u8, room_number_str, 10);
    if (room_number < 1) return error.BadData;

    try utils.growArrayList([]u8, &prst.index.room_names, allocator, room_number + 1, &.{});
    prst.index.room_names.items[room_number] = try allocator.dupe(u8, room_name);

    if (cur_state.*) |*state| if (state.disk_number != disk_number) {
        try finishDisk(state);
        state.deinit();
        cur_state.* = null;
    };

    if (cur_state.* == null) {
        cur_state.* = @as(DiskState, undefined); // TODO: is there a better way?
        try startDisk(allocator, disk_number, output_path, prst, &cur_state.*.?);
    }

    const state = &cur_state.*.?;

    try prst.cur_path.appendSlice(room_name);
    try prst.cur_path.append('/');
    defer prst.cur_path.len -= @intCast(room_name.len + 1);

    const room_file = room_file: {
        const path = try pathf.append(&prst.cur_path, "room.txt");
        defer path.restore();
        break :room_file try std.fs.cwd().openFileZ(path.full(), .{});
    };
    defer room_file.close();

    var room_reader = std.io.bufferedReader(room_file.reader());
    var room_line_buf: [1024]u8 = undefined;

    const lflf_fixup = try beginBlock(&state.writer, "LFLF");

    if (prst.index.lfl_disks) |*lfl_disks| {
        try utils.growArrayList(u8, lfl_disks, allocator, room_number + 1, 0);
        lfl_disks.items[room_number] = disk_number;
    } else {
        if (disk_number != 1)
            return error.BadData;
    }

    try utils.growArrayList(u32, &prst.index.lfl_offsets, allocator, room_number + 1, 0);
    prst.index.lfl_offsets.items[room_number] = @intCast(state.writer.bytes_written);

    while (true) {
        const room_line = room_reader.reader()
            .readUntilDelimiter(&room_line_buf, '\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        var room_line_split = std.mem.tokenizeScalar(u8, room_line, ' ');
        const keyword = room_line_split.next() orelse return error.BadData;
        if (std.mem.eql(u8, keyword, "raw-glob"))
            try handleRawGlob(
                allocator,
                room_number,
                room_line_split.rest(),
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "raw-block"))
            try handleRawBlock(room_line_split.rest(), prst, state)
        else if (std.mem.eql(u8, keyword, "rmda"))
            try handleRmda(
                allocator,
                room_number,
                &room_reader,
                &room_line_buf,
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "room-image"))
            try handleRoomImage(
                allocator,
                room_number,
                room_line_split.rest(),
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "scrp-asm"))
            try handleScrpAsm(
                allocator,
                room_number,
                room_line_split.rest(),
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "audio"))
            try handleAudio(
                allocator,
                room_number,
                room_line_split.rest(),
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "awiz"))
            try handleAwiz(
                allocator,
                room_number,
                room_line_split.rest(),
                &room_reader,
                &room_line_buf,
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "mult"))
            try handleMult(
                allocator,
                room_number,
                room_line_split.rest(),
                &room_reader,
                &room_line_buf,
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "wsou"))
            try handleWsou(
                allocator,
                room_number,
                room_line_split.rest(),
                prst,
                state,
            )
        else if (std.mem.eql(u8, keyword, "akos"))
            try handleAkos(
                allocator,
                room_number,
                room_line_split.rest(),
                &room_reader,
                &room_line_buf,
                prst,
                state,
            )
        else
            return error.BadData;
    }

    try endBlock(&state.writer, &state.fixups, lflf_fixup);
}

fn readIndexBlobs(allocator: std.mem.Allocator, prst: *ProjectState) !void {
    prst.index.maxs = try readIndexBlob(allocator, prst, "maxs.bin");
    prst.index.dobj = try readIndexBlob(allocator, prst, "dobj.bin");
    prst.index.aary = try readIndexBlob(allocator, prst, "aary.bin");
    if (games.hasIndexSver(prst.game))
        prst.index.sver = try readIndexBlob(allocator, prst, "sver.bin");
}

fn readIndexBlob(
    allocator: std.mem.Allocator,
    prst: *ProjectState,
    filename: []const u8,
) ![]u8 {
    const path = try pathf.append(&prst.cur_path, filename);
    defer path.restore();
    return fs.readFileZ(allocator, std.fs.cwd(), path.full());
}

fn startDisk(
    allocator: std.mem.Allocator,
    disk_number: u8,
    output_path: [:0]u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    games.pointPathToDisk(prst.game, output_path, disk_number);

    state.disk_number = disk_number;

    state.file = try std.fs.cwd().createFileZ(output_path, .{});
    errdefer state.file.close();

    state.xor_writer = io.xorWriter(state.file.writer(), xor_key);
    state.buf_writer = std.io.bufferedWriter(state.xor_writer.writer());
    state.writer = std.io.countingWriter(state.buf_writer.writer());

    state.fixups = std.ArrayList(Fixup).init(allocator);
    errdefer state.fixups.deinit();

    // Hardcode the fixup pos since it's always the same
    const lecf_start = try beginBlock(&state.writer, "LECF");
    std.debug.assert(lecf_start == 0);
}

fn finishDisk(state: *DiskState) !void {
    // End the LECF block
    try endBlock(&state.writer, &state.fixups, 0);

    try state.buf_writer.flush();

    try writeFixups(state.file, state.xor_writer.writer(), state.fixups.items);
}

fn handleRawGlob(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: []const u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    // Parse line

    var words = std.mem.splitScalar(u8, line, ' ');

    const block_id_str = words.next() orelse return error.BadData;
    const block_id = parseBlockId(block_id_str) orelse return error.BadData;

    const glob_number_str = words.next() orelse return error.BadData;
    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    const relative_path = words.next() orelse return error.BadData;

    if (words.next()) |_| return error.BadData;

    // Process block

    const block_fixup = try beginBlockImpl(&state.writer, block_id);

    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();

    try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.writer.writer());

    try endBlock(&state.writer, &state.fixups, block_fixup);
    const block_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        block_id,
        room_number,
        glob_number,
        block_fixup,
        block_len,
    );
}

fn handleRawBlock(
    line: []const u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const block_id_str = tokens.next() orelse return error.BadData;
    const block_id = parseBlockId(block_id_str) orelse return error.BadData;

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Process block

    const start = try beginBlockImpl(&state.writer, block_id);

    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();

    try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.writer.writer());

    try endBlock(&state.writer, &state.fixups, start);
}

fn handleRmda(
    allocator: std.mem.Allocator,
    room_number: u8,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    const rmda_fixup = try beginBlock(&state.writer, "RMDA");

    while (true) {
        const line = try room_reader.reader().readUntilDelimiter(room_line_buf, '\n');
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        const keyword = tokens.next() orelse return error.BadData;
        if (std.mem.eql(u8, keyword, "raw-block")) {
            const block_id_str = tokens.next() orelse return error.BadData;
            const block_id = parseBlockId(block_id_str) orelse return error.BadData;

            const relative_path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            const fixup = try beginBlockImpl(&state.writer, block_id);

            const path = try pathf.append(&prst.cur_path, relative_path);
            defer path.restore();

            try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.writer.writer());

            try endBlock(&state.writer, &state.fixups, fixup);
        } else if (std.mem.eql(u8, keyword, "encd-asm") or
            std.mem.eql(u8, keyword, "excd-asm"))
        {
            const block_id = if (std.mem.eql(u8, keyword, "encd-asm"))
                comptime blockId("ENCD")
            else if (std.mem.eql(u8, keyword, "excd-asm"))
                comptime blockId("EXCD")
            else
                unreachable;

            const relative_path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            const path = try pathf.append(&prst.cur_path, relative_path);
            defer path.restore();

            const asm_str = try fs.readFileZ(allocator, std.fs.cwd(), path.full());
            defer allocator.free(asm_str);

            const script_id: Symbols.ScriptId = switch (block_id) {
                blockId("ENCD") => .{ .enter = .{ .room = room_number } },
                blockId("EXCD") => .{ .exit = .{ .room = room_number } },
                else => unreachable,
            };
            var bytecode = try assemble.assemble(
                allocator,
                prst.languageStuff(),
                asm_str,
                &prst.symbols,
                script_id,
            );
            defer bytecode.deinit(allocator);

            const fixup = try beginBlockImpl(&state.writer, block_id);
            try state.writer.writer().writeAll(bytecode.items);
            try endBlock(&state.writer, &state.fixups, fixup);
        } else if (std.mem.eql(u8, keyword, "lsc-asm")) {
            const block_id_str = tokens.next() orelse return error.BadData;
            const block_id = parseBlockId(block_id_str) orelse return error.BadData;

            const lsc_number_str = tokens.next() orelse return error.BadData;
            const lsc_number = try std.fmt.parseInt(u16, lsc_number_str, 10);

            const relative_path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            const path = try pathf.append(&prst.cur_path, relative_path);
            defer path.restore();

            const asm_str = try fs.readFileZ(allocator, std.fs.cwd(), path.full());
            defer allocator.free(asm_str);

            var bytecode = try assemble.assemble(
                allocator,
                prst.languageStuff(),
                asm_str,
                &prst.symbols,
                .{ .local = .{ .room = room_number, .number = lsc_number } },
            );
            defer bytecode.deinit(allocator);

            const fixup = try beginBlockImpl(&state.writer, block_id);
            switch (block_id) {
                blockId("LSCR") => {
                    const num = std.math.cast(u8, lsc_number) orelse return error.BadData;
                    try state.writer.writer().writeInt(u8, num, .little);
                },
                blockId("LSC2") => {
                    try state.writer.writer().writeInt(u32, lsc_number, .little);
                },
                else => return error.BadData,
            }
            try state.writer.writer().writeAll(bytecode.items);
            try endBlock(&state.writer, &state.fixups, fixup);
        } else if (std.mem.eql(u8, keyword, "end-rmda")) {
            break;
        } else {
            return error.BadData;
        }
    }

    try endBlock(&state.writer, &state.fixups, rmda_fixup);
    const rmda_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("RMDA"),
        room_number,
        room_number,
        rmda_fixup,
        rmda_len,
    );
}

fn handleRoomImage(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: []const u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const compression_str = tokens.next() orelse return error.BadData;
    const compression = try std.fmt.parseInt(u8, compression_str, 10);

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Write block

    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();

    const bmp_raw = try fs.readFileZ(allocator, std.fs.cwd(), path.full());
    defer allocator.free(bmp_raw);

    const block_fixup = try beginBlock(&state.writer, "RMIM");

    try rmim_encode.encode(compression, bmp_raw, &state.writer, &state.fixups);

    try endBlock(&state.writer, &state.fixups, block_fixup);
    const block_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("RMIM"),
        room_number,
        room_number,
        block_fixup,
        block_len,
    );
}

fn handleScrpAsm(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: []const u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    // Parse line

    var words = std.mem.splitScalar(u8, line, ' ');

    const glob_number_str = words.next() orelse return error.BadData;
    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    const relative_path = words.next() orelse return error.BadData;

    if (words.next()) |_| return error.BadData;

    // Process block

    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();

    const asm_string = try fs.readFileZ(allocator, std.fs.cwd(), path.full());
    defer allocator.free(asm_string);

    const scrp_fixup = try beginBlock(&state.writer, "SCRP");

    var bytecode = try assemble.assemble(
        allocator,
        prst.languageStuff(),
        asm_string,
        &prst.symbols,
        .{ .global = glob_number },
    );
    defer bytecode.deinit(allocator);

    try state.writer.writer().writeAll(bytecode.items);

    try endBlock(&state.writer, &state.fixups, scrp_fixup);
    const scrp_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("SCRP"),
        room_number,
        glob_number,
        scrp_fixup,
        scrp_len,
    );
}

fn handleAudio(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: []const u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    // Parse line

    var words = std.mem.splitScalar(u8, line, ' ');

    const block_id_str = words.next() orelse return error.BadData;
    const block_id = parseBlockId(block_id_str) orelse return error.BadData;

    const glob_number_str = words.next() orelse return error.BadData;
    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    const relative_path = words.next() orelse return error.BadData;

    if (words.next()) |_| return error.BadData;

    // Process block

    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();

    const wav_file = try std.fs.cwd().openFileZ(path.full(), .{});
    defer wav_file.close();
    var wav_reader = std.io.bufferedReader(wav_file.reader());

    const digi_fixup = try beginBlockImpl(&state.writer, block_id);
    try audio.encode(wav_reader.reader(), &state.writer, &state.fixups);
    try endBlock(&state.writer, &state.fixups, digi_fixup);
    const digi_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        block_id,
        room_number,
        glob_number,
        digi_fixup,
        digi_len,
    );
}

fn handleWsou(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: []const u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    // Parse line

    var tokens = std.mem.tokenizeScalar(u8, line, ' ');

    const glob_number_str = tokens.next() orelse return error.BadData;
    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    const relative_path = tokens.next() orelse return error.BadData;

    if (tokens.next()) |_| return error.BadData;

    // Process block

    const block_start = try beginBlock(&state.writer, "WSOU");

    const path = try pathf.append(&prst.cur_path, relative_path);
    defer path.restore();
    try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.writer.writer());

    try endBlock(&state.writer, &state.fixups, block_start);
    const block_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("WSOU"),
        room_number,
        glob_number,
        block_start,
        block_len,
    );
}

fn handleAwiz(
    allocator: std.mem.Allocator,
    room_number: u8,
    glob_number_str: []const u8,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    var wiz = try readAwizLines(allocator, room_reader, room_line_buf, prst);
    defer wiz.deinit(allocator);

    const awiz_fixup = try beginBlock(&state.writer, "AWIZ");
    try awiz.encode(&wiz, &state.writer, &state.fixups);
    try endBlock(&state.writer, &state.fixups, awiz_fixup);
    const awiz_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("AWIZ"),
        room_number,
        glob_number,
        awiz_fixup,
        awiz_len,
    );
}

fn readAwizLines(
    allocator: std.mem.Allocator,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    prst: *ProjectState,
) !awiz.Awiz {
    var wiz = awiz.Awiz{};
    errdefer wiz.deinit(allocator);

    while (true) {
        const room_line =
            try room_reader.reader().readUntilDelimiter(room_line_buf, '\n');
        const room_line_trimmed = std.mem.trimLeft(u8, room_line, " ");
        var split = std.mem.splitScalar(u8, room_line_trimmed, ' ');
        const keyword = split.first();
        if (std.mem.eql(u8, keyword, "end-awiz"))
            break;
        const block_id = parseBlockId(keyword) orelse return error.BadData;
        switch (block_id) {
            blockId("RGBS") => {
                try wiz.blocks.append(.rgbs);
            },
            blockId("CNVS"), blockId("SPOT"), blockId("RELO") => {
                const str1 = split.next() orelse return error.BadData;
                const str2 = split.next() orelse return error.BadData;
                if (split.next()) |_| return error.BadData;

                const int1 = try std.fmt.parseInt(i32, str1, 10);
                const int2 = try std.fmt.parseInt(i32, str2, 10);

                try wiz.blocks.append(.{
                    .two_ints = .{
                        .id = block_id,
                        .ints = .{ int1, int2 },
                    },
                });
            },
            blockId("WIZH") => {
                if (split.next()) |_| return error.BadData;
                try wiz.blocks.append(.wizh);
            },
            blockId("WIZD") => {
                const relative_path = split.next() orelse return error.BadData;
                if (split.next()) |_| return error.BadData;

                const path = try pathf.append(&prst.cur_path, relative_path);
                defer path.restore();

                const bmp_raw = try fs.readFileZ(allocator, std.fs.cwd(), path.full());
                errdefer allocator.free(bmp_raw);

                try wiz.blocks.append(.{
                    .wizd = std.ArrayListUnmanaged(u8).fromOwnedSlice(bmp_raw),
                });
            },
            else => return error.BadData,
        }
    }

    return wiz;
}

fn handleMult(
    allocator: std.mem.Allocator,
    room_number: u8,
    line: []const u8,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const desc = try parseMult(
        arena.allocator(),
        line,
        room_reader,
        room_line_buf,
        prst,
    );

    const mult_fixup = try beginBlock(&state.writer, "MULT");

    for (desc.raws.items) |raw| {
        const raw_fixup = try beginBlockImpl(&state.writer, raw.id);

        const path = try pathf.append(&prst.cur_path, raw.path);
        defer path.restore();

        try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.writer.writer());

        try endBlock(&state.writer, &state.fixups, raw_fixup);
    }

    const wrap_fixup = try beginBlock(&state.writer, "WRAP");

    const offs_fixup = try beginBlock(&state.writer, "OFFS");
    // will be filled in using fixups
    for (desc.indices.items.len) |_|
        try state.writer.writer().writeInt(i32, undefined, .little);
    try endBlock(&state.writer, &state.fixups, offs_fixup);

    var wiz_offsets = std.ArrayListUnmanaged(u32){};
    defer wiz_offsets.deinit(allocator);
    try wiz_offsets.ensureTotalCapacityPrecise(allocator, desc.wizs.items.len);

    for (desc.wizs.items) |*wiz| {
        const wiz_offset: u32 = @intCast(state.writer.bytes_written - offs_fixup);
        try wiz_offsets.append(allocator, wiz_offset);

        const awiz_fixup = try beginBlock(&state.writer, "AWIZ");

        switch (wiz.*) {
            .wiz => |*img| try awiz.encode(img, &state.writer, &state.fixups),
            .raw => |*raw| {
                const path = try pathf.append(&prst.cur_path, raw.path);
                defer path.restore();

                try fs.readFileIntoZ(std.fs.cwd(), path.full(), state.writer.writer());
            },
        }

        try endBlock(&state.writer, &state.fixups, awiz_fixup);
    }

    // Go back and fill the OFFS with the offset corresponding to each index.
    for (desc.indices.items, 0..) |wiz_index, off_index_usize| {
        const off_index: u32 = @intCast(off_index_usize);
        if (wiz_index >= wiz_offsets.items.len)
            return error.BadData;
        const offset = wiz_offsets.items[wiz_index];
        try state.fixups.append(.{
            .offset = offs_fixup + 8 + off_index * 4,
            .bytes = Fixup.encode(offset, .little),
        });
    }

    try endBlock(&state.writer, &state.fixups, wrap_fixup);

    try endBlock(&state.writer, &state.fixups, mult_fixup);
    const mult_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("MULT"),
        room_number,
        desc.glob_number,
        mult_fixup,
        mult_len,
    );
}

// leaks. use an arena.
const Mult = struct {
    glob_number: u32,
    raws: std.ArrayListUnmanaged(MultRaw),
    wizs: std.ArrayListUnmanaged(union(enum) { wiz: awiz.Awiz, raw: MultRaw }),
    indices: std.ArrayListUnmanaged(u32),
};

const MultRaw = struct {
    id: BlockId,
    path: []const u8,
};

fn parseMult(
    arena: std.mem.Allocator,
    first_line: []const u8,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    prst: *ProjectState,
) !Mult {
    // Parse first line

    var words = std.mem.splitScalar(u8, first_line, ' ');

    const glob_number_str = words.next() orelse return error.BadData;
    const glob_number = try std.fmt.parseInt(u16, glob_number_str, 10);

    if (words.next()) |_| return error.BadData;

    // Parse remaining lines

    var result = Mult{
        .glob_number = glob_number,
        .raws = .{},
        .wizs = .{},
        .indices = .{},
    };

    while (true) {
        const room_line =
            try room_reader.reader().readUntilDelimiter(room_line_buf, '\n');
        var tokens = std.mem.tokenizeScalar(u8, room_line, ' ');
        const keyword = tokens.next() orelse return error.BadData;
        if (std.mem.eql(u8, keyword, "raw-block")) {
            const block_id_str = tokens.next() orelse return error.BadData;
            const block_id = parseBlockId(block_id_str) orelse return error.BadData;

            const path = tokens.next() orelse return error.BadData;

            if (tokens.next()) |_| return error.BadData;

            const path_alloc = try arena.dupe(u8, path);
            const raw = MultRaw{
                .id = block_id,
                .path = path_alloc,
            };
            if (block_id == comptime blockId("AWIZ"))
                try result.wizs.append(arena, .{ .raw = raw })
            else
                try result.raws.append(arena, raw);
        } else if (std.mem.eql(u8, keyword, "awiz")) {
            const wiz = try readAwizLines(arena, room_reader, room_line_buf, prst);
            try result.wizs.append(arena, .{ .wiz = wiz });
        } else if (std.mem.eql(u8, keyword, "indices")) {
            if (result.indices.items.len != 0)
                return error.BadData;
            try result.indices.ensureTotalCapacityPrecise(arena, result.wizs.items.len);
            while (tokens.next()) |num_str| {
                const num = try std.fmt.parseInt(u32, num_str, 10);
                try result.indices.append(arena, num);
            }
        } else if (std.mem.eql(u8, keyword, "end-mult"))
            break
        else
            return error.BadData;
    }

    return result;
}

fn handleAkos(
    allocator: std.mem.Allocator,
    room_number: u8,
    glob_number_str: []const u8,
    room_reader: anytype,
    room_line_buf: *[1024]u8,
    prst: *ProjectState,
    state: *DiskState,
) !void {
    const glob_number = try std.fmt.parseInt(u32, glob_number_str, 10);

    const akos_fixup = try beginBlock(&state.writer, "AKOS");
    try akos.encode(
        allocator,
        room_reader,
        room_line_buf,
        &prst.cur_path,
        &state.writer,
        &state.fixups,
    );
    try endBlock(&state.writer, &state.fixups, akos_fixup);
    const akos_len = state.lastBlockLen();

    try addGlobToDirectory(
        allocator,
        prst,
        comptime blockId("AKOS"),
        room_number,
        glob_number,
        akos_fixup,
        akos_len,
    );
}

fn addGlobToDirectory(
    allocator: std.mem.Allocator,
    prst: *ProjectState,
    block_id: BlockId,
    room_number: u8,
    glob_number: u32,
    block_start: u32,
    block_len: u32,
) !void {
    const directory = directoryForBlockId(&prst.index.directories, block_id) orelse
        return error.BadData;
    try utils.growMultiArrayList(DirectoryEntry, directory, allocator, glob_number + 1, .{
        .room = 0,
        .offset = 0,
        .len = games.directoryNonPresentLen(prst.game),
    });
    const offset = block_start - prst.index.lfl_offsets.items[room_number];
    const len = if (block_id == (comptime blockId("MULT")) and !games.writeMultLen(prst.game))
        0xffff_ffff
    else
        block_len;
    directory.set(glob_number, .{
        .room = room_number,
        .offset = @intCast(offset),
        .len = len,
    });
}

fn writeIndex(
    allocator: std.mem.Allocator,
    prst: *ProjectState,
    output_path: [:0]u8,
    attribution: bool,
) !void {
    games.pointPathToIndex(prst.game, output_path);

    const file = try std.fs.cwd().createFileZ(output_path, .{});
    errdefer file.close();

    const xor_writer = io.xorWriter(file.writer(), xor_key);
    var buf_writer = std.io.bufferedWriter(xor_writer.writer());
    var writer = std.io.countingWriter(buf_writer.writer());

    var fixups = std.ArrayList(Fixup).init(allocator);
    defer fixups.deinit();

    const maxs_fixup = try beginBlock(&writer, "MAXS");
    try writer.writer().writeAll(prst.index.maxs);
    try endBlock(&writer, &fixups, maxs_fixup);

    // SCUMM outputs sequential room numbers for these whether or not the room
    // actually exists.
    for (0.., prst.index.directories.room_images.items(.room)) |i, *room|
        room.* = @intCast(i);
    for (0.., prst.index.directories.rooms.items(.room)) |i, *room|
        room.* = @intCast(i);

    try writeDirectory(&writer, "DIRI", &prst.index.directories.room_images, &fixups);
    try writeDirectory(&writer, "DIRR", &prst.index.directories.rooms, &fixups);
    try writeDirectory(&writer, "DIRS", &prst.index.directories.scripts, &fixups);
    try writeDirectory(&writer, "DIRN", &prst.index.directories.sounds, &fixups);
    try writeDirectory(&writer, "DIRC", &prst.index.directories.costumes, &fixups);
    try writeDirectory(&writer, "DIRF", &prst.index.directories.charsets, &fixups);
    try writeDirectory(&writer, "DIRM", &prst.index.directories.images, &fixups);
    if (games.hasTalkies(prst.game))
        try writeDirectory(&writer, "DIRT", &prst.index.directories.talkies, &fixups);

    const dlfl_fixup = try beginBlock(&writer, "DLFL");
    try writer.writer().writeInt(u16, @intCast(prst.index.lfl_offsets.items.len), .little);
    try writer.writer().writeAll(std.mem.sliceAsBytes(prst.index.lfl_offsets.items));
    try endBlock(&writer, &fixups, dlfl_fixup);

    if (prst.index.lfl_disks) |*lfl_disks| {
        const disk_fixup = try beginBlock(&writer, "DISK");
        try writer.writer().writeInt(u16, @intCast(lfl_disks.items.len), .little);
        try writer.writer().writeAll(lfl_disks.items);
        try endBlock(&writer, &fixups, disk_fixup);
    }

    if (prst.index.sver) |sver| {
        const start = try beginBlock(&writer, "SVER");
        try writer.writer().writeAll(sver);
        try endBlock(&writer, &fixups, start);
    }

    const rnam_fixup = try beginBlock(&writer, "RNAM");
    for (0.., prst.index.room_names.items) |num, name| {
        if (name.len == 0)
            continue;
        try writer.writer().writeInt(u16, @intCast(num), .little);
        // TODO: could you writeAll with a null-terminated name to save a write
        // call here?
        try writer.writer().writeAll(name);
        try writer.writer().writeByte(0);
    }
    try writer.writer().writeInt(u16, 0, .little); // terminator
    try endBlock(&writer, &fixups, rnam_fixup);

    const dobj_fixup = try beginBlock(&writer, "DOBJ");
    try writer.writer().writeAll(prst.index.dobj);
    try endBlock(&writer, &fixups, dobj_fixup);

    const aary_fixup = try beginBlock(&writer, "AARY");
    try writer.writer().writeAll(prst.index.aary);
    try endBlock(&writer, &fixups, aary_fixup);

    if (games.hasIndexInib(prst.game)) {
        const inib_fixup = try beginBlock(&writer, "INIB");
        const note_fixup = try beginBlock(&writer, "NOTE");
        const note = if (attribution)
            "Modded with Baller <https://github.com/whatisaphone/baller>"
        else
            "";
        try writeLengthPrefixedString(writer.writer(), note);
        try endBlock(&writer, &fixups, note_fixup);
        try endBlock(&writer, &fixups, inib_fixup);
    }

    try buf_writer.flush();

    try writeFixups(file, xor_writer.writer(), fixups.items);
}

fn writeLengthPrefixedString(writer: anytype, str: []const u8) !void {
    const len = std.math.cast(u16, str.len) orelse return error.Overflow;
    try writer.writeInt(u16, len, .little);
    try writer.writeAll(str);
}

fn writeDirectory(
    stream: anytype,
    comptime block_id: []const u8,
    directory: *const std.MultiArrayList(DirectoryEntry),
    fixups: *std.ArrayList(Fixup),
) !void {
    const id = comptime blockId(block_id);
    return writeDirectoryImpl(stream, id, directory, fixups);
}

fn writeDirectoryImpl(
    stream: anytype,
    block_id: BlockId,
    directory: *const std.MultiArrayList(DirectoryEntry),
    fixups: *std.ArrayList(Fixup),
) !void {
    const block_fixup = try beginBlockImpl(stream, block_id);

    const slice = directory.slice();
    try stream.writer().writeInt(u16, @intCast(slice.len), .little);
    try stream.writer().writeAll(slice.items(.room));
    try stream.writer().writeAll(std.mem.sliceAsBytes(slice.items(.offset)));
    try stream.writer().writeAll(std.mem.sliceAsBytes(slice.items(.len)));

    try endBlock(stream, fixups, block_fixup);
}

const ProjectState = struct {
    game: games.Game,
    cur_path: pathf.Path,
    index: Index,
    language: lang.Language,
    ins_map: std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    symbols_text: []const u8,
    symbols: Symbols,

    fn init(allocator: std.mem.Allocator, game: games.Game) !ProjectState {
        var result: ProjectState = undefined;
        result.game = game;
        result.cur_path = .{};
        result.index = .{};
        result.language = lang.buildLanguage(game);
        result.ins_map = try lang.buildInsMap(allocator, &result.language);
        errdefer result.ins_map.deinit(allocator);
        result.symbols_text = "";
        result.symbols = .{ .game = game };
        return result;
    }

    fn deinit(self: *ProjectState, allocator: std.mem.Allocator) void {
        self.symbols.deinit(allocator);
        allocator.free(self.symbols_text);
        self.ins_map.deinit(allocator);
        self.index.deinit(allocator);
    }

    fn languageStuff(self: *const ProjectState) struct {
        *const lang.Language,
        *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    } {
        return .{ &self.language, &self.ins_map };
    }
};

const DiskState = struct {
    disk_number: u8,
    file: std.fs.File,
    xor_writer: io.XorWriter(std.fs.File.Writer),
    buf_writer: std.io.BufferedWriter(4096, io.XorWriter(std.fs.File.Writer).Writer),
    writer: std.io.CountingWriter(std.io.BufferedWriter(4096, io.XorWriter(std.fs.File.Writer).Writer).Writer),
    fixups: std.ArrayList(Fixup),

    fn deinit(self: *const DiskState) void {
        self.fixups.deinit();
        self.file.close();
    }

    fn lastBlockLen(self: *const DiskState) u32 {
        return std.mem.readInt(u32, &self.fixups.getLast().bytes, .big);
    }
};

const Index = struct {
    maxs: []u8 = &.{},
    directories: Directories = .{},
    lfl_offsets: std.ArrayListUnmanaged(u32) = .{},
    lfl_disks: ?std.ArrayListUnmanaged(u8) = null,
    room_names: std.ArrayListUnmanaged([]u8) = .{},
    dobj: []u8 = &.{},
    aary: []u8 = &.{},
    sver: ?[]u8 = null,

    fn deinit(self: *Index, allocator: std.mem.Allocator) void {
        if (self.sver) |sver|
            allocator.free(sver);
        allocator.free(self.aary);
        allocator.free(self.dobj);

        var i = self.room_names.items.len;
        while (i > 0) {
            i -= 1;
            const room_name = self.room_names.items[i];
            allocator.free(room_name);
        }
        self.room_names.deinit(allocator);

        if (self.lfl_disks) |*lfl_disks|
            lfl_disks.deinit(allocator);
        self.lfl_offsets.deinit(allocator);
        self.directories.deinit(allocator);
        allocator.free(self.maxs);
    }
};

const Directories = struct {
    room_images: std.MultiArrayList(DirectoryEntry) = .{},
    rooms: std.MultiArrayList(DirectoryEntry) = .{},
    scripts: std.MultiArrayList(DirectoryEntry) = .{},
    sounds: std.MultiArrayList(DirectoryEntry) = .{},
    costumes: std.MultiArrayList(DirectoryEntry) = .{},
    charsets: std.MultiArrayList(DirectoryEntry) = .{},
    images: std.MultiArrayList(DirectoryEntry) = .{},
    talkies: std.MultiArrayList(DirectoryEntry) = .{},

    fn deinit(self: *Directories, allocator: std.mem.Allocator) void {
        self.talkies.deinit(allocator);
        self.images.deinit(allocator);
        self.charsets.deinit(allocator);
        self.costumes.deinit(allocator);
        self.sounds.deinit(allocator);
        self.scripts.deinit(allocator);
        self.rooms.deinit(allocator);
        self.room_images.deinit(allocator);
    }
};

const DirectoryEntry = struct {
    room: u8,
    offset: u32,
    len: u32,
};

// TODO: this is duplicated
fn directoryForBlockId(
    directories: *Directories,
    block_id: BlockId,
) ?*std.MultiArrayList(DirectoryEntry) {
    return switch (block_id) {
        blockId("RMIM") => &directories.room_images,
        blockId("RMDA") => &directories.rooms,
        blockId("SCRP") => &directories.scripts,
        blockId("DIGI"),
        blockId("SOUN"),
        blockId("TALK"),
        blockId("WSOU"),
        => &directories.sounds,
        blockId("AKOS") => &directories.costumes,
        blockId("CHAR") => &directories.charsets,
        blockId("AWIZ"), blockId("MULT") => &directories.images,
        blockId("TLKE") => &directories.talkies,
        else => null,
    };
}
