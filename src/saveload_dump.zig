const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Block = @import("block_reader.zig").Block;
const FixedBlockReader = @import("block_reader.zig").FixedBlockReader;
const cliargs = @import("cliargs.zig");
const Maxs = @import("extract.zig").Maxs;
const xor_key = @import("extract.zig").xor_key;
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const utils = @import("utils.zig");

const debug = false;

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var index_path_opt: ?[:0]const u8 = null;
    var savegame_path_opt: ?[:0]const u8 = null;

    var it: cliargs.Iterator = .init(args);
    while (it.next()) |arg| switch (arg) {
        .positional => |str| {
            if (index_path_opt == null)
                index_path_opt = str
            else if (savegame_path_opt == null)
                savegame_path_opt = str
            else
                return arg.reportUnexpected();
        },
        else => return arg.reportUnexpected(),
    };

    const index_path = index_path_opt orelse return cliargs.reportMissing("index");
    const savegame_path = savegame_path_opt orelse return cliargs.reportMissing("savegame");

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);

    run(.{
        .gpa = gpa,
        .diagnostic = &diagnostic,
        .index_path = index_path,
        .savegame_path = savegame_path,
        .out = &out.interface,
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
    };
    try out.interface.flush();
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

const Args = struct {
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    index_path: [:0]const u8,
    savegame_path: [:0]const u8,
    out: *std.io.Writer,
};

pub fn run(args: Args) !void {
    const game = try games.detectGameOrFatal(args.diagnostic, args.index_path);
    const maxs = try readIndex(args.diagnostic, game, args.index_path);

    var arena: std.heap.ArenaAllocator = .init(args.gpa);
    defer arena.deinit();

    const buf = try fs.readFileZ(arena.allocator(), std.fs.cwd(), args.savegame_path);
    var in: std.io.Reader = .fixed(buf);

    const diag: Diagnostic.ForBinaryFile = .init(args.diagnostic, args.savegame_path);

    try dumpSaveGame(arena.allocator(), game, &maxs, &in, &diag, args.out);
}

// This is basically just enough of `extract.readIndex` to read the MAXS block.
fn readIndex(diagnostic: *Diagnostic, game: games.Game, index_path: [:0]const u8) !Maxs {
    const diag: Diagnostic.ForBinaryFile = .init(diagnostic, index_path);

    const file = try std.fs.cwd().openFileZ(index_path, .{});
    defer file.close();

    var raw: [8 + @sizeOf(Maxs)]u8 = undefined;
    var reader = file.reader(&raw);
    try reader.interface.fill(raw.len);
    for (&raw) |*b|
        b.* ^= xor_key;

    var in: std.io.Reader = .fixed(&raw);
    var blocks: FixedBlockReader = .init(&in, &diag);

    const maxs_raw = try blocks.expect(.MAXS).bytes();
    if (maxs_raw.len != games.maxsLen(game))
        return error.BadData;

    var maxs: Maxs = undefined;
    const maxs_present_bytes = std.mem.asBytes(&maxs)[0..maxs_raw.len];
    const maxs_missing_bytes = std.mem.asBytes(&maxs)[maxs_raw.len..@sizeOf(Maxs)];
    @memcpy(maxs_present_bytes, maxs_raw);
    @memset(maxs_missing_bytes, 0);

    blocks.abandon();

    return maxs;
}

fn dumpSaveGame(
    arena: std.mem.Allocator,
    game: games.Game,
    maxs: *const Maxs,
    in: *std.io.Reader,
    diag: *const Diagnostic.ForBinaryFile,
    out: *std.io.Writer,
) !void {
    var save: Save = undefined;

    var root: FixedBlockReader = .init(in, diag);
    var save_blocks = try root.expect(.SAVE).nested();

    _ = try save_blocks.expect(.VARS).block();

    try readValue(in, &save.room);
    try debugValue(out, "room", &save.room);

    save.objects = try readArray(arena, in, Object, maxs.objects_in_room);
    try debugSlice(out, "objects", save.objects);

    try readValue(in, &save.scripts);
    try debugSlice(out, "scripts", &save.scripts);

    try readValue(in, &save.polygons);
    try debugSlice(out, "polygons", &save.polygons);

    try readValue(in, &save.default_actor_clipping);
    try debugSlice(out, "default_actor_clipping", &save.default_actor_clipping);

    if (game.target().le(.sputm90))
        _ = try io.readInPlaceAsValue(in, [62]ActorSputm90)
    else if (game.target().ge(.sputm99))
        _ = try io.readInPlaceAsValue(in, [62]ActorSputm99)
    else
        return error.GameNotSupported;

    save.array_local_script_number = try readArray(arena, in, i32, maxs.arrays);
    try debugSlice(out, "array_local_script_number", save.array_local_script_number);

    save.local_scripts_offset = try readArray(arena, in, u32, maxs.local_scripts);
    try debugSlice(out, "local_scripts_offset", save.local_scripts_offset);

    save.local_scripts_data_offset = try readArray(arena, in, u32, maxs.local_scripts);
    try debugSlice(out, "local_scripts_data_offset", save.local_scripts_data_offset);

    save.object_states = try readArray(arena, in, u8, maxs.objects);
    try debugSlice(out, "object_states", save.object_states);

    save.object_owners = try readArray(arena, in, u8, maxs.objects);
    try debugSlice(out, "object_owners", save.object_owners);

    save.object_rooms = try readArray(arena, in, u8, maxs.objects);
    try debugSlice(out, "object_rooms", save.object_rooms);

    save.object_classes = try readArray(arena, in, i32, maxs.objects);
    try debugSlice(out, "object_classes", save.object_classes);

    save.global_vars = try readArray(arena, in, i32, maxs.variables);
    try debugSlice(out, "global_vars", save.global_vars);

    save.room_vars = try readArray(arena, in, i32, maxs.room_variables);
    try debugSlice(out, "room_vars", save.room_vars);

    try readValue(in, &save.local_vars);
    try debugSlice(out, "local_vars", &save.local_vars);

    try readValue(in, &save.new_object_names);
    try debugSlice(out, "new_object_names", &save.new_object_names);

    try readValue(in, &save.room_pseudo_table);
    try debugSlice(out, "room_pseudo_table", &save.room_pseudo_table);

    const stack_size: usize = if (game.target().le(.sputm90))
        100
    else if (game.target().ge(.sputm99))
        256
    else
        return error.GameNotSupported;
    try in.readSliceAll(std.mem.sliceAsBytes(save.stack[0..stack_size]));
    try debugSlice(out, "stack", save.stack[0..stack_size]);

    try readValue(in, &save.stack_ptr);
    try debugValue(out, "stack_ptr", &save.stack_ptr);

    try readValue(in, &save.text_colors);
    try debugSlice(out, "text_colors", &save.text_colors);

    save.charset_colors = try readArray(arena, in, [16]i32, maxs.charsets);
    try debugSlice(out, "charset_colors", save.charset_colors);

    try readValue(in, &save.actor_talkies);
    try debugSlice(out, "actor_talkies", &save.actor_talkies);

    try readValue(in, &save.next_script);
    try debugValue(out, "next_script", &save.next_script);

    try readValue(in, &save.cur_script_slot);
    try debugValue(out, "cur_script_slot", &save.cur_script_slot);

    try readValue(in, &save.skipped_blob_1);
    try debugValue(out, "skipped_blob_1", &save.skipped_blob_1);

    try readValue(in, &save.msgs_buffer);
    try debugValue(out, "msgs_buffer", &save.msgs_buffer);

    try readValue(in, &save.msgs_ptr);
    try debugValue(out, "msgs_ptr", &save.msgs_ptr);

    try readValue(in, &save.sentence_queue_ptr);
    try debugValue(out, "sentence_queue_ptr", &save.sentence_queue_ptr);

    try readValue(in, &save.sentence_queue);
    try debugSlice(out, "sentence_queue", &save.sentence_queue);

    try readValue(in, &save.cutscene_stack_ptr);
    try debugValue(out, "cutscene_stack_ptr", &save.cutscene_stack_ptr);

    try readValue(in, &save.cutscene);
    try debugSlice(out, "cutscene", &save.cutscene);

    try readValue(in, &save.recursive_stacks);
    try debugSlice(out, "recursive_stacks", &save.recursive_stacks);

    try readValue(in, &save.recursive_stack_ptr);
    try debugValue(out, "recursive_stack_ptr", &save.recursive_stack_ptr);

    // I'll be honest, I have no idea if these are right or not.
    const trailing: usize = switch (game) {
        .baseball_1997 => 26043,
        .football_1999 => 190315,
        .baseball_2001 => 214572,
        .basketball => 305376,
        else => return error.GameNotSupported,
    };
    _ = try io.readInPlace(in, trailing);

    try out.print("room: {}\n", .{save.room.number});
    try out.print("cur_script_slot: {}\n", .{save.cur_script_slot});
    try out.print("next_script: {}\n", .{save.next_script});
    try out.print("stack_ptr: {}\n", .{save.stack_ptr});
    for (0..save.scripts.len) |i|
        try dumpScript(game, maxs, &save, i, out);

    try out.writeAll("\nglobal vars:\n");
    for (save.global_vars, 0..) |value, i|
        if (value != 0)
            try dumpVar("global", i, value, out);

    try out.writeAll("\nroom vars:\n");
    for (save.room_vars, 0..) |value, i|
        if (value != 0)
            try dumpVar("room", i, value, out);

    try out.writeByte('\n');

    while (!save_blocks.atEnd()) {
        const block = try save_blocks.next().block();
        const raw = try io.readInPlace(in, block.size);
        switch (block.id) {
            .DBGL => {}, // not useful
            .HBGL => try dumpHbgl(raw, out),
            else => return error.BadData,
        }
    }

    try save_blocks.finish();
    try root.finish();
}

fn readValue(in: *std.io.Reader, ptr: anytype) !void {
    const value = try io.readInPlaceAsValue(in, @TypeOf(ptr.*));
    ptr.* = value.*;
}

fn readArray(gpa: std.mem.Allocator, in: *std.io.Reader, T: type, count: usize) ![]T {
    const result = try gpa.alloc(T, count);
    errdefer gpa.free(result);
    try in.readSliceAll(std.mem.sliceAsBytes(result));
    return result;
}

fn dumpScript(
    game: games.Game,
    maxs: *const Maxs,
    save: *const Save,
    slot: usize,
    out: *std.io.Writer,
) !void {
    const script = &save.scripts[slot];
    if (script.state == .dead) return;

    try out.print("\nscript slot {}: [{c}] ", .{ slot, script.state.char() });
    const type_str = switch (script.type) {
        .inventory => "inv",
        .object => "obj",
        .script => "scr",
        .local_script => "lsc",
        .flobject => "flo",
        _ => "err",
    };
    try out.print("{s}{}, pc=", .{ type_str, script.script });

    const real_pc = pc: switch (script.type) {
        .script => script.pc - Block.header_size,
        .local_script => {
            const first_lsc = games.firstLocalScript(game);
            if (script.script < first_lsc) break :pc null;
            const number: u32 = @intCast(script.script - first_lsc);
            if (number >= maxs.local_scripts) break :pc null;
            break :pc script.pc -
                save.local_scripts_offset[number] -
                save.local_scripts_data_offset[number];
        },
        else => null,
    };
    if (real_pc) |pc|
        try out.print("0x{x}", .{pc})
    else
        try out.print("(raw)0x{x}", .{script.pc});
    try out.writeByte('\n');

    const locals = &save.local_vars[slot];
    for (locals, 0..) |value, i|
        if (value != 0)
            try dumpVar("local", i, value, out);
}

fn dumpVar(prefix: []const u8, number: usize, value: i32, out: *std.io.Writer) !void {
    try out.print("    {s}{} = ", .{ prefix, number });
    try dumpValue(value, out);
    try out.writeByte('\n');
}

fn dumpValue(value: i32, out: *std.io.Writer) !void {
    try out.print("{}", .{value});
    if (getArrayNumber(value)) |array_number|
        try out.print("<array{}>", .{array_number});
}

fn getArrayNumber(value: i32) ?u12 {
    if (@as(u32, @bitCast(value)) & 0xfffff000 != 0x33539000) return null;
    return @intCast(value & 0xfff);
}

fn dumpHbgl(raw: []const u8, out: *std.io.Writer) !void {
    if (raw.len < @sizeOf(HbglHeader)) return error.BadData;
    const header = std.mem.bytesAsValue(HbglHeader, raw[0..@sizeOf(HbglHeader)]);
    const payload = raw[@sizeOf(HbglHeader)..];
    if (header.size != payload.len) return error.BadData;
    try out.print("glob {f} {} = ", .{ header.type, header.number });
    switch (header.type) {
        .array => try dumpArray(payload, out),
        .image => try out.writeAll("<image>"),
        else => try out.print("{x}", .{payload}),
    }
    try out.writeByte('\n');
}

fn dumpArray(raw: []const u8, out: *std.io.Writer) !void {
    if (raw.len < @sizeOf(ArrayHeader)) return error.BadData;
    // verify we can safely cast
    _ = std.meta.intToEnum(ArrayHeader.Type, std.mem.readInt(u32, raw[0..4], .little)) catch
        return error.BadData;
    const header = std.mem.bytesAsValue(ArrayHeader, raw[0..@sizeOf(ArrayHeader)]);
    const payload = raw[@sizeOf(ArrayHeader)..];

    if (header.down_min > header.down_max) return error.BadData;
    const down_len: u32 = @intCast(header.down_max - header.down_min + 1);
    if (header.across_min > header.across_max) return error.BadData;
    const across_len: u32 = @intCast(header.across_max - header.across_min + 1);
    const expected_size = down_len * across_len * header.type.bytes();
    if (payload.len < expected_size) return error.BadData;
    const excess = payload.len - expected_size;
    if (excess > 3) return error.BadData;

    try out.print("array {s} [{} to {}][{} to {}]\n", .{
        header.type.str(),
        header.down_min,
        header.down_max,
        header.across_min,
        header.across_max,
    });

    var cur: utils.SafeManyPointer([*]const u8) = .init(payload);
    for (0..down_len) |y_from_0| {
        try out.print("    [{}] =", .{utils.add(i32, header.down_min, y_from_0).?});
        switch (header.type) {
            .string => {
                const str = cur.use()[0..across_len];
                try out.print(" \"{f}\"", .{std.ascii.hexEscape(str, .lower)});
            },
            else => {
                for (0..across_len) |_| {
                    const value = switch (header.type) {
                        .int8 => cur.get(0),
                        .string => unreachable,
                        .int16 => std.mem.readInt(i16, cur.use()[0..2], .little),
                        .int32 => std.mem.readInt(i32, cur.use()[0..4], .little),
                    };
                    try out.writeByte(' ');
                    try dumpValue(value, out);
                    cur = cur.plus(header.type.bytes());
                }
            },
        }
        try out.writeByte('\n');
    }
}

const HbglHeader = extern struct {
    size: u32 align(1),
    type: Type align(1),
    number: u16 align(1),
    modified: BoolU8 align(1),

    const Type = enum(u16) {
        array = 7,
        image = 19,
        _,

        pub fn format(self: Type, w: *std.io.Writer) !void {
            return fmtTagNameNonExhaustive(self).format(w);
        }
    };
};

const ArrayHeader = extern struct {
    type: Type,
    across_min: i32,
    across_max: i32,
    down_min: i32,
    down_max: i32,
    unk_14: i32,
    unk_18: i32,

    const Type = enum(u32) {
        int8 = 3,
        string = 4,
        int16 = 5,
        int32 = 6,

        fn bytes(self: Type) u8 {
            return switch (self) {
                .int8, .string => 1,
                .int16 => 2,
                .int32 => 4,
            };
        }

        fn str(self: Type) []const u8 {
            return switch (self) {
                .int8 => "int8",
                .string => "string",
                .int16 => "int16",
                .int32 => "int32",
            };
        }
    };
};

/// avoid UB if we read a byte other than 0 or 1
const BoolU8 = enum(u8) { false, true, _ };

const Save = struct {
    room: Room,
    objects: []Object,
    scripts: [80]Script,
    polygons: [200]Polygon,
    default_actor_clipping: [4]i32,
    array_local_script_number: []i32,
    local_scripts_offset: []u32,
    local_scripts_data_offset: []u32,
    object_states: []u8,
    object_owners: []u8,
    object_rooms: []u8,
    object_classes: []i32,
    global_vars: []i32,
    room_vars: []i32,
    local_vars: [80][25]i32,
    new_object_names: [10]u32,
    room_pseudo_table: [127]i32,
    stack: [256]i32,
    stack_ptr: i32,
    text_colors: [16]i32,
    charset_colors: [][16]i32,
    actor_talkies: [16]ActorTalkie,
    next_script: i32,
    cur_script_slot: i32,
    skipped_blob_1: [54]u8,
    msgs_buffer: [256]u8,
    msgs_ptr: i32,
    sentence_queue_ptr: u8,
    sentence_queue: [6]Sentence,
    cutscene_stack_ptr: i32,
    cutscene: [5]Cutscene,
    recursive_stacks: [15]RecursiveStack,
    recursive_stack_ptr: i32,
};

const Room = extern struct {
    number: i32,
    skipped_blob: [40]u8,
};

const Object = [56]u8;

const Script = extern struct {
    pc: u32,
    sleep: i32,
    script: i32,
    break_count: i32,
    state: State,
    type: Type,
    bak: i32,
    rec: i32,
    freeze: i32,
    once: i32,
    cutscene: i32,
    priority: i32,
    flags: i32,

    const State = enum(i32) {
        dead,
        sleeping,
        running,
        halted = 128,
        _,

        fn char(self: State) u8 {
            return switch (self) {
                .dead => 'D',
                .sleeping => 'S',
                .running => 'R',
                .halted => 'H',
                else => '?',
            };
        }
    };

    const Type = enum(i32) {
        inventory,
        object,
        script,
        local_script,
        flobject,
        _,
    };
};

const Polygon = [68]u8;
const ActorSputm90 = [1923]u8;
const ActorSputm99 = [1951]u8;
const ActorTalkie = [144]u8;
const Sentence = [11]u8;
const Cutscene = [12]u8;
const RecursiveStack = [12]u8;

fn fmtTagNameNonExhaustive(value: anytype) FmtTagNameNonExhaustive(@TypeOf(value)) {
    return .{ .value = value };
}

fn FmtTagNameNonExhaustive(E: type) type {
    return struct {
        value: E,

        pub fn format(self: @This(), w: *std.io.Writer) !void {
            if (std.enums.tagName(E, self.value)) |name|
                return w.writeAll(name);
            return w.print("0x{x}", .{@intFromEnum(self.value)});
        }
    };
}

fn debugValue(out: *std.io.Writer, name: []const u8, ptr: anytype) !void {
    if (!debug) return;
    try out.print("{s}: {x}\n", .{ name, std.mem.asBytes(ptr) });
}

fn debugSlice(out: *std.io.Writer, name: []const u8, slice: anytype) !void {
    if (!debug) return;
    for (slice, 0..) |*ptr, i|
        try out.print("{s}[{}]: {x}\n", .{ name, i, std.mem.asBytes(ptr) });
}
