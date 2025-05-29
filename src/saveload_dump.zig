const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const Block = @import("block_reader.zig").Block;
const fixedBlockReader = @import("block_reader.zig").fixedBlockReader;
const cliargs = @import("cliargs.zig");
const io = @import("io.zig");

pub fn runCli(gpa: std.mem.Allocator, args: []const [:0]const u8) !void {
    var it: cliargs.Iterator = .init(args);
    if (it.next()) |arg|
        return arg.reportUnexpected();

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();
    const diag: Diagnostic.ForBinaryFile = .init(&diagnostic, "-");

    var in_buf: std.ArrayListUnmanaged(u8) = try .initCapacity(gpa, 1 << 20);
    defer in_buf.deinit(gpa);
    try io.copy(std.io.getStdIn().reader(), in_buf.fixedWriter());
    var in = std.io.fixedBufferStream(@as([]const u8, in_buf.items));

    const cx: Cx = .{
        .in = &in,
        .diag = &diag,
        .out = std.io.getStdOut().writer(),
    };
    _ = run(&cx) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
    };
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

const Cx = struct {
    in: *std.io.FixedBufferStream([]const u8),
    diag: *const Diagnostic.ForBinaryFile,
    out: std.fs.File.Writer,
};

fn run(cx: *const Cx) !void {
    var result: Save = undefined;

    var root = fixedBlockReader(cx.in, cx.diag);
    var save_blocks = try root.expect(.SAVE).nested();

    _ = try save_blocks.expect(.VARS).block();
    try fill(cx, &result.room);
    try fill(cx, &result.objects);
    try fill(cx, &result.scripts);
    try fill(cx, &result.polygons);
    try fill(cx, &result.default_actor_clipping);
    try fill(cx, &result.actors);
    try fill(cx, &result.array_local_script_number);
    try fill(cx, &result.local_scripts_offset);
    try fill(cx, &result.local_scripts_data_offset);
    try fill(cx, &result.object_states);
    try fill(cx, &result.object_owners);
    try fill(cx, &result.object_rooms);
    try fill(cx, &result.object_classes);
    try fill(cx, &result.global_vars);
    try fill(cx, &result.room_vars);
    try fill(cx, &result.local_vars);
    try fill(cx, &result.new_object_names);
    try fill(cx, &result.room_pseudo_table);
    try fill(cx, &result.stack);
    try fill(cx, &result.stack_ptr);
    try fill(cx, &result.text_colors);
    try fill(cx, &result.charset_colors);
    try fill(cx, &result.actor_talkies);
    try fill(cx, &result.next_script);
    try fill(cx, &result.cur_script_slot);
    try fill(cx, &result.skipped_blob_1);
    try fill(cx, &result.msgs_buffer);
    try fill(cx, &result.msgs_ptr);
    try fill(cx, &result.sentence_queue_ptr);
    try fill(cx, &result.sentence_queue);
    try fill(cx, &result.cutscene_stack_ptr);
    try fill(cx, &result.cutscene);
    try fill(cx, &result.recursive_stacks);
    try fill(cx, &result.recursive_stack_ptr);
    try fill(cx, &result.skipped_blob_2);

    try cx.out.print("room: {}\n", .{result.room.number});
    try cx.out.print("cur_script_slot: {}\n", .{result.cur_script_slot});
    try cx.out.print("next_script: {}\n", .{result.next_script});
    try cx.out.print("stack_ptr: {}\n", .{result.stack_ptr});
    for (0..result.scripts.len) |i|
        try dumpScript(cx, &result, i);

    try cx.out.writeAll("\nglobal vars:\n");
    for (result.global_vars, 0..) |value, i|
        if (value != 0)
            try dumpVar(cx, "global", i, value);

    try cx.out.writeAll("\nroom vars:\n");
    for (result.room_vars, 0..) |value, i|
        if (value != 0)
            try dumpVar(cx, "room", i, value);

    while (!save_blocks.atEnd()) {
        const dbgl = try save_blocks.nextIf(.DBGL) orelse break;
        _ = try dbgl.bytes();
    }

    while (!save_blocks.atEnd()) {
        const hbgl = try save_blocks.nextIf(.HBGL) orelse break;
        _ = try hbgl.bytes();
    }

    try save_blocks.finish();
    try root.finish();
}

fn fill(cx: *const Cx, ptr: anytype) !void {
    const value = try io.readInPlaceAsValue(cx.in, @TypeOf(ptr.*));
    ptr.* = value.*;
}

fn dumpScript(cx: *const Cx, save: *const Save, slot: usize) !void {
    const script = &save.scripts[slot];
    if (script.state == .dead) return;

    try cx.out.print("\nscript slot {}: [{c}] ", .{ slot, script.state.char() });
    const type_str = switch (script.type) {
        .inventory => "inv",
        .object => "obj",
        .script => "scr",
        .local_script => "lsc",
        .flobject => "flo",
        _ => "err",
    };
    try cx.out.print("{s}{}, pc=", .{ type_str, script.script });

    const real_pc = pc: switch (script.type) {
        .script => script.pc - Block.header_size,
        .local_script => {
            if (script.script < first_lsc) break :pc null;
            const number: u32 = @intCast(script.script - first_lsc);
            if (number >= maxs_local_scripts) break :pc null;
            break :pc script.pc -
                save.local_scripts_offset[number] -
                save.local_scripts_data_offset[number];
        },
        else => null,
    };
    if (real_pc) |pc|
        try cx.out.print("0x{x}", .{pc})
    else
        try cx.out.print("(raw)0x{x}", .{script.pc});
    try cx.out.writeByte('\n');

    const locals = &save.local_vars[slot];
    for (locals, 0..) |value, i|
        if (value != 0)
            try dumpVar(cx, "local", i, value);
}

fn dumpVar(cx: *const Cx, prefix: []const u8, number: usize, value: i32) !void {
    try cx.out.print("    {s}{} = {}", .{ prefix, number, value });
    if (getArrayNumber(value)) |array_number|
        try cx.out.print(" <array {}>", .{array_number});
    try cx.out.writeByte('\n');
}

fn getArrayNumber(value: i32) ?u8 {
    if (@as(u32, @bitCast(value)) & 0xffffff00 != 0x33539000) return null;
    return @intCast(value & 0xff);
}

const first_lsc = 200;

const maxs_variables = 500;
const maxs_room_variables = 64;
const maxs_objects_in_room = 200;
const maxs_arrays = 100;
const maxs_charsets = 12;
const maxs_objects = 1250;
const maxs_local_scripts = 256;

const Save = struct {
    room: Room,
    objects: [maxs_objects_in_room]Object,
    scripts: [80]Script,
    polygons: [200]Polygon,
    default_actor_clipping: [4]i32,
    actors: [62]Actor,
    array_local_script_number: [maxs_arrays]i32,
    local_scripts_offset: [maxs_local_scripts]u32,
    local_scripts_data_offset: [maxs_local_scripts]u32,
    object_states: [maxs_objects]u8,
    object_owners: [maxs_objects]u8,
    object_rooms: [maxs_objects]u8,
    object_classes: [maxs_objects]i32,
    global_vars: [maxs_variables]i32,
    room_vars: [maxs_room_variables]i32,
    local_vars: [80][25]i32,
    new_object_names: [10]u32,
    room_pseudo_table: [127]i32,
    stack: [100]i32,
    stack_ptr: i32,
    text_colors: [16]i32,
    charset_colors: [maxs_charsets][16]i32,
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
    skipped_blob_2: [26043]u8,
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
const Actor = [1923]u8;
const ActorTalkie = [144]u8;
const Sentence = [11]u8;
const Cutscene = [12]u8;
const RecursiveStack = [12]u8;
