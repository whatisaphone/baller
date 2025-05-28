const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
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

    try dumpBytes(cx, "room", &result.room);
    try dumpArray(cx, "objects", &result.objects);
    try dumpArray(cx, "scripts", &result.scripts);
    try dumpArray(cx, "polygons", &result.polygons);
    try dumpArray(cx, "default_actor_clipping", &result.default_actor_clipping);
    try dumpArray(cx, "actors", &result.actors);
    try dumpArray(cx, "array_local_script_number", &result.array_local_script_number);
    try dumpArray(cx, "local_scripts_offset", &result.local_scripts_offset);
    try dumpArray(cx, "local_scripts_data_offset", &result.local_scripts_data_offset);
    try dumpArray(cx, "object_states", &result.object_states);
    try dumpArray(cx, "object_owners", &result.object_owners);
    try dumpArray(cx, "object_rooms", &result.object_rooms);
    try dumpArray(cx, "object_classes", &result.object_classes);
    try dumpArray(cx, "global_vars", &result.global_vars);
    try dumpArray(cx, "room_vars", &result.room_vars);
    try dumpArray(cx, "local_vars", &result.local_vars);
    try dumpArray(cx, "new_object_names", &result.new_object_names);
    try dumpArray(cx, "room_pseudo_table", &result.room_pseudo_table);
    try dumpArray(cx, "stack", &result.stack);
    try dumpBytes(cx, "stack_ptr", &result.stack_ptr);
    try dumpArray(cx, "text_colors", &result.text_colors);
    try dumpArray(cx, "charset_colors", &result.charset_colors);
    try dumpArray(cx, "actor_talkies", &result.actor_talkies);
    try dumpBytes(cx, "next_script", &result.next_script);
    try dumpBytes(cx, "cur_script_slot", &result.cur_script_slot);
    try dumpBytes(cx, "skipped_blob_1", &result.skipped_blob_1);
    try dumpBytes(cx, "msgs_buffer", &result.msgs_buffer);
    try dumpBytes(cx, "msgs_ptr", &result.msgs_ptr);
    try dumpBytes(cx, "sentence_queue_ptr", &result.sentence_queue_ptr);
    try dumpArray(cx, "sentence_queue", &result.sentence_queue);
    try dumpBytes(cx, "cutscene_stack_ptr", &result.cutscene_stack_ptr);
    try dumpArray(cx, "cutscene", &result.cutscene);
    try dumpArray(cx, "recursive_stacks", &result.recursive_stacks);
    try dumpBytes(cx, "recursive_stack_ptr", &result.recursive_stack_ptr);
    // try dumpBytes(cx, "skipped_blob_2", &result.skipped_blob_2);

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
    local_scripts_offset: [maxs_local_scripts]i32,
    local_scripts_data_offset: [maxs_local_scripts]i32,
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

const Room = [44]u8;
const Object = [56]u8;
const Script = [52]u8;
const Polygon = [68]u8;
const Actor = [1923]u8;
const ActorTalkie = [144]u8;
const Sentence = [11]u8;
const Cutscene = [12]u8;
const RecursiveStack = [12]u8;

fn dumpBytes(cx: *const Cx, name: []const u8, ptr: anytype) !void {
    try cx.out.print(
        "{s}: {}\n",
        .{ name, std.fmt.fmtSliceHexLower(std.mem.asBytes(ptr)) },
    );
}

fn dumpArray(cx: *const Cx, name: []const u8, slice: anytype) !void {
    for (slice, 0..) |*item, index| {
        try cx.out.print(
            "{s}[{}]: {}\n",
            .{ name, index, std.fmt.fmtSliceHexLower(std.mem.asBytes(item)) },
        );
    }
}
