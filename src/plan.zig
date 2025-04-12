const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const assemble = @import("assemble.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const block_header_size = @import("block_reader.zig").block_header_size;
const Fixup = @import("block_writer.zig").Fixup;
const applyFixups = @import("block_writer.zig").applyFixups;
const beginBlock = @import("block_writer.zig").beginBlock;
const endBlock = @import("block_writer.zig").endBlock;
const fs = @import("fs.zig");
const games = @import("games.zig");
const lang = @import("lang.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub const Event = struct {
    index: u16,
    payload: Payload,
};

pub const Payload = union(enum) {
    project_end,
    disk_start: u8,
    disk_end,
    room_start: u8,
    room_end,
    glob: struct { block_id: BlockId, glob_number: u16, data: std.ArrayListUnmanaged(u8) },
    glob_start: struct { block_id: BlockId, glob_number: u16 },
    glob_end,
    raw_block: struct { block_id: BlockId, data: std.ArrayListUnmanaged(u8) },
    index_start,
    index_end,
    index_block: @FieldType(Ast.Node, "index_block"),
    err,
};

pub fn run(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    game: games.Game,
    project_dir: std.fs.Dir,
    project: *const Project,
    awiz_strategy: awiz.EncodingStrategy,
    pool: *std.Thread.Pool,
    events: *sync.Channel(Event, 16),
) void {
    var cx: Context = .{
        .gpa = gpa,
        .diagnostic = diagnostic,
        .game = game,
        .project_dir = project_dir,
        .project = project,
        .awiz_strategy = awiz_strategy,
        .pool = pool,
        .events = events,
        .next_event_index = 0,
        .pending_jobs = .init(0),
    };

    planProject(&cx) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
        cx.sendSyncEvent(.err);
    };

    diagnostic.trace("waiting for jobs", .{});
    while (true) {
        const pending = cx.pending_jobs.load(.acquire);
        if (pending == 0) break;
        std.Thread.Futex.wait(&cx.pending_jobs, pending);
    }
    diagnostic.trace("all jobs finished", .{});
}

const Context = struct {
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    game: games.Game,
    project_dir: std.fs.Dir,
    project: *const Project,
    awiz_strategy: awiz.EncodingStrategy,
    pool: *std.Thread.Pool,
    events: *sync.Channel(Event, 16),
    next_event_index: u16,
    pending_jobs: std.atomic.Value(u32),

    fn sendSyncEvent(self: *Context, payload: Payload) void {
        self.events.send(.{ .index = self.next_event_index, .payload = payload });
        self.next_event_index += 1;
    }
};

fn planProject(cx: *Context) !void {
    const project_file = &cx.project.files.items[0].?;
    const project_node = &project_file.ast.nodes.items[project_file.ast.root].project;
    for (project_file.ast.getExtra(project_node.disks), 0..) |disk_node, disk_index| {
        if (disk_node == Ast.null_node) continue;
        const disk_number: u8 = @intCast(disk_index + 1);
        const disk = &project_file.ast.nodes.items[disk_node].disk;
        cx.sendSyncEvent(.{ .disk_start = disk_number });
        for (project_file.ast.getExtra(disk.children)) |room_node| {
            const room = &project_file.ast.nodes.items[room_node].disk_room;
            cx.sendSyncEvent(.{ .room_start = room.room_number });
            try planRoom(cx, room);
            cx.sendSyncEvent(.room_end);
        }
        cx.sendSyncEvent(.disk_end);
    }

    try planIndex(cx);

    cx.sendSyncEvent(.project_end);
}

fn planRoom(cx: *Context, room: *const @FieldType(Ast.Node, "disk_room")) !void {
    const room_file = &cx.project.files.items[room.room_number].?;
    const root = &room_file.ast.nodes.items[room_file.ast.root].room_file;
    for (room_file.ast.getExtra(root.children)) |child_node| {
        switch (room_file.ast.nodes.items[child_node]) {
            .raw_glob_file => |*n| try planRawGlobFile(cx, n),
            .raw_glob_block => |*n| try planRawGlobBlock(cx, room.room_number, n),
            .raw_block => |*n| try planRawBlock(cx, n),
            .scrp => |*n| try planScrp(cx, n),
            .awiz => |*n| try planAwiz(cx, room.room_number, n),
            .mult => |*n| try planMult(cx, room.room_number, n),
            else => unreachable,
        }
    }
}

fn planRawBlock(cx: *Context, node: *const @FieldType(Ast.Node, "raw_block")) !void {
    const data = try fs.readFile(cx.gpa, cx.project_dir, node.path);
    errdefer cx.gpa.free(data);

    cx.sendSyncEvent(.{ .raw_block = .{
        .block_id = node.block_id,
        .data = .fromOwnedSlice(data),
    } });
}

fn planRawGlobFile(cx: *Context, node: *const @FieldType(Ast.Node, "raw_glob_file")) !void {
    const data = try fs.readFile(cx.gpa, cx.project_dir, node.path);
    errdefer cx.gpa.free(data);

    cx.sendSyncEvent(.{ .glob = .{
        .block_id = node.block_id,
        .glob_number = node.glob_number,
        .data = .fromOwnedSlice(data),
    } });
}

fn planRawGlobBlock(
    cx: *Context,
    room_number: u8,
    glob: *const @FieldType(Ast.Node, "raw_glob_block"),
) !void {
    cx.sendSyncEvent(.{ .glob_start = .{
        .block_id = glob.block_id,
        .glob_number = glob.glob_number,
    } });

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(glob.children)) |node| {
        const raw_block = &room_file.ast.nodes.items[node].raw_block;
        try planRawBlock(cx, raw_block);
    }

    cx.sendSyncEvent(.glob_end);
}

fn planScrp(cx: *Context, node: *const @FieldType(Ast.Node, "scrp")) !void {
    const event_index = cx.next_event_index;
    cx.next_event_index += 1;

    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.pool.spawn(runScrp, .{ cx, event_index, node });
}

fn runScrp(
    cx: *Context,
    event_index: u16,
    node: *const @FieldType(Ast.Node, "scrp"),
) void {
    buildScrp(cx, event_index, node) catch |err| {
        if (err != error.AddedToDiagnostic)
            cx.diagnostic.zigErr("{s} {}: unexpected error: {s}", .{ "SCRP", node.glob_number }, err);
        cx.events.send(.{ .index = event_index, .payload = .err });
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn buildScrp(
    cx: *const Context,
    event_index: u16,
    node: *const @FieldType(Ast.Node, "scrp"),
) !void {
    const in = try fs.readFile(cx.gpa, cx.project_dir, node.path);
    defer cx.gpa.free(in);

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    const language = lang.buildLanguage(cx.game);
    const ins_map = try lang.buildInsMap(cx.gpa, &language);
    const symbols: Symbols = .{ .game = cx.game };
    const id: Symbols.ScriptId = .{ .global = node.glob_number };

    const result = try assemble.assemble(cx.gpa, .{ &language, &ins_map }, in, &symbols, id);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("SCRP"),
            .glob_number = node.glob_number,
            .data = result,
        } },
    });
}

fn planAwiz(
    cx: *Context,
    room_number: u8,
    node: *const @FieldType(Ast.Node, "awiz"),
) !void {
    const event_index = cx.next_event_index;
    cx.next_event_index += 1;

    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.pool.spawn(runAwiz, .{ cx, event_index, room_number, node });
}

fn runAwiz(
    cx: *Context,
    event_index: u16,
    room_number: u8,
    node: *const @FieldType(Ast.Node, "awiz"),
) void {
    buildAwiz(cx, event_index, room_number, node) catch |err| {
        if (err != error.AddedToDiagnostic)
            cx.diagnostic.zigErr("{s} {}: unexpected error: {s}", .{ "AWIZ", node.glob_number }, err);
        cx.events.send(.{ .index = event_index, .payload = .err });
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn buildAwiz(
    cx: *const Context,
    event_index: u16,
    room_number: u8,
    awiz_node: *const @FieldType(Ast.Node, "awiz"),
) !void {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    var stream = std.io.countingWriter(out.writer(cx.gpa));
    try buildAwizInner(cx, room_number, awiz_node.children, &stream, &fixups);
    applyFixups(out.items, fixups.items);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("AWIZ"),
            .glob_number = awiz_node.glob_number,
            .data = out,
        } },
    });
}

fn buildAwizInner(
    cx: *const Context,
    room_number: u8,
    children: Ast.ExtraSlice,
    out: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    var the_awiz: awiz.Awiz = .{};
    defer the_awiz.deinit(cx.gpa);

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(children)) |node| {
        const child_node = &room_file.ast.nodes.items[node];
        switch (child_node.*) {
            .awiz_rgbs => {
                the_awiz.blocks.append(.rgbs) catch unreachable;
            },
            .awiz_two_ints => |ti| {
                the_awiz.blocks.append(.{ .two_ints = .{
                    .id = ti.block_id,
                    .ints = ti.ints,
                } }) catch unreachable;
            },
            .awiz_wizh => {
                the_awiz.blocks.append(.wizh) catch unreachable;
            },
            .awiz_bmp => |wizd| {
                const awiz_raw = try fs.readFile(cx.gpa, cx.project_dir, wizd.path);
                const awiz_raw_arraylist: std.ArrayListUnmanaged(u8) = .fromOwnedSlice(awiz_raw);
                the_awiz.blocks.append(.{ .wizd = .{
                    .compression = wizd.compression,
                    .bmp = awiz_raw_arraylist,
                } }) catch unreachable;
            },
            else => unreachable,
        }
    }

    try awiz.encode(&the_awiz, cx.awiz_strategy, out, fixups);
}

fn planMult(
    cx: *Context,
    room_number: u8,
    node: *const @FieldType(Ast.Node, "mult"),
) !void {
    const event_index = cx.next_event_index;
    cx.next_event_index += 1;

    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.pool.spawn(runMult, .{ cx, event_index, room_number, node });
}

fn runMult(
    cx: *Context,
    event_index: u16,
    room_number: u8,
    node: *const @FieldType(Ast.Node, "mult"),
) void {
    buildMult(cx, event_index, room_number, node) catch |err| {
        if (err != error.AddedToDiagnostic)
            cx.diagnostic.zigErr("{s} {}: unexpected error: {s}", .{ "MULT", node.glob_number }, err);
        cx.events.send(.{ .index = event_index, .payload = .err });
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn buildMult(
    cx: *const Context,
    event_index: u16,
    room_number: u8,
    mult: *const @FieldType(Ast.Node, "mult"),
) !void {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    var stream = std.io.countingWriter(out.writer(cx.gpa));
    try buildMultInner(cx, room_number, mult, &stream, &fixups);
    applyFixups(out.items, fixups.items);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("MULT"),
            .glob_number = mult.glob_number,
            .data = out,
        } },
    });
}

fn buildMultInner(
    cx: *const Context,
    room_number: u8,
    mult_node: *const @FieldType(Ast.Node, "mult"),
    out: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    const room_file = &cx.project.files.items[room_number].?;

    if (mult_node.raw_defa_path) |path| {
        const defa_start = try beginBlock(out, "DEFA");
        try fs.readFileInto(cx.project_dir, path, out.writer());
        try endBlock(out, fixups, defa_start);
    }

    const wrap_start = try beginBlock(out, "WRAP");

    const offs_start = try beginBlock(out, "OFFS");
    // just write garbage bytes for now, they'll be replaced at the end
    try out.writer().writeAll(std.mem.sliceAsBytes(room_file.ast.getExtra(mult_node.indices)));
    try endBlock(out, fixups, offs_start);

    var awiz_offsets: std.BoundedArray(u32, Ast.max_mult_children) = .{};
    for (room_file.ast.getExtra(mult_node.children)) |node| {
        awiz_offsets.appendAssumeCapacity(@as(u32, @intCast(out.bytes_written)) - offs_start);
        const wiz = &room_file.ast.nodes.items[node].mult_awiz;
        const awiz_start = try beginBlock(out, "AWIZ");
        try buildAwizInner(cx, room_number, wiz.children, out, fixups);
        try endBlock(out, fixups, awiz_start);
    }

    var off_pos = offs_start + block_header_size;
    for (room_file.ast.getExtra(mult_node.indices)) |i| {
        try fixups.append(.{
            .offset = off_pos,
            .bytes = Fixup.encode(awiz_offsets.get(i), .little),
        });
        off_pos += 4;
    }

    try endBlock(out, fixups, wrap_start);
}

fn planIndex(cx: *Context) !void {
    cx.sendSyncEvent(.index_start);

    const project_file = &cx.project.files.items[0].?;
    const project_root = &project_file.ast.nodes.items[project_file.ast.root].project;
    for (project_file.ast.getExtra(project_root.index)) |node| {
        switch (project_file.ast.nodes.items[node]) {
            .raw_block => |*n| try planRawBlock(cx, n),
            .index_block => |b| switch (b) {
                .RNAM => try planRoomNames(cx),
                else => cx.sendSyncEvent(.{ .index_block = b }),
            },
            else => unreachable,
        }
    }

    cx.sendSyncEvent(.index_end);
}

fn planRoomNames(cx: *Context) !void {
    var result: std.ArrayListUnmanaged(u8) = .empty;
    errdefer result.deinit(cx.gpa);

    // Collect rooms by number
    var room_nodes: std.BoundedArray(Ast.NodeIndex, 256) = .{};
    const project_file = &cx.project.files.items[0].?;
    const project_root = &project_file.ast.nodes.items[project_file.ast.root].project;
    for (project_file.ast.getExtra(project_root.disks)) |disk_node| {
        if (disk_node == Ast.null_node) continue;
        const disk = &project_file.ast.nodes.items[disk_node].disk;
        for (project_file.ast.getExtra(disk.children)) |child_node| {
            const child = &project_file.ast.nodes.items[child_node];
            if (child.* != .disk_room) continue;
            const room = &child.disk_room;
            utils.growBoundedArray(&room_nodes, room.room_number + 1, Ast.null_node);
            room_nodes.set(room.room_number, child_node);
        }
    }

    // Write room names in order
    for (room_nodes.slice()) |room_node| {
        if (room_node == Ast.null_node) continue;
        const room = &project_file.ast.nodes.items[room_node].disk_room;
        try result.appendSlice(cx.gpa, std.mem.asBytes(&@as(u16, room.room_number)));
        try result.appendSlice(cx.gpa, room.name);
        try result.append(cx.gpa, 0);
    }
    try result.appendSlice(cx.gpa, &.{ 0, 0 });

    cx.sendSyncEvent(.{ .raw_block = .{
        .block_id = blockId("RNAM"),
        .data = result,
    } });
}
