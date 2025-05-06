const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const akos = @import("akos.zig");
const assemble = @import("assemble.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const block_header_size = @import("block_reader.zig").block_header_size;
const Fixup = @import("block_writer.zig").Fixup;
const applyFixups = @import("block_writer.zig").applyFixups;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockImpl = @import("block_writer.zig").beginBlockImpl;
const endBlock = @import("block_writer.zig").endBlock;
const compile = @import("compile.zig");
const fs = @import("fs.zig");
const games = @import("games.zig");
const io = @import("io.zig");
const lang = @import("lang.zig");
const rmim_encode = @import("rmim_encode.zig");
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
    var cx: Context = undefined;

    (blk: {
        const language = lang.buildLanguage(game);
        const ins_map = lang.buildInsMap(gpa, &language) catch |err| break :blk err;
        cx = .{
            .gpa = gpa,
            .diagnostic = diagnostic,
            .game = game,
            .project_dir = project_dir,
            .project = project,
            .awiz_strategy = awiz_strategy,
            .language = &language,
            .ins_map = &ins_map,
            .project_scope = .empty,
            .room_scopes = undefined,
            .pool = pool,
            .events = events,
            .next_event_index = 0,
            .pending_jobs = .init(0),
        };

        planProject(&cx) catch |err| break :blk err;
    }) catch |err| {
        if (err != error.AddedToDiagnostic)
            diagnostic.zigErr("unexpected error: {s}", .{}, err);
        cx.sendSyncEvent(.err);
        cx.sendSyncEvent(.project_end);
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
    language: *const lang.Language,
    ins_map: *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    project_scope: std.StringHashMapUnmanaged(lang.Variable),
    room_scopes: [256]std.StringHashMapUnmanaged(lang.Variable),
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

    for (project_file.ast.getExtra(project_node.variables)) |var_node_index| {
        const var_node = &project_file.ast.nodes.items[var_node_index].variable;
        const scope_entry = try cx.project_scope.getOrPut(cx.gpa, var_node.name);
        if (scope_entry.found_existing) return error.BadData;
        scope_entry.value_ptr.* = .init(.{ .global = var_node.number });
    }

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

    const room_scope = &cx.room_scopes[room.room_number];
    room_scope.* = .empty;
    for (room_file.ast.getExtra(root.variables)) |var_node_index| {
        const var_node = &room_file.ast.nodes.items[var_node_index].variable;
        const scope_entry = try room_scope.getOrPut(cx.gpa, var_node.name);
        if (scope_entry.found_existing) return error.BadData;
        scope_entry.value_ptr.* = .init(.{ .room = var_node.number });
    }

    var start: [160]RoomWork = undefined;
    var rmda_excd: [1]RoomWork = undefined;
    var rmda_encd: [1]RoomWork = undefined;
    var rmda_lscr: [640]RoomWork = undefined;
    var end: [5120]RoomWork = undefined;
    var state: RoomPlan = .{
        .work = .init(.{
            .start = .initBuffer(&start),
            .rmda_excd = .initBuffer(&rmda_excd),
            .rmda_encd = .initBuffer(&rmda_encd),
            .rmda_lscr = .initBuffer(&rmda_lscr),
            .end = .initBuffer(&end),
        }),
        .cur_section = .start,
    };

    try scanRoom(cx, &state, room.room_number);
    try scheduleRoom(cx, &state, room.room_number);
}

const RoomPlan = struct {
    work: std.EnumArray(RoomSection, std.ArrayListUnmanaged(RoomWork)),
    cur_section: RoomSection,

    fn add(self: *RoomPlan, section: RoomSection, work: RoomWork) !void {
        try self.work.getPtr(section).append(utils.null_allocator, work);
    }
};

const RoomSection = enum {
    start,
    rmda_excd,
    rmda_encd,
    rmda_lscr,
    end,
};

const RoomWork = union(enum) {
    event: Payload,
    node: Ast.NodeIndex,
};

fn scanRoom(cx: *Context, plan: *RoomPlan, room_number: u8) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const root = &room_file.ast.nodes.items[room_file.ast.root].room_file;
    for (room_file.ast.getExtra(root.children)) |child_node| {
        switch (room_file.ast.nodes.items[child_node]) {
            .raw_glob_file, .raw_glob_block, .raw_block, .rmim => {
                try plan.add(plan.cur_section, .{ .node = child_node });
            },
            .rmda => |*rmda| try scanRmda(cx, plan, room_number, rmda),
            .scrp => try plan.add(.end, .{ .node = child_node }),
            .excd => try plan.add(.rmda_excd, .{ .node = child_node }),
            .encd => try plan.add(.rmda_encd, .{ .node = child_node }),
            .lscr => try plan.add(.rmda_lscr, .{ .node = child_node }),
            .awiz, .mult, .akos => try plan.add(.end, .{ .node = child_node }),
            .script => try plan.add(.end, .{ .node = child_node }),
            else => unreachable,
        }
    }
}

fn scanRmda(
    cx: *Context,
    plan: *RoomPlan,
    room_number: u8,
    rmda: *const @FieldType(Ast.Node, "rmda"),
) !void {
    try plan.add(plan.cur_section, .{ .event = .{ .glob_start = .{
        .block_id = blockId("RMDA"),
        .glob_number = room_number,
    } } });
    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(rmda.children)) |node_index| {
        const raw_block = &room_file.ast.nodes.items[node_index].raw_block;
        if (raw_block.block_id == blockId("POLD"))
            plan.cur_section = .end;
        const section = sectionForBlockId(raw_block.block_id) orelse plan.cur_section;
        try plan.add(section, .{ .node = node_index });
    }
    plan.cur_section = .end;
    try plan.add(plan.cur_section, .{ .event = .glob_end });
}

fn sectionForBlockId(block_id: BlockId) ?RoomSection {
    return switch (block_id) {
        blockId("EXCD") => .rmda_excd,
        blockId("ENCD") => .rmda_encd,
        blockId("NLSC") => .rmda_lscr,
        blockId("LSCR") => .rmda_lscr,
        blockId("LSC2") => .rmda_lscr,
        else => null,
    };
}

fn scheduleRoom(cx: *Context, plan: *const RoomPlan, room_number: u8) !void {
    const room_file = &cx.project.files.items[room_number].?;
    for (std.meta.tags(RoomSection)) |section| {
        for (plan.work.getPtrConst(section).items) |work| {
            switch (work) {
                .event => |payload| cx.sendSyncEvent(payload),
                .node => |child_node| switch (room_file.ast.nodes.items[child_node]) {
                    .raw_glob_file => |*n| try planRawGlobFile(cx, n),
                    .raw_glob_block => |*n| try planRawGlobBlock(cx, room_number, n),
                    .raw_block => |*n| try planRawBlock(cx, n),
                    .rmim => try spawnJob(planRmim, cx, room_number, child_node),
                    .scrp => try spawnJob(planScrp, cx, room_number, child_node),
                    .encd, .excd => try spawnJob(planEncdExcd, cx, room_number, child_node),
                    .lscr => try spawnJob(planLscr, cx, room_number, child_node),
                    .awiz => try spawnJob(planAwiz, cx, room_number, child_node),
                    .mult => try spawnJob(planMult, cx, room_number, child_node),
                    .akos => try spawnJob(planAkos, cx, room_number, child_node),
                    .script => try spawnJob(planScript, cx, room_number, child_node),
                    else => unreachable,
                },
            }
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
        const child = &room_file.ast.nodes.items[node].raw_block;
        try planRawBlock(cx, child);
    }

    cx.sendSyncEvent(.glob_end);
}

fn planRmda(cx: *Context, room_number: u8, node_index: u32) !void {
    const rmda = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].rmda;

    cx.sendSyncEvent(.{ .glob_start = .{
        .block_id = blockId("RMDA"),
        .glob_number = room_number,
    } });

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(rmda.children)) |node| {
        const child = &room_file.ast.nodes.items[node];
        switch (child.*) {
            .raw_block => |*n| try planRawBlock(cx, n),
            .encd, .excd => try spawnJob(planEncdExcd, cx, room_number, node),
            .lscr => try spawnJob(planLscr, cx, room_number, node),
            else => unreachable,
        }
    }

    cx.sendSyncEvent(.glob_end);
}

const Job = fn (cx: *const Context, room_number: u8, node_index: u32, event_index: u16) anyerror!void;

fn spawnJob(job: Job, cx: *Context, room_number: u8, node_index: u32) !void {
    const event_index = cx.next_event_index;
    cx.next_event_index += 1;

    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.pool.spawn(runJob, .{ job, cx, room_number, node_index, event_index });
}

fn runJob(job: Job, cx: *Context, room_number: u8, node_index: u32, event_index: u16) void {
    job(cx, room_number, node_index, event_index) catch |err| {
        if (err != error.AddedToDiagnostic)
            cx.diagnostic.zigErr("unexpected error: {s}", .{}, err);
        cx.events.send(.{ .index = event_index, .payload = .err });
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn planRmim(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const rmim = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].rmim;

    const bmp = try fs.readFile(cx.gpa, cx.project_dir, rmim.path);
    defer cx.gpa.free(bmp);

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    var stream = std.io.countingWriter(out.writer(cx.gpa));
    try rmim_encode.encode(rmim.compression, bmp, &stream, &fixups);
    applyFixups(out.items, fixups.items);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("RMIM"),
            .glob_number = room_number,
            .data = out,
        } },
    });
}

fn planScrp(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const scrp = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].scrp;

    const in = try fs.readFile(cx.gpa, cx.project_dir, scrp.path);
    defer cx.gpa.free(in);

    const id: Symbols.ScriptId = .{ .global = scrp.glob_number };
    const result = try assemble.assemble(cx.gpa, cx.language, cx.ins_map, in, &cx.project_scope, &.{}, id);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("SCRP"),
            .glob_number = scrp.glob_number,
            .data = result,
        } },
    });
}

fn planEncdExcd(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index];
    const edge: enum { encd, excd }, const path = switch (node.*) {
        .encd => |*n| .{ .encd, n.path },
        .excd => |*n| .{ .excd, n.path },
        else => unreachable,
    };

    const in = try fs.readFile(cx.gpa, cx.project_dir, path);
    defer cx.gpa.free(in);

    const id: Symbols.ScriptId = switch (edge) {
        .encd => .{ .enter = .{ .room = room_number } },
        .excd => .{ .exit = .{ .room = room_number } },
    };
    var result = try assemble.assemble(cx.gpa, cx.language, cx.ins_map, in, &cx.project_scope, &cx.room_scopes[room_number], id);
    errdefer result.deinit(cx.gpa);

    const block_id = switch (edge) {
        .encd => blockId("ENCD"),
        .excd => blockId("EXCD"),
    };
    cx.events.send(.{
        .index = event_index,
        .payload = .{ .raw_block = .{
            .block_id = block_id,
            .data = result,
        } },
    });
}

fn planLscr(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const lscr = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].lscr;

    const in = try fs.readFile(cx.gpa, cx.project_dir, lscr.path);
    defer cx.gpa.free(in);

    const id: Symbols.ScriptId = .{ .local = .{
        .room = room_number,
        .number = lscr.script_number,
    } };
    var bytecode = try assemble.assemble(cx.gpa, cx.language, cx.ins_map, in, &cx.project_scope, &cx.room_scopes[room_number], id);
    defer bytecode.deinit(cx.gpa);

    const result = try cx.gpa.alloc(u8, 4 + bytecode.items.len);
    errdefer cx.gpa.free(result);
    std.mem.writeInt(i32, result[0..4], lscr.script_number, .little);
    @memcpy(result[4..], bytecode.items); // TODO: avoid this memcpy

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .raw_block = .{
            .block_id = blockId("LSC2"),
            .data = .fromOwnedSlice(result),
        } },
    });
}

fn planAwiz(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const awiz_node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].awiz;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    var stream = std.io.countingWriter(out.writer(cx.gpa));
    try planAwizInner(cx, room_number, awiz_node.children, &stream, &fixups);
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

fn planAwizInner(
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

fn planMult(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const mult = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].mult;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    var stream = std.io.countingWriter(out.writer(cx.gpa));
    try planMultInner(cx, room_number, mult, &stream, &fixups);
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

fn planMultInner(
    cx: *const Context,
    room_number: u8,
    mult_node: *const @FieldType(Ast.Node, "mult"),
    out: anytype,
    fixups: *std.ArrayList(Fixup),
) !void {
    const room_file = &cx.project.files.items[room_number].?;

    if (mult_node.raw_block != Ast.null_node) {
        const defa = &room_file.ast.nodes.items[mult_node.raw_block].raw_block_nested;
        const defa_start = try beginBlock(out, "DEFA");
        for (room_file.ast.getExtra(defa.children)) |child_node| {
            const child = &room_file.ast.nodes.items[child_node].raw_block;

            const file = try cx.project_dir.openFile(child.path, .{});
            defer file.close();

            const child_start = try beginBlockImpl(out, child.block_id);
            try io.copy(file, out.writer());
            try endBlock(out, fixups, child_start);
        }
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
        try planAwizInner(cx, room_number, wiz.children, out, fixups);
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

fn planAkos(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].akos;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    var fixups: std.ArrayList(Fixup) = .init(cx.gpa);
    defer fixups.deinit();

    var stream = std.io.countingWriter(out.writer(cx.gpa));
    try akos.encode(cx.gpa, cx.project, cx.project_dir, cx.awiz_strategy, room_number, node_index, &stream, &fixups);
    applyFixups(out.items, fixups.items);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("AKOS"),
            .glob_number = node.glob_number,
            .data = out,
        } },
    });
}

fn planScript(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const script = &room_file.ast.nodes.items[node_index].script;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, cx.language, cx.ins_map, &room_file.ast, script.statements, &out);

    cx.events.send(.{
        .index = event_index,
        .payload = .{ .glob = .{
            .block_id = blockId("SCRP"),
            .glob_number = script.glob_number,
            .data = out,
        } },
    });
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
