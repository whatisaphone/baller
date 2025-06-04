const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const Project = @import("Project.zig");
const Symbols = @import("Symbols.zig");
const akos = @import("akos.zig");
const assemble = @import("assemble.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const block_header_size = @import("block_reader.zig").block_header_size;
const Fixup = @import("block_writer.zig").Fixup;
const applyFixups = @import("block_writer.zig").applyFixups;
const beginBlock = @import("block_writer.zig").beginBlock;
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlock = @import("block_writer.zig").endBlock;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const compile = @import("compile.zig");
const decompile = @import("decompile.zig");
const VerbEntry = @import("extract.zig").VerbEntry;
const fs = @import("fs.zig");
const games = @import("games.zig");
const lang = @import("lang.zig");
const obim = @import("obim.zig");
const rmim_encode = @import("rmim_encode.zig");
const script = @import("script.zig");
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

    // Bit messy, watch out
    cx.project_scope = .empty;
    defer cx.project_scope.deinit(gpa);
    var room_scopes: [256]std.StringHashMapUnmanaged(script.Symbol) = @splat(.empty);
    defer for (&room_scopes) |*s| s.deinit(gpa);

    (blk: {
        const vm = lang.buildVm(game);
        const op_map = decompile.buildOpMap(game);
        cx = .{
            .gpa = gpa,
            .diagnostic = diagnostic,
            .game = game,
            .project_dir = project_dir,
            .project = project,
            .awiz_strategy = awiz_strategy,
            .vm = &vm,
            .op_map = &op_map,
            .project_scope = .empty,
            .room_scopes = &room_scopes,
            .room_lsc_types = @splat(.undef),
            .pool = pool,
            .events = events,
            .next_event_index = 0,
            .pending_jobs = .init(0),
        };

        diagnostic.trace("planning jobs", .{});
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
    vm: *const lang.Vm,
    op_map: *const std.EnumArray(lang.Op, decompile.Op),
    project_scope: std.StringHashMapUnmanaged(script.Symbol),
    room_scopes: *[256]std.StringHashMapUnmanaged(script.Symbol),
    room_lsc_types: [256]utils.SafeUndefined(LocalScriptBlockType),
    pool: *std.Thread.Pool,
    events: *sync.Channel(Event, 16),
    next_event_index: u16,
    pending_jobs: std.atomic.Value(u32),

    fn sendEvent(self: *const Context, index: u16, payload: Payload) void {
        self.events.send(.{ .index = index, .payload = payload });
    }

    fn sendSyncEvent(self: *Context, payload: Payload) void {
        self.sendEvent(self.next_event_index, payload);
        self.next_event_index += 1;
    }
};

fn planProject(cx: *Context) !void {
    const project_file = &cx.project.files.items[0].?;
    const project_node = &project_file.ast.nodes.items[project_file.ast.root].project;

    try buildProjectScope(cx);

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

fn buildProjectScope(cx: *Context) !void {
    const project_file = &cx.project.files.items[0].?;
    const project_node = &project_file.ast.nodes.items[project_file.ast.root].project;

    for (project_file.ast.getExtra(project_node.variables)) |node_index| {
        const var_node = &project_file.ast.nodes.items[node_index].variable;
        const var_num = std.math.cast(u14, var_node.number) orelse return error.BadData;
        const symbol: script.Symbol = .{ .variable = .init(.global, var_num) };
        try addScopeSymbol(cx, &cx.project_scope, var_node.name, symbol);
    }

    for (project_file.ast.getExtra(project_node.disks)) |disk_node| {
        if (disk_node == Ast.null_node) continue;
        const disk = &project_file.ast.nodes.items[disk_node].disk;
        for (project_file.ast.getExtra(disk.children)) |room_node| {
            const disk_room = &project_file.ast.nodes.items[room_node].disk_room;

            const room_symbol: script.Symbol = .{ .constant = disk_room.room_number };
            try addScopeSymbol(cx, &cx.project_scope, disk_room.name, room_symbol);

            const room_file = &cx.project.files.items[disk_room.room_number].?;
            const root = &room_file.ast.nodes.items[room_file.ast.root].room_file;
            for (room_file.ast.getExtra(root.children)) |child_index| {
                switch (room_file.ast.nodes.items[child_index]) {
                    .raw_glob_file => |n| if (n.name) |name| {
                        const symbol: script.Symbol = .{ .constant = n.glob_number };
                        try addScopeSymbol(cx, &cx.project_scope, name, symbol);
                    },
                    .raw_glob_block => |n| if (n.name) |name| {
                        const symbol: script.Symbol = .{ .constant = n.glob_number };
                        try addScopeSymbol(cx, &cx.project_scope, name, symbol);
                    },
                    .scr => |n| {
                        const symbol: script.Symbol = .{ .constant = n.glob_number };
                        try addScopeSymbol(cx, &cx.project_scope, n.name, symbol);
                    },
                    .script => |n| {
                        const symbol: script.Symbol = .{ .constant = n.glob_number };
                        try addScopeSymbol(cx, &cx.project_scope, n.name, symbol);
                    },
                    else => {},
                }
            }
        }
    }
}

fn addScopeSymbol(
    cx: *const Context,
    scope: *std.StringHashMapUnmanaged(script.Symbol),
    name: []const u8,
    symbol: script.Symbol,
) !void {
    const entry = try scope.getOrPut(cx.gpa, name);
    if (entry.found_existing) {
        cx.diagnostic.err("duplicate name: {s}", .{name});
        return error.AddedToDiagnostic;
    }
    entry.value_ptr.* = symbol;
}

fn planRoom(cx: *Context, room: *const @FieldType(Ast.Node, "disk_room")) !void {
    try buildRoomScope(cx, room.room_number);

    var start: [6]RoomWork = undefined;
    var rmda_obim: [64]RoomWork = undefined;
    var rmda_obcd: [64]RoomWork = undefined;
    var rmda_excd: [1]RoomWork = undefined;
    var rmda_encd: [1]RoomWork = undefined;
    var rmda_lsc: [640]RoomWork = undefined;
    var end: [5120]RoomWork = undefined;
    var state: RoomPlan = .{
        .work = .init(.{
            .start = .initBuffer(&start),
            .rmda_obim = .initBuffer(&rmda_obim),
            .rmda_obcd = .initBuffer(&rmda_obcd),
            .rmda_excd = .initBuffer(&rmda_excd),
            .rmda_encd = .initBuffer(&rmda_encd),
            .rmda_lsc = .initBuffer(&rmda_lsc),
            .end = .initBuffer(&end),
        }),
        .cur_section = .start,
    };

    try scanRoom(cx, &state, room.room_number);
    try scheduleRoom(cx, &state, room.room_number);
}

fn buildRoomScope(cx: *Context, room_number: u8) !void {
    const room_scope = &cx.room_scopes[room_number];

    const room_file = &cx.project.files.items[room_number].?;
    const root = &room_file.ast.nodes.items[room_file.ast.root].room_file;

    for (room_file.ast.getExtra(root.variables)) |node_index| {
        const node = &room_file.ast.nodes.items[node_index].variable;
        const num = std.math.cast(u14, node.number) orelse return error.BadData;
        const symbol: script.Symbol = .{ .variable = .init(.room, num) };
        try addScopeSymbol(cx, room_scope, node.name, symbol);
    }

    for (room_file.ast.getExtra(root.children)) |node_index| {
        switch (room_file.ast.nodes.items[node_index]) {
            .lsc => |n| {
                const symbol: script.Symbol = .{ .constant = n.script_number };
                try addScopeSymbol(cx, room_scope, n.name, symbol);
            },
            .local_script => |n| {
                const symbol: script.Symbol = .{ .constant = n.script_number };
                try addScopeSymbol(cx, room_scope, n.name, symbol);
            },
            else => {},
        }
    }
}

const RoomPlan = struct {
    work: std.EnumArray(RoomSection, std.ArrayListUnmanaged(RoomWork)),
    cur_section: RoomSection,

    fn add(self: *RoomPlan, section: RoomSection, work: RoomWork) !void {
        try self.work.getPtr(section).append(utils.null_allocator, work);
    }
};

const LocalScriptBlockType = enum(BlockId.Raw) {
    lscr = BlockId.LSCR.raw(),
    lsc2 = BlockId.LSC2.raw(),

    fn blockId(self: LocalScriptBlockType) BlockId {
        return BlockId.init(@intFromEnum(self)).?;
    }
};

const RoomSection = enum {
    start,
    rmda_obim,
    rmda_obcd,
    rmda_excd,
    rmda_encd,
    rmda_lsc,
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
            inline .raw_glob_file, .raw_glob_block, .raw_block => |n| {
                const section = sectionForBlockId(n.block_id) orelse plan.cur_section;
                try plan.add(section, .{ .node = child_node });
            },
            .rmim => try plan.add(plan.cur_section, .{ .node = child_node }),
            .rmda => |*rmda| try scanRmda(cx, plan, room_number, rmda),
            .scr => try plan.add(.end, .{ .node = child_node }),
            .excd => try plan.add(.rmda_excd, .{ .node = child_node }),
            .encd => try plan.add(.rmda_encd, .{ .node = child_node }),
            .lsc => try plan.add(.rmda_lsc, .{ .node = child_node }),
            .obim => try plan.add(.rmda_obim, .{ .node = child_node }),
            .awiz, .mult, .akos => try plan.add(.end, .{ .node = child_node }),
            .script => try plan.add(.end, .{ .node = child_node }),
            .local_script => try plan.add(.rmda_lsc, .{ .node = child_node }),
            .enter => try plan.add(.rmda_encd, .{ .node = child_node }),
            .exit => try plan.add(.rmda_excd, .{ .node = child_node }),
            .object => try plan.add(.rmda_obcd, .{ .node = child_node }),
            else => unreachable,
        }
    }

    // Even if there's no enter/exit scripts, the original compiler still
    // outputs an empty block.
    if (plan.work.getPtr(.rmda_excd).items.len == 0)
        plan.work.getPtr(.rmda_excd).appendAssumeCapacity(.{ .event = .{ .raw_block = .{
            .block_id = .EXCD,
            .data = .empty,
        } } });
    if (plan.work.getPtr(.rmda_encd).items.len == 0)
        plan.work.getPtr(.rmda_encd).appendAssumeCapacity(.{ .event = .{ .raw_block = .{
            .block_id = .ENCD,
            .data = .empty,
        } } });

    // Any rooms where every script number < 256, all local scripts are LSCR.
    // Otherwise all are LSC2.
    var max_lsc_number: u16 = 0;
    for (plan.work.getPtrConst(.rmda_lsc).items) |*work| {
        if (work.* == .node) {
            const number: ?u16 = switch (room_file.ast.nodes.items[work.node]) {
                inline .lsc, .local_script => |n| n.script_number,
                else => null,
            };
            if (number) |num|
                max_lsc_number = @max(max_lsc_number, num);
        }
    }
    cx.room_lsc_types[room_number].setOnce(if (max_lsc_number < 256) .lscr else .lsc2);
}

fn scanRmda(
    cx: *Context,
    plan: *RoomPlan,
    room_number: u8,
    rmda: *const @FieldType(Ast.Node, "rmda"),
) !void {
    try plan.add(plan.cur_section, .{ .event = .{ .glob_start = .{
        .block_id = .RMDA,
        .glob_number = room_number,
    } } });
    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(rmda.children)) |node_index| {
        const raw_block = &room_file.ast.nodes.items[node_index].raw_block;
        if (raw_block.block_id == .OBCD)
            plan.cur_section = .rmda_obcd;
        if (raw_block.block_id == .POLD)
            plan.cur_section = .end;
        const section = sectionForBlockId(raw_block.block_id) orelse plan.cur_section;
        try plan.add(section, .{ .node = node_index });
    }
    plan.cur_section = .end;
    try plan.add(plan.cur_section, .{ .event = .glob_end });
}

fn sectionForBlockId(block_id: BlockId) ?RoomSection {
    return switch (block_id) {
        .OBIM => .rmda_obim,
        .OBCD => .rmda_obcd,
        .EXCD => .rmda_excd,
        .ENCD => .rmda_encd,
        .NLSC => .rmda_lsc,
        .LSCR => .rmda_lsc,
        .LSC2 => .rmda_lsc,
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
                    .scr => try spawnJob(planScr, cx, room_number, child_node),
                    .encd, .excd => try spawnJob(planEncdExcd, cx, room_number, child_node),
                    .lsc => try spawnJob(planLsc, cx, room_number, child_node),
                    .obim => try spawnJob(planObim, cx, room_number, child_node),
                    .awiz => try spawnJob(planAwiz, cx, room_number, child_node),
                    .mult => try spawnJob(planMult, cx, room_number, child_node),
                    .akos => try spawnJob(planAkos, cx, room_number, child_node),
                    .script => try spawnJob(planScript, cx, room_number, child_node),
                    .local_script => try spawnJob(planLocalScript, cx, room_number, child_node),
                    .enter => try spawnJob(planEnterScript, cx, room_number, child_node),
                    .exit => try spawnJob(planExitScript, cx, room_number, child_node),
                    .object => try spawnJob(planObject, cx, room_number, child_node),
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
        .block_id = .RMDA,
        .glob_number = room_number,
    } });

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(rmda.children)) |node| {
        const child = &room_file.ast.nodes.items[node];
        switch (child.*) {
            .raw_block => |*n| try planRawBlock(cx, n),
            .encd, .excd => try spawnJob(planEncdExcd, cx, room_number, node),
            .lsc => try spawnJob(planLsc, cx, room_number, node),
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
        cx.sendEvent(event_index, .err);
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

    try rmim_encode.encode(cx.gpa, rmim.compression, bmp, &out);

    cx.sendEvent(event_index, .{ .glob = .{
        .block_id = .RMIM,
        .glob_number = room_number,
        .data = out,
    } });
}

fn planScr(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const scr = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].scr;

    const in = try fs.readFile(cx.gpa, cx.project_dir, scr.path);
    defer cx.gpa.free(in);

    const id: Symbols.ScriptId = .{ .global = scr.glob_number };
    const result = try assemble.assemble(cx.gpa, cx.vm, in, &cx.project_scope, &cx.room_scopes[room_number], id);

    cx.sendEvent(event_index, .{ .glob = .{
        .block_id = .SCRP,
        .glob_number = scr.glob_number,
        .data = result,
    } });
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
    var result = try assemble.assemble(cx.gpa, cx.vm, in, &cx.project_scope, &cx.room_scopes[room_number], id);
    errdefer result.deinit(cx.gpa);

    const block_id: BlockId = switch (edge) {
        .encd => .ENCD,
        .excd => .EXCD,
    };
    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = block_id,
        .data = result,
    } });
}

fn planLsc(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const lsc = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].lsc;

    const in = try fs.readFile(cx.gpa, cx.project_dir, lsc.path);
    defer cx.gpa.free(in);

    const id: Symbols.ScriptId = .{ .local = .{
        .room = room_number,
        .number = lsc.script_number,
    } };
    var bytecode = try assemble.assemble(cx.gpa, cx.vm, in, &cx.project_scope, &cx.room_scopes[room_number], id);
    defer bytecode.deinit(cx.gpa);

    const block_type = cx.room_lsc_types[room_number].defined;
    const script_number_size: usize = switch (block_type) {
        .lscr => 1,
        .lsc2 => 4,
    };
    const result = try cx.gpa.alloc(u8, script_number_size + bytecode.items.len);
    errdefer cx.gpa.free(result);
    switch (block_type) {
        .lscr => result[0] = @intCast(lsc.script_number),
        .lsc2 => std.mem.writeInt(i32, result[0..4], lsc.script_number, .little),
    }
    @memcpy(result[script_number_size..], bytecode.items); // TODO: avoid this memcpy

    const block_id: BlockId = switch (block_type) {
        .lscr => .LSCR,
        .lsc2 => .LSC2,
    };
    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = block_id,
        .data = .fromOwnedSlice(result),
    } });
}

fn planObim(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compileObim(cx, room_number, node_index, &out);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .OBIM,
        .data = out,
    } });
}

fn compileObim(
    cx: *const Context,
    room_number: u8,
    node_index: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const obim_node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].obim;

    var im_index: usize = 0;

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(obim_node.children)) |child_index| {
        switch (room_file.ast.nodes.items[child_index]) {
            .raw_block => |n| {
                try encodeRawBlock(cx.gpa, out, n.block_id, cx.project_dir, n.path);
            },
            .obim_im => {
                const block_id = obim.makeImBlockId(im_index) orelse return error.BadData;
                const im_start = try beginBlockAl(cx.gpa, out, block_id);
                try compileIm(cx, room_number, child_index, out);
                try endBlockAl(out, im_start);
                im_index += 1;
            },
            else => unreachable,
        }
    }
}

fn compileIm(
    cx: *const Context,
    room_number: u8,
    node_index: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const im_node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].obim_im;

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(im_node.children)) |child_index| {
        switch (room_file.ast.nodes.items[child_index]) {
            .raw_block => |n| {
                try encodeRawBlock(cx.gpa, out, n.block_id, cx.project_dir, n.path);
            },
            else => unreachable,
        }
    }
}

pub fn encodeRawBlock(
    gpa: std.mem.Allocator,
    out: *std.ArrayListUnmanaged(u8),
    block_id: BlockId,
    dir: std.fs.Dir,
    path: []const u8,
) !void {
    const start = try beginBlockAl(gpa, out, block_id);
    try fs.readFileInto(dir, path, out.writer(gpa));
    try endBlockAl(out, start);
}

fn planAwiz(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const awiz_node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].awiz;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try planAwizInner(cx, room_number, awiz_node.children, &out);

    cx.sendEvent(event_index, .{ .glob = .{
        .block_id = .AWIZ,
        .glob_number = awiz_node.glob_number,
        .data = out,
    } });
}

fn planAwizInner(
    cx: *const Context,
    room_number: u8,
    children: Ast.ExtraSlice,
    out: *std.ArrayListUnmanaged(u8),
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

    try awiz.encode(cx.gpa, &the_awiz, cx.awiz_strategy, out);
}

fn planMult(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const mult = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].mult;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try planMultInner(cx, room_number, mult, &out);

    cx.sendEvent(event_index, .{ .glob = .{
        .block_id = .MULT,
        .glob_number = mult.glob_number,
        .data = out,
    } });
}

fn planMultInner(
    cx: *const Context,
    room_number: u8,
    mult_node: *const @FieldType(Ast.Node, "mult"),
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const room_file = &cx.project.files.items[room_number].?;

    if (mult_node.raw_block != Ast.null_node) {
        const defa = &room_file.ast.nodes.items[mult_node.raw_block].raw_block_nested;
        const defa_start = try beginBlockAl(cx.gpa, out, .DEFA);
        for (room_file.ast.getExtra(defa.children)) |child_node| {
            const child = &room_file.ast.nodes.items[child_node].raw_block;
            try encodeRawBlock(cx.gpa, out, child.block_id, cx.project_dir, child.path);
        }
        try endBlockAl(out, defa_start);
    }

    const wrap_start = try beginBlockAl(cx.gpa, out, .WRAP);

    const offs_start = try beginBlockAl(cx.gpa, out, .OFFS);
    // the offsets are filled in at the end
    _ = try out.addManyAsSlice(cx.gpa, mult_node.indices.len * 4);
    try endBlockAl(out, offs_start);

    var awiz_offsets: std.BoundedArray(u32, Ast.max_mult_children) = .{};
    for (room_file.ast.getExtra(mult_node.children)) |node| {
        awiz_offsets.appendAssumeCapacity(@as(u32, @intCast(out.items.len)) - offs_start);
        const wiz = &room_file.ast.nodes.items[node].mult_awiz;
        const awiz_start = try beginBlockAl(cx.gpa, out, .AWIZ);
        try planAwizInner(cx, room_number, wiz.children, out);
        try endBlockAl(out, awiz_start);
    }

    var off_pos = offs_start + block_header_size;
    for (room_file.ast.getExtra(mult_node.indices)) |i| {
        std.mem.writeInt(u32, out.items[off_pos..][0..4], awiz_offsets.get(i), .little);
        off_pos += 4;
    }

    try endBlockAl(out, wrap_start);
}

fn planAkos(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const node = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].akos;

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try akos.encode(cx.gpa, cx.project, cx.project_dir, cx.awiz_strategy, room_number, node_index, &out);

    cx.sendEvent(event_index, .{ .glob = .{
        .block_id = .AKOS,
        .glob_number = node.glob_number,
        .data = out,
    } });
}

fn planScript(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.items[node_index].script;

    const diag: Diagnostic.ForTextFile = .{
        .diagnostic = cx.diagnostic,
        .path = room_file.path,
    };

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, &diag, cx.vm, cx.op_map, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, node.statements, &out);

    cx.sendEvent(event_index, .{ .glob = .{
        .block_id = .SCRP,
        .glob_number = node.glob_number,
        .data = out,
    } });
}

fn planLocalScript(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.items[node_index].local_script;

    const diag: Diagnostic.ForTextFile = .{
        .diagnostic = cx.diagnostic,
        .path = room_file.path,
    };

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    const lsc_type = cx.room_lsc_types[room_number].defined;
    switch (lsc_type) {
        .lscr => try out.append(cx.gpa, @intCast(node.script_number)),
        .lsc2 => try out.writer(cx.gpa).writeInt(u32, node.script_number, .little),
    }

    try compile.compile(cx.gpa, &diag, cx.vm, cx.op_map, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, node.statements, &out);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = lsc_type.blockId(),
        .data = out,
    } });
}

fn planEnterScript(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.items[node_index].enter;

    const diag: Diagnostic.ForTextFile = .{
        .diagnostic = cx.diagnostic,
        .path = room_file.path,
    };

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, &diag, cx.vm, cx.op_map, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, node.statements, &out);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .ENCD,
        .data = out,
    } });
}

fn planExitScript(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.items[node_index].exit;

    const diag: Diagnostic.ForTextFile = .{
        .diagnostic = cx.diagnostic,
        .path = room_file.path,
    };

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, &diag, cx.vm, cx.op_map, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, node.statements, &out);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .EXCD,
        .data = out,
    } });
}

fn planObject(cx: *const Context, room_number: u8, node_index: u32, event_index: u16) !void {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try planObjectInner(cx, room_number, node_index, &out);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .OBCD,
        .data = out,
    } });
}

fn planObjectInner(
    cx: *const Context,
    room_number: u8,
    node_index: u32,
    out: *std.ArrayListUnmanaged(u8),
) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const object = &room_file.ast.nodes.items[node_index].object;

    var verb_count: u32 = 0;
    for (room_file.ast.getExtra(object.children)) |child_index| {
        const child = &room_file.ast.nodes.items[child_index];
        if (child.* == .verb) verb_count += 1;
    }

    var verb_fixup: ?u32 = null;
    var cur_verb_index: u8 = 0;

    for (room_file.ast.getExtra(object.children)) |child_index| {
        const child = &room_file.ast.nodes.items[child_index];
        switch (child.*) {
            .raw_block => |n| {
                try encodeRawBlock(cx.gpa, out, n.block_id, cx.project_dir, n.path);
            },
            .verb => |verb| {
                if (verb_fixup == null) {
                    verb_fixup = try beginBlockAl(cx.gpa, out, .VERB);
                    _ = try out.addManyAsSlice(cx.gpa, verb_count * @sizeOf(VerbEntry));
                    try out.append(cx.gpa, 0);
                }

                const entries_bytes = out.items[verb_fixup.? + block_header_size ..][0 .. verb_count * @sizeOf(VerbEntry)];
                const entries = std.mem.bytesAsSlice(VerbEntry, entries_bytes);
                entries[cur_verb_index] = .{
                    .number = verb.number,
                    .offset = std.math.cast(u16, out.items.len - verb_fixup.?) orelse
                        return error.BadData,
                };

                switch (verb.body) {
                    .assembly => |path| {
                        const in = try fs.readFile(cx.gpa, cx.project_dir, path);
                        defer cx.gpa.free(in);

                        const id: Symbols.ScriptId = .{ .object = .{
                            .room = room_number,
                            .number = object.number,
                            .verb = verb.number,
                        } };
                        var result = try assemble.assemble(cx.gpa, cx.vm, in, &cx.project_scope, &cx.room_scopes[room_number], id);
                        defer result.deinit(cx.gpa);

                        try out.appendSlice(cx.gpa, result.items); // TODO: avoid this memcpy
                    },
                    .script => |statements| {
                        const diag: Diagnostic.ForTextFile = .{
                            .diagnostic = cx.diagnostic,
                            .path = room_file.path,
                        };
                        try compile.compile(cx.gpa, &diag, cx.vm, cx.op_map, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, statements, out);
                    },
                }

                cur_verb_index += 1;
            },
            else => unreachable,
        }
    }

    if (verb_fixup == null) {
        verb_fixup = try beginBlockAl(cx.gpa, out, .VERB);
        try out.append(cx.gpa, 0);
    }
    try endBlockAl(out, verb_fixup.?);

    const obna_fixup = try beginBlockAl(cx.gpa, out, .OBNA);
    try out.appendSlice(cx.gpa, object.obna);
    try out.append(cx.gpa, 0);
    try endBlockAl(out, obna_fixup);
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
        .block_id = .RNAM,
        .data = result,
    } });
}
