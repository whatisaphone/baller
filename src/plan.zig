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
const bmp = @import("bmp.zig");
const compile = @import("compile.zig");
const decompile = @import("decompile.zig");
const VerbEntry = @import("extract.zig").VerbEntry;
const expected_pals_size = @import("extract.zig").expected_pals_size;
const fs = @import("fs.zig");
const fsd = @import("fsd.zig");
const games = @import("games.zig");
const lang = @import("lang.zig");
const music = @import("music.zig");
const obim = @import("obim.zig");
const rmim_encode = @import("rmim_encode.zig");
const script = @import("script.zig");
const sounds = @import("sounds.zig");
const sync = @import("sync.zig");
const utils = @import("utils.zig");

pub const Payload = union(enum) {
    nop,
    project_end,
    target: games.Target,
    disk_start: u8,
    disk_end,
    room_start: u8,
    room_end,
    glob: struct {
        node_index: Ast.NodeIndex,
        block_id: BlockId,
        glob_number: u16,
        data: []u8,
    },
    glob_start: struct {
        node_index: Ast.NodeIndex,
        block_id: BlockId,
        glob_number: u16,
    },
    glob_end,
    raw_block: struct {
        block_id: BlockId,
        data: []u8,
    },
    index_start,
    index_end,
    index_maxs: []u8,
    index_block: @FieldType(Ast.Node, "index_block"),
    err,
};

pub fn run(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    project_dir: std.fs.Dir,
    project: *const Project,
    awiz_strategy: awiz.EncodingStrategy,
    output_dir: std.fs.Dir,
    index_name: []const u8,
    pool: *std.Thread.Pool,
    events: *sync.Channel(sync.OrderedEvent(Payload), 16),
) void {
    var cx: Context = undefined;

    // Bit messy, watch out
    cx.project_scope = .empty;
    defer cx.project_scope.deinit(gpa);
    var room_scopes: [256]std.StringHashMapUnmanaged(script.Symbol) = @splat(.empty);
    defer for (&room_scopes) |*s| s.deinit(gpa);

    (blk: {
        cx = .{
            .gpa = gpa,
            .diagnostic = diagnostic,
            .project_dir = project_dir,
            .project = project,
            .awiz_strategy = awiz_strategy,
            .output_dir = output_dir,
            .index_name = index_name,
            .target = .undef,
            .vm = .undef,
            .op_map = .undef,
            .project_scope = .empty,
            .room_scopes = &room_scopes,
            .room_lsc_types = @splat(.undef),
            .pool = pool,
            .events = events,
            .next_event_index = 0,
            .pending_jobs = .init(0),
        };
        cx.project_scope.ensureTotalCapacity(cx.gpa, 4096) catch |err| break :blk err;

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
    project_dir: std.fs.Dir,
    project: *const Project,
    awiz_strategy: awiz.EncodingStrategy,
    output_dir: std.fs.Dir,
    index_name: []const u8,
    target: utils.SafeUndefined(games.Target),
    vm: utils.SafeUndefined(lang.Vm),
    op_map: utils.SafeUndefined(std.EnumArray(lang.Op, decompile.Op)),
    project_scope: std.StringHashMapUnmanaged(script.Symbol),
    room_scopes: *[256]std.StringHashMapUnmanaged(script.Symbol),
    room_lsc_types: [256]utils.SafeUndefined(LocalScriptBlockType),
    pool: *std.Thread.Pool,
    events: *sync.Channel(sync.OrderedEvent(Payload), 16),
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
    const project_node = &project_file.ast.nodes.at(project_file.ast.root).project;

    try planTarget(cx);

    try buildProjectScope(cx);

    for (project_file.ast.getExtra(project_node.children)) |node_index| {
        const node = project_file.ast.nodes.at(node_index);
        if (node.* != .disk) continue;
        cx.sendSyncEvent(.{ .disk_start = node.disk.number });
        for (project_file.ast.getExtra(node.disk.children)) |room_node| {
            const room = &project_file.ast.nodes.at(room_node).disk_room;
            cx.sendSyncEvent(.{ .room_start = room.room_number });
            try planRoom(cx, room);
            cx.sendSyncEvent(.room_end);
        }
        cx.sendSyncEvent(.disk_end);
    }

    try planIndex(cx);

    try spawnJob(buildMusic, cx, undefined, undefined);

    cx.sendSyncEvent(.project_end);
}

fn planTarget(cx: *Context) !void {
    const project_file = &cx.project.files.items[0].?;
    const project_node = &project_file.ast.nodes.at(project_file.ast.root).project;
    const project_children = project_file.ast.getExtra(project_node.children);

    const target = target: {
        if (project_children.len == 0) break :target null;
        const target_node = project_file.ast.nodes.at(project_children[0]);
        if (target_node.* != .target) break :target null;
        break :target target_node.target;
    } orelse {
        cx.diagnostic.err("missing target", .{});
        return error.AddedToDiagnostic;
    };

    cx.sendSyncEvent(.{ .target = target });

    const game = target.pickAnyGame();
    cx.target.setOnce(target);
    cx.vm.setOnce(lang.buildVm(game));
    cx.op_map.setOnce(decompile.buildOpMap(game));
}

fn buildProjectScope(cx: *Context) !void {
    const project_file = &cx.project.files.items[0].?;
    const project_node = &project_file.ast.nodes.at(project_file.ast.root).project;

    const from_proj: AddSymbolArgs = .{
        .cx = cx,
        .scope = &cx.project_scope,
        .file = project_file,
    };

    for (project_file.ast.getExtra(project_node.children)) |node_index| {
        switch (project_file.ast.nodes.at(node_index).*) {
            .constant => |c| {
                try addScopeSymbol(&from_proj, node_index, c.name, .{ .constant = c.value });
            },
            .variable => |v| {
                const var_num = std.math.cast(u14, v.number) orelse return error.BadData;
                const variable: lang.Variable = .init(.global, var_num);
                try addScopeSymbol(&from_proj, node_index, v.name, .{ .variable = variable });
            },
            .disk => |disk| {
                for (project_file.ast.getExtra(disk.children)) |room_node| {
                    const disk_room = &project_file.ast.nodes.at(room_node).disk_room;
                    const num = disk_room.room_number;
                    try addScopeSymbol(&from_proj, room_node, disk_room.name, .{ .constant = num });

                    const room_file = &cx.project.files.items[disk_room.room_number].?;
                    const from_room: AddSymbolArgs = .{
                        .cx = cx,
                        .scope = &cx.project_scope,
                        .file = room_file,
                    };

                    const root = &room_file.ast.nodes.at(room_file.ast.root).room_file;
                    for (room_file.ast.getExtra(root.children)) |child_index| {
                        switch (room_file.ast.nodes.at(child_index).*) {
                            inline .raw_glob_file, .raw_glob_block => |n| if (n.name) |name| {
                                try addScopeSymbol(&from_room, child_index, name, .{ .constant = n.glob_number });
                            },
                            inline .scr, .script, .sound, .awiz, .mult, .akos, .talkie => |n| {
                                try addScopeSymbol(&from_room, child_index, n.name, .{ .constant = n.glob_number });
                            },
                            else => {},
                        }
                    }
                }
            },
            else => {},
        }
    }
}

const AddSymbolArgs = struct {
    cx: *const Context,
    scope: *std.StringHashMapUnmanaged(script.Symbol),
    file: *const Project.SourceFile,
};

fn addScopeSymbol(
    args: *const AddSymbolArgs,
    node_index: Ast.NodeIndex,
    name: Ast.StringSlice,
    symbol: script.Symbol,
) !void {
    const str = args.file.ast.strings.get(name);
    const entry = try args.scope.getOrPut(args.cx.gpa, str);
    if (entry.found_existing) {
        args.cx.diagnostic.errAt(.node(args.file, node_index), "duplicate name: {s}", .{str});
        return error.AddedToDiagnostic;
    }
    entry.value_ptr.* = symbol;
}

fn checkUniqueSymbolName(
    args: *const AddSymbolArgs,
    node_index: Ast.NodeIndex,
    name: Ast.StringSlice,
) !void {
    const str = args.file.ast.strings.get(name);
    if (args.cx.project_scope.contains(str)) {
        args.cx.diagnostic.errAt(.node(args.file, node_index), "duplicate name: {s}", .{str});
        return error.AddedToDiagnostic;
    }
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
    const room_file = &cx.project.files.items[room_number].?;
    const root = &room_file.ast.nodes.at(room_file.ast.root).room_file;

    const from_room: AddSymbolArgs = .{
        .cx = cx,
        .scope = &cx.room_scopes[room_number],
        .file = room_file,
    };

    for (room_file.ast.getExtra(root.variables)) |node_index| {
        const node = &room_file.ast.nodes.at(node_index).variable;
        const num = std.math.cast(u14, node.number) orelse return error.BadData;
        try checkUniqueSymbolName(&from_room, node_index, node.name);
        try addScopeSymbol(&from_room, node_index, node.name, .{ .variable = .init(.room, num) });
    }

    for (room_file.ast.getExtra(root.children)) |node_index| {
        switch (room_file.ast.nodes.at(node_index).*) {
            inline .lsc, .local_script => |n| {
                try checkUniqueSymbolName(&from_room, node_index, n.name);
                try addScopeSymbol(&from_room, node_index, n.name, .{ .constant = n.script_number });
            },
            else => {},
        }
    }
}

const RoomPlan = struct {
    work: std.EnumArray(RoomSection, std.ArrayList(RoomWork)),
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
    node_second_pass: Ast.NodeIndex,
};

fn scanRoom(cx: *Context, plan: *RoomPlan, room_number: u8) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const root = &room_file.ast.nodes.at(room_file.ast.root).room_file;
    for (room_file.ast.getExtra(root.children)) |child_node| {
        switch (room_file.ast.nodes.at(child_node).*) {
            inline .raw_glob_file, .raw_glob_block, .raw_block => |n| {
                const section = sectionForBlockId(n.block_id) orelse plan.cur_section;
                try plan.add(section, .{ .node = child_node });
            },
            .rmim => {
                try plan.add(.start, .{ .node = child_node });
                try plan.add(.rmda_obim, .{ .node_second_pass = child_node });
            },
            .rmda => try scanRmda(cx, plan, room_number, child_node),
            .scr => try plan.add(.end, .{ .node = child_node }),
            .excd => try plan.add(.rmda_excd, .{ .node = child_node }),
            .encd => try plan.add(.rmda_encd, .{ .node = child_node }),
            .lsc => try plan.add(.rmda_lsc, .{ .node = child_node }),
            .obim => try plan.add(.rmda_obim, .{ .node = child_node }),
            .sound, .awiz, .mult, .akos, .talkie => try plan.add(.end, .{ .node = child_node }),
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
            .data = &.{},
        } } });
    if (plan.work.getPtr(.rmda_encd).items.len == 0)
        plan.work.getPtr(.rmda_encd).appendAssumeCapacity(.{ .event = .{ .raw_block = .{
            .block_id = .ENCD,
            .data = &.{},
        } } });

    // Any rooms where every script number < 256, all local scripts are LSCR.
    // Otherwise all are LSC2.
    var max_lsc_number: u16 = 0;
    for (plan.work.getPtrConst(.rmda_lsc).items) |*work| {
        if (work.* == .node) {
            const number: ?u16 = switch (room_file.ast.nodes.at(work.node).*) {
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
    rmda_node_index: Ast.NodeIndex,
) !void {
    try plan.add(plan.cur_section, .{ .event = .{ .glob_start = .{
        .node_index = rmda_node_index,
        .block_id = .RMDA,
        .glob_number = room_number,
    } } });
    const room_file = &cx.project.files.items[room_number].?;
    const rmda = &room_file.ast.nodes.at(rmda_node_index).rmda;
    for (room_file.ast.getExtra(rmda.children)) |node_index| {
        const raw_block = &room_file.ast.nodes.at(node_index).raw_block;
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
                .node => |child_node| switch (room_file.ast.nodes.at(child_node).*) {
                    .raw_glob_file => try planRawGlobFile(cx, room_number, child_node),
                    .raw_glob_block => try planRawGlobBlock(cx, room_number, child_node),
                    .raw_block => try planRawBlock(cx, room_number, child_node),
                    .rmim => try spawnJob(planRmim, cx, room_number, child_node),
                    .scr => try spawnJob(planScr, cx, room_number, child_node),
                    .encd, .excd => try spawnJob(planEncdExcd, cx, room_number, child_node),
                    .lsc => try spawnJob(planLsc, cx, room_number, child_node),
                    .obim => try spawnJob(planObim, cx, room_number, child_node),
                    .sound => try spawnJob(planSound, cx, room_number, child_node),
                    .awiz => try spawnJob(planAwiz, cx, room_number, child_node),
                    .mult => try spawnJob(planMult, cx, room_number, child_node),
                    .akos => try spawnJob(planAkos, cx, room_number, child_node),
                    .talkie => try spawnJob(planTalkie, cx, room_number, child_node),
                    .script => try spawnJob(planScript, cx, room_number, child_node),
                    .local_script => try spawnJob(planLocalScript, cx, room_number, child_node),
                    .enter => try spawnJob(planEnterScript, cx, room_number, child_node),
                    .exit => try spawnJob(planExitScript, cx, room_number, child_node),
                    .object => try spawnJob(planObject, cx, room_number, child_node),
                    else => unreachable,
                },
                .node_second_pass => |child_node| switch (room_file.ast.nodes.at(child_node).*) {
                    .rmim => try spawnJob(planPalsFromRmim, cx, room_number, child_node),
                    else => unreachable,
                },
            }
        }
    }
}

fn planRawBlock(cx: *Context, room_number: u8, node_index: Ast.NodeIndex) !void {
    const file = &cx.project.files.items[room_number].?;
    const node = &file.ast.nodes.at(node_index).raw_block;

    const data = switch (node.contents) {
        .path => |ss| try fsd.readFile(
            cx.gpa,
            cx.diagnostic,
            .node(file, node_index),
            cx.project_dir,
            file.ast.strings.get(ss),
        ),
        .data => |ss| try cx.gpa.dupe(u8, file.ast.strings.get(ss)),
    };
    errdefer cx.gpa.free(data);

    cx.sendSyncEvent(.{ .raw_block = .{
        .block_id = node.block_id,
        .data = data,
    } });
}

fn planRawGlobFile(cx: *Context, room_number: u8, node_index: Ast.NodeIndex) !void {
    const file = &cx.project.files.items[room_number].?;
    const node = &file.ast.nodes.at(node_index).raw_glob_file;

    const data = try fsd.readFile(
        cx.gpa,
        cx.diagnostic,
        .node(file, node_index),
        cx.project_dir,
        file.ast.strings.get(node.path),
    );
    errdefer cx.gpa.free(data);

    cx.sendSyncEvent(.{ .glob = .{
        .node_index = node_index,
        .block_id = node.block_id,
        .glob_number = node.glob_number,
        .data = data,
    } });
}

fn planRawGlobBlock(cx: *Context, room_number: u8, node_index: Ast.NodeIndex) !void {
    const file = &cx.project.files.items[room_number].?;
    const glob = &file.ast.nodes.at(node_index).raw_glob_block;

    cx.sendSyncEvent(.{ .glob_start = .{
        .node_index = node_index,
        .block_id = glob.block_id,
        .glob_number = glob.glob_number,
    } });

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(glob.children)) |node| {
        try planRawBlock(cx, room_number, node);
    }

    cx.sendSyncEvent(.glob_end);
}

fn planRmda(cx: *Context, room_number: u8, node_index: Ast.NodeIndex) !void {
    const rmda = &cx.project.files.items[room_number].?.ast.nodes.items[node_index].rmda;

    cx.sendSyncEvent(.{ .glob_start = .{
        .block_id = .RMDA,
        .glob_number = room_number,
    } });

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(rmda.children)) |node| {
        const child = &room_file.ast.nodes.items[node];
        switch (child.*) {
            .raw_block => try planRawBlock(cx, room_number, node),
            .encd, .excd => try spawnJob(planEncdExcd, cx, room_number, node),
            .lsc => try spawnJob(planLsc, cx, room_number, node),
            else => unreachable,
        }
    }

    cx.sendSyncEvent(.glob_end);
}

const Job = fn (cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) anyerror!void;

fn spawnJob(job: Job, cx: *Context, room_number: u8, node_index: Ast.NodeIndex) !void {
    const event_index = cx.next_event_index;
    cx.next_event_index += 1;

    _ = cx.pending_jobs.fetchAdd(1, .monotonic);
    try cx.pool.spawn(runJob, .{ job, cx, room_number, node_index, event_index });
}

fn runJob(job: Job, cx: *Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) void {
    job(cx, room_number, node_index, event_index) catch |err| {
        if (err != error.AddedToDiagnostic)
            cx.diagnostic.zigErr("unexpected error: {s}", .{}, err);
        cx.sendEvent(event_index, .err);
    };

    const prev_pending = cx.pending_jobs.fetchSub(1, .monotonic);
    if (prev_pending == 1)
        std.Thread.Futex.wake(&cx.pending_jobs, 1);
}

fn planRmim(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const rmim = &file.ast.nodes.at(node_index).rmim;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    const rmih = &file.ast.nodes.at(rmim.rmih).raw_block;
    try encodeRawBlock(cx.gpa, cx.project_dir, file, rmih, &out);

    const im00_start = try beginBlockAl(cx.gpa, &out, .IM00);

    const im = &file.ast.nodes.at(rmim.im).rmim_im;
    for (file.ast.getExtra(im.children)) |child_index| {
        switch (file.ast.nodes.at(child_index).*) {
            .raw_block => |*n| {
                try encodeRawBlock(cx.gpa, cx.project_dir, file, n, &out);
            },
            .bmap => |*n| {
                const loc: Diagnostic.Location = .node(file, child_index);
                const bmp_raw = try fsd.readFile(
                    cx.gpa,
                    cx.diagnostic,
                    loc,
                    cx.project_dir,
                    file.ast.strings.get(n.path),
                );
                defer cx.gpa.free(bmp_raw);
                try rmim_encode.encode(cx.gpa, cx.diagnostic, loc, cx.target.defined, n.compression, bmp_raw, &out);
            },
            else => unreachable,
        }
    }

    endBlockAl(&out, im00_start);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .RMIM,
        .glob_number = room_number,
        .data = data,
    } });
}

fn planPalsFromRmim(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const rmim = &file.ast.nodes.at(node_index).rmim;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);
    try out.ensureTotalCapacityPrecise(cx.gpa, expected_pals_size);

    const im = &file.ast.nodes.at(rmim.im).rmim_im;
    const im_children = file.ast.getExtra(im.children);
    if (im_children.len == 0) return error.BadData;
    const bmap_node_index = im_children[0];
    const bmap = file.ast.nodes.at(bmap_node_index);
    if (bmap.* != .bmap) return error.BadData;

    const bmp_path = file.ast.strings.get(bmap.bmap.path);
    // XXX: this is not quite ideal since it reads the bmp from disk a second time
    const bmp_raw = try fsd.readFile(
        cx.gpa,
        cx.diagnostic,
        .node(file, bmap_node_index),
        cx.project_dir,
        bmp_path,
    );
    defer cx.gpa.free(bmp_raw);
    var bmp_err: bmp.HeaderError = undefined;
    const bmp_header = bmp.readHeader(bmp_raw, &bmp_err) catch
        return bmp_err.addToDiag(cx.diagnostic, .node(file, bmap_node_index));
    const bmp8 = bmp_header.as8Bit(&bmp_err) catch
        return bmp_err.addToDiag(cx.diagnostic, .node(file, bmap_node_index));
    try buildPals(&bmp8, &out);

    errdefer comptime unreachable;
    std.debug.assert(out.items.len == out.capacity);
    const data = out.items;

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .PALS,
        .data = data,
    } });
}

fn buildPals(bitmap: *const bmp.Bmp8, out: *std.ArrayList(u8)) !void {
    const na = utils.null_allocator;

    const wrap_start = try beginBlockAl(na, out, .WRAP);

    const offs_start = try beginBlockAl(na, out, .OFFS);
    try utils.writeInt(na, out, u32, 12, .little);
    endBlockAl(out, offs_start);

    const apal_start = try beginBlockAl(na, out, .APAL);
    try awiz.writeRgbs(na, bitmap.*, out);
    endBlockAl(out, apal_start);

    endBlockAl(out, wrap_start);
}

fn planScr(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const scr = &file.ast.nodes.at(node_index).scr;

    const path = file.ast.strings.get(scr.path);
    const in = try fsd.readFile(
        cx.gpa,
        cx.diagnostic,
        .node(file, node_index),
        cx.project_dir,
        path,
    );
    defer cx.gpa.free(in);

    var result = try assemble.assemble(
        cx.gpa,
        cx.diagnostic,
        path,
        &cx.vm.defined,
        in,
        &cx.project_scope,
        &cx.room_scopes[room_number],
    );
    errdefer result.deinit(cx.gpa);

    const data = try result.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .SCRP,
        .glob_number = scr.glob_number,
        .data = data,
    } });
}

fn planEncdExcd(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const node = file.ast.nodes.at(node_index);
    const edge: enum { encd, excd }, const path_slice = switch (node.*) {
        .encd => |*n| .{ .encd, n.path },
        .excd => |*n| .{ .excd, n.path },
        else => unreachable,
    };

    const path = file.ast.strings.get(path_slice);
    const in = try fsd.readFile(
        cx.gpa,
        cx.diagnostic,
        .node(file, node_index),
        cx.project_dir,
        path,
    );
    defer cx.gpa.free(in);

    var result = try assemble.assemble(
        cx.gpa,
        cx.diagnostic,
        path,
        &cx.vm.defined,
        in,
        &cx.project_scope,
        &cx.room_scopes[room_number],
    );
    errdefer result.deinit(cx.gpa);

    const block_id: BlockId = switch (edge) {
        .encd => .ENCD,
        .excd => .EXCD,
    };
    const data = try result.toOwnedSlice(cx.gpa);
    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = block_id,
        .data = data,
    } });
}

fn planLsc(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const lsc = &file.ast.nodes.at(node_index).lsc;

    const path = file.ast.strings.get(lsc.path);
    const in = try fsd.readFile(
        cx.gpa,
        cx.diagnostic,
        .node(file, node_index),
        cx.project_dir,
        path,
    );
    defer cx.gpa.free(in);

    var bytecode = try assemble.assemble(
        cx.gpa,
        cx.diagnostic,
        path,
        &cx.vm.defined,
        in,
        &cx.project_scope,
        &cx.room_scopes[room_number],
    );
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
        .data = result,
    } });
}

fn planObim(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compileObim(cx, room_number, node_index, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .OBIM,
        .data = data,
    } });
}

fn compileObim(
    cx: *const Context,
    room_number: u8,
    node_index: Ast.NodeIndex,
    out: *std.ArrayList(u8),
) !void {
    const file = &cx.project.files.items[room_number].?;
    const obim_node = &file.ast.nodes.at(node_index).obim;

    var im_index: usize = 0;

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(obim_node.children)) |child_index| {
        switch (room_file.ast.nodes.at(child_index).*) {
            .raw_block => |*n| {
                try encodeRawBlock(cx.gpa, cx.project_dir, file, n, out);
            },
            .obim_im => {
                const block_id = obim.makeImBlockId(im_index) orelse return error.BadData;
                const im_start = try beginBlockAl(cx.gpa, out, block_id);
                try compileIm(cx, room_number, child_index, out);
                endBlockAl(out, im_start);
                im_index += 1;
            },
            else => unreachable,
        }
    }
}

fn compileIm(
    cx: *const Context,
    room_number: u8,
    node_index: Ast.NodeIndex,
    out: *std.ArrayList(u8),
) !void {
    const file = &cx.project.files.items[room_number].?;
    const im_node = &file.ast.nodes.at(node_index).obim_im;

    const room_file = &cx.project.files.items[room_number].?;
    for (room_file.ast.getExtra(im_node.children)) |child_index| {
        switch (room_file.ast.nodes.at(child_index).*) {
            .raw_block => |*n| {
                try encodeRawBlock(cx.gpa, cx.project_dir, file, n, out);
            },
            .smap => |*n| {
                const path = file.ast.strings.get(n.path);
                const bmp_raw = try fs.readFile(cx.gpa, cx.project_dir, path);
                defer cx.gpa.free(bmp_raw);

                const smap_start = try beginBlockAl(cx.gpa, out, .SMAP);
                const strip_compression = room_file.ast.getExtraU32(n.strip_compression);
                try obim.encodeSmap(cx.gpa, cx.diagnostic, cx.target.defined, .node(file, child_index), bmp_raw, strip_compression, out);
                endBlockAl(out, smap_start);
            },
            else => unreachable,
        }
    }
}

pub fn encodeRawBlock(
    gpa: std.mem.Allocator,
    dir: std.fs.Dir,
    file: *const Project.SourceFile,
    node: *const @FieldType(Ast.Node, "raw_block"),
    out: *std.ArrayList(u8),
) !void {
    const start = try beginBlockAl(gpa, out, node.block_id);
    switch (node.contents) {
        .path => |ss| {
            var writer: std.io.Writer.Allocating = .fromArrayList(gpa, out);
            defer out.* = writer.toArrayList();
            // TODO: use `fsd` for better errors
            try fs.readFileInto(dir, file.ast.strings.get(ss), &writer.writer);
        },
        .data => |ss| try out.appendSlice(gpa, file.ast.strings.get(ss)),
    }
    endBlockAl(out, start);
}

fn planSound(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const node = &file.ast.nodes.at(node_index).sound;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try sounds.build(cx.gpa, cx.diagnostic, .node(file, node_index), cx.project_dir, file, node.children, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = node.block_id,
        .glob_number = node.glob_number,
        .data = data,
    } });
}

fn planAwiz(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const awiz_node = &file.ast.nodes.at(node_index).awiz;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try awiz.encode(cx.gpa, cx.diagnostic, .node(file, node_index), cx.project_dir, file, awiz_node.children, cx.awiz_strategy, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .AWIZ,
        .glob_number = awiz_node.glob_number,
        .data = data,
    } });
}

fn planMult(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const mult = &file.ast.nodes.at(node_index).mult;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try planMultInner(cx, room_number, mult, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .MULT,
        .glob_number = mult.glob_number,
        .data = data,
    } });
}

fn planMultInner(
    cx: *const Context,
    room_number: u8,
    mult_node: *const @FieldType(Ast.Node, "mult"),
    out: *std.ArrayList(u8),
) !void {
    const room_file = &cx.project.files.items[room_number].?;

    if (mult_node.raw_block.unwrap()) |index| {
        const defa = &room_file.ast.nodes.at(index).raw_block_nested;
        const defa_start = try beginBlockAl(cx.gpa, out, .DEFA);
        for (room_file.ast.getExtra(defa.children)) |child_node| {
            const child = &room_file.ast.nodes.at(child_node).raw_block;
            try encodeRawBlock(cx.gpa, cx.project_dir, room_file, child, out);
        }
        endBlockAl(out, defa_start);
    }

    const wrap_start = try beginBlockAl(cx.gpa, out, .WRAP);

    const offs_start = try beginBlockAl(cx.gpa, out, .OFFS);
    // the offsets are filled in at the end
    _ = try out.addManyAsSlice(cx.gpa, mult_node.indices.len * 4);
    endBlockAl(out, offs_start);

    var awiz_offsets: utils.TinyArray(u32, Ast.max_mult_children) = .empty;
    for (room_file.ast.getExtra(mult_node.children)) |node| {
        awiz_offsets.appendAssumeCapacity(@as(u32, @intCast(out.items.len)) - offs_start);
        const wiz = &room_file.ast.nodes.at(node).mult_awiz;
        const awiz_start = try beginBlockAl(cx.gpa, out, .AWIZ);
        try awiz.encode(cx.gpa, cx.diagnostic, .node(room_file, node), cx.project_dir, room_file, wiz.children, cx.awiz_strategy, out);
        endBlockAl(out, awiz_start);
    }

    var off_pos = offs_start + block_header_size;
    for (room_file.ast.getExtraU32(mult_node.indices)) |i| {
        std.mem.writeInt(u32, out.items[off_pos..][0..4], awiz_offsets.get(i), .little);
        off_pos += 4;
    }

    endBlockAl(out, wrap_start);
}

fn planAkos(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const node = &file.ast.nodes.at(node_index).akos;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try akos.encode(cx.gpa, cx.diagnostic, cx.project, cx.project_dir, cx.awiz_strategy, room_number, node_index, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .AKOS,
        .glob_number = node.glob_number,
        .data = data,
    } });
}

fn planTalkie(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const file = &cx.project.files.items[room_number].?;
    const node = &file.ast.nodes.at(node_index).talkie;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);
    try out.ensureTotalCapacityPrecise(cx.gpa, 8 + node.text.len + 1);

    const text_start = try beginBlockAl(cx.gpa, &out, .TEXT);
    out.appendSliceAssumeCapacity(file.ast.strings.get(node.text));
    out.appendAssumeCapacity(0);
    endBlockAl(&out, text_start);

    errdefer comptime unreachable;
    std.debug.assert(out.items.len == out.capacity);
    const data = out.items;

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .TLKE,
        .glob_number = node.glob_number,
        .data = data,
    } });
}

fn planScript(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.at(node_index).script;

    const diag: Diagnostic.ForTextFile = .init(cx.diagnostic, room_file.path);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, &diag, &cx.vm.defined, &cx.op_map.defined, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, node.params, node.statements, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .glob = .{
        .node_index = node_index,
        .block_id = .SCRP,
        .glob_number = node.glob_number,
        .data = data,
    } });
}

fn planLocalScript(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.at(node_index).local_script;

    const diag: Diagnostic.ForTextFile = .init(cx.diagnostic, room_file.path);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    const lsc_type = cx.room_lsc_types[room_number].defined;
    switch (lsc_type) {
        .lscr => try out.append(cx.gpa, @intCast(node.script_number)),
        .lsc2 => try utils.writeInt(cx.gpa, &out, u32, node.script_number, .little),
    }

    try compile.compile(cx.gpa, &diag, &cx.vm.defined, &cx.op_map.defined, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, node.params, node.statements, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = lsc_type.blockId(),
        .data = data,
    } });
}

fn planEnterScript(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.at(node_index).enter;

    const diag: Diagnostic.ForTextFile = .init(cx.diagnostic, room_file.path);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, &diag, &cx.vm.defined, &cx.op_map.defined, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, .empty, node.statements, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .ENCD,
        .data = data,
    } });
}

fn planExitScript(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const node = &room_file.ast.nodes.at(node_index).exit;

    const diag: Diagnostic.ForTextFile = .init(cx.diagnostic, room_file.path);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try compile.compile(cx.gpa, &diag, &cx.vm.defined, &cx.op_map.defined, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, .empty, node.statements, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .EXCD,
        .data = data,
    } });
}

fn planObject(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(cx.gpa);

    try planObjectInner(cx, room_number, node_index, &out);

    const data = try out.toOwnedSlice(cx.gpa);

    cx.sendEvent(event_index, .{ .raw_block = .{
        .block_id = .OBCD,
        .data = data,
    } });
}

fn planObjectInner(
    cx: *const Context,
    room_number: u8,
    node_index: Ast.NodeIndex,
    out: *std.ArrayList(u8),
) !void {
    const room_file = &cx.project.files.items[room_number].?;
    const object = &room_file.ast.nodes.at(node_index).object;

    var verb_count: u32 = 0;
    for (room_file.ast.getExtra(object.children)) |child_index| {
        const child = room_file.ast.nodes.at(child_index);
        if (child.* == .verb) verb_count += 1;
    }

    var verb_fixup: ?u32 = null;
    var cur_verb_index: u8 = 0;

    for (room_file.ast.getExtra(object.children)) |child_index| {
        const child = room_file.ast.nodes.at(child_index);
        switch (child.*) {
            .raw_block => |*n| {
                try encodeRawBlock(cx.gpa, cx.project_dir, room_file, n, out);
            },
            .verb => |*verb| {
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
                    .assembly => |path_slice| {
                        const path = room_file.ast.strings.get(path_slice);
                        const in = try fsd.readFile(
                            cx.gpa,
                            cx.diagnostic,
                            .node(room_file, child_index),
                            cx.project_dir,
                            path,
                        );
                        defer cx.gpa.free(in);

                        var result = try assemble.assemble(
                            cx.gpa,
                            cx.diagnostic,
                            path,
                            &cx.vm.defined,
                            in,
                            &cx.project_scope,
                            &cx.room_scopes[room_number],
                        );
                        defer result.deinit(cx.gpa);

                        try out.appendSlice(cx.gpa, result.items); // TODO: avoid this memcpy
                    },
                    .script => |statements| {
                        const diag: Diagnostic.ForTextFile = .{
                            .diagnostic = cx.diagnostic,
                            .path = room_file.path,
                        };
                        try compile.compile(cx.gpa, &diag, &cx.vm.defined, &cx.op_map.defined, &cx.project_scope, &cx.room_scopes[room_number], room_file, node_index, .empty, statements, out);
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
    endBlockAl(out, verb_fixup.?);

    const obna_fixup = try beginBlockAl(cx.gpa, out, .OBNA);
    try out.appendSlice(cx.gpa, room_file.ast.strings.get(object.obna));
    try out.append(cx.gpa, 0);
    endBlockAl(out, obna_fixup);
}

fn planIndex(cx: *Context) !void {
    cx.sendSyncEvent(.index_start);

    const project_file = &cx.project.files.items[0].?;
    const project_root = &project_file.ast.nodes.at(project_file.ast.root).project;

    const index_node = for (project_file.ast.getExtra(project_root.children)) |node_index| {
        const node = project_file.ast.nodes.at(node_index);
        if (node.* == .index) break node_index;
    } else unreachable;
    const index = &project_file.ast.nodes.at(index_node).index;

    for (project_file.ast.getExtra(index.children)) |node| {
        switch (project_file.ast.nodes.at(node).*) {
            .raw_block => try planRawBlock(cx, 0, node),
            .maxs => try planMaxs(cx, node),
            .index_block => |b| switch (b) {
                .RNAM => try planRoomNames(cx),
                else => cx.sendSyncEvent(.{ .index_block = b }),
            },
            else => unreachable,
        }
    }

    cx.sendSyncEvent(.index_end);
}

fn planMaxs(cx: *Context, node_index: Ast.NodeIndex) !void {
    const project_file = &cx.project.files.items[0].?;
    const node = &project_file.ast.nodes.at(node_index).maxs;

    const path = project_file.ast.strings.get(node.path);
    const data = try fsd.readFile(
        cx.gpa,
        cx.diagnostic,
        .node(project_file, node_index),
        cx.project_dir,
        path,
    );
    errdefer cx.gpa.free(data);

    cx.sendSyncEvent(.{ .index_maxs = data });
}

fn planRoomNames(cx: *Context) !void {
    var result: std.ArrayList(u8) = .empty;
    errdefer result.deinit(cx.gpa);
    try result.ensureTotalCapacity(cx.gpa, 256);

    // Collect rooms by number
    var room_nodes: utils.TinyArray(Ast.NodeIndex.Optional, 256) = .empty;
    const project_file = &cx.project.files.items[0].?;
    const project_root = &project_file.ast.nodes.at(project_file.ast.root).project;
    for (project_file.ast.getExtra(project_root.children)) |node_index| {
        const node = project_file.ast.nodes.at(node_index);
        if (node.* != .disk) continue;
        for (project_file.ast.getExtra(node.disk.children)) |child_node| {
            const child = project_file.ast.nodes.at(child_node);
            if (child.* != .disk_room) continue;
            const room = &child.disk_room;
            room_nodes.grow(room.room_number + 1, Ast.NodeIndex.Optional.null);
            room_nodes.set(room.room_number, child_node.wrap());
        }
    }

    // Write room names in order
    for (room_nodes.slice()) |room_node_opt| {
        const room_node = room_node_opt.unwrap() orelse continue;
        const room = &project_file.ast.nodes.at(room_node).disk_room;
        try result.appendSlice(cx.gpa, std.mem.asBytes(&@as(u16, room.room_number)));
        try result.appendSlice(cx.gpa, project_file.ast.strings.get(room.name));
        try result.append(cx.gpa, 0);
    }
    try result.appendSlice(cx.gpa, &.{ 0, 0 });

    const data = try result.toOwnedSlice(cx.gpa);

    cx.sendSyncEvent(.{ .raw_block = .{
        .block_id = .RNAM,
        .data = data,
    } });
}

fn buildMusic(cx: *const Context, room_number: u8, node_index: Ast.NodeIndex, event_index: u16) !void {
    // HACK: function signature is all wrong, if I fixed it I wouldn't need nop
    // nodes at all
    _ = room_number;
    _ = node_index;

    var path_buf: [games.longest_index_name_len + 1]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "{s}", .{cx.index_name}) catch unreachable;
    games.pointPathToMusic(path);

    try music.build(cx.gpa, cx.diagnostic, cx.project_dir, cx.project, cx.output_dir, path);

    cx.sendEvent(event_index, .nop);
}
