const builtin = @import("builtin");
const std = @import("std");

const keyed = @import("keyed.zig");
const sync = @import("sync.zig");

const Blinkenlights = @This();

const max_nodes = 31; // TODO: handle too much parallelism
const max_lines = 10;

queue: sync.Channel(Message, 16),
next_node_id: std.atomic.Value(NodeIndex),
thread: std.Thread,

pub fn initAndStart(self: *Blinkenlights) !void {
    self.queue = .init;
    self.next_node_id = .init(1); // 0 is `.root`, and is allocated automatically
    self.thread = try std.Thread.spawn(.{}, threadEntry, .{self});
}

pub fn stop(self: *Blinkenlights) void {
    self.queue.send(.terminate);
    self.thread.join();
}

pub fn addNode(self: *Blinkenlights, parent: NodeId) NodeId {
    const prev_index = self.next_node_id.fetchAdd(1, .monotonic);
    const index = prev_index + 1;
    const id: NodeId = .fromIndex(index);
    self.queue.send(.{ .add = .{ .id = id, .parent = parent } });
    return id;
}

pub fn removeNode(self: *Blinkenlights, id: NodeId) void {
    self.queue.send(.{ .remove = .{ .id = id } });
}

pub fn setText(self: *Blinkenlights, id: NodeId, text: []const u8) void {
    var buf: [Node.max_text_len:0]u8 = undefined;
    const len = @min(text.len, Node.max_text_len); // silently truncate text :(
    @memcpy(buf[0..len], text[0..len]);
    buf[len] = 0;
    self.queue.send(.{ .set_text = .{ .id = id, .text = buf } });
}

pub fn setTextPrint(
    self: *Blinkenlights,
    id: NodeId,
    comptime fmt: []const u8,
    args: anytype,
) void {
    var buf: [Node.max_text_len:0]u8 = undefined;
    var w: std.io.Writer = .fixed(&buf);
    w.print(fmt, args) catch {}; // silently truncate text :(
    buf[w.end] = 0;
    self.queue.send(.{ .set_text = .{ .id = id, .text = buf } });
}

pub fn setProgressStyle(self: *Blinkenlights, id: NodeId, style: ProgressStyle) void {
    self.queue.send(.{ .set_progress_style = .{ .id = id, .style = style } });
}

pub fn setMax(self: *Blinkenlights, id: NodeId, max: u32) void {
    self.queue.send(.{ .set_max = .{ .id = id, .max = max } });
}

pub fn addProgress(self: *Blinkenlights, id: NodeId, amount: u32) void {
    self.queue.send(.{ .add_progress = .{ .id = id, .amount = amount } });
}

pub fn debugAssertProgressFinished(self: *Blinkenlights, id: NodeId) void {
    if (builtin.mode != .Debug) return;

    var start_barrier: std.atomic.Value(u32) = .init(0);
    var state: *State = undefined;
    var end_barrier: *std.atomic.Value(u32) = undefined;
    // Tell the render thread to pause and temporarily send us the state so we
    // can examine it.
    self.queue.send(.{ .debug = .{
        .start_barrier = &start_barrier,
        .state = &state,
        .end_barrier = &end_barrier,
    } });
    // Wait for the render thread to populate the state.
    futexWaitNotEqual(&start_barrier, 0, .acquire);

    const slot = findSlotById(state, id);
    const node = &state.nodes[slot.index()];
    std.testing.expectEqual(node.progress, node.max.?) catch unreachable;

    // Signal the render thread to continue.
    end_barrier.store(1, .monotonic);
    std.Thread.Futex.wake(end_barrier, 1);
}

fn futexWaitNotEqual(
    ptr: *std.atomic.Value(u32),
    not_equal_to: u32,
    comptime order: std.builtin.AtomicOrder,
) void {
    while (true) {
        const value = ptr.load(order);
        if (value != not_equal_to) break;
        std.Thread.Futex.wait(ptr, value);
    }
}

const Message = union(enum) {
    add: struct { id: NodeId, parent: NodeId },
    remove: struct { id: NodeId },
    set_text: struct { id: NodeId, text: [Node.max_text_len:0]u8 },
    set_progress_style: struct { id: NodeId, style: ProgressStyle },
    set_max: struct { id: NodeId, max: u32 },
    add_progress: struct { id: NodeId, amount: u32 },
    terminate,
    debug: if (builtin.mode == .Debug)
        struct {
            start_barrier: *std.atomic.Value(u32),
            state: **State,
            end_barrier: **std.atomic.Value(u32),
        }
    else
        noreturn,
};

const State = struct {
    /// Stores the external `NodeId` associated with each storage slot
    ids: [max_nodes]NodeId,
    /// Densely indexed storage slots
    nodes: [max_nodes]Node,
};

const NodeIndex = u32;

pub const NodeId = enum(NodeIndex) {
    root = 0,
    null = std.math.maxInt(NodeIndex),
    _,

    fn fromIndex(idx: NodeIndex) NodeId {
        const result: NodeId = @enumFromInt(idx);
        std.debug.assert(result != .null);
        return result;
    }

    fn index(self: NodeId) NodeIndex {
        std.debug.assert(self != .null);
        return @intFromEnum(self);
    }
};

const Slot = keyed.Key(enum(u8) {}).Optional;

const Node = struct {
    const max_text_len = 23;

    // tree relationships
    parent: Slot,
    prev_sibling: Slot,
    next_sibling: Slot,
    first_child: Slot,
    last_child: Slot,
    // node data
    text: [max_text_len:0]u8,
    progress_style: ProgressStyle,
    max: ?u32,
    progress: u32,
};

const ProgressStyle = enum {
    bar,
    bar_bytes,

    const initial: ProgressStyle = .bar;
};

fn threadEntry(self: *Blinkenlights) void {
    var state: State = .{
        .ids = @splat(.null),
        .nodes = undefined,
    };
    state.ids[0] = .root;
    state.nodes[0] = .{
        .parent = .null,
        .prev_sibling = .null,
        .next_sibling = .null,
        .first_child = .null,
        .last_child = .null,
        .text = .{0} ++ .{undefined} ** (Node.max_text_len - 1),
        .progress_style = .initial,
        .max = null,
        .progress = 0,
    };

    var stderr_buf: [4096]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&stderr_buf);

    const display = setupTerminal();

    outer: while (true) {
        const start = std.time.Instant.now() catch unreachable;

        while (true) {
            const message = self.queue.receive();
            if (message == .terminate) break :outer;
            handleMessage(&state, &message);

            const now = std.time.Instant.now() catch unreachable;
            if (now.since(start) >= 50 * std.time.ns_per_ms)
                break;
        }

        if (display)
            renderTree(&state, &stderr.interface) catch {};
    }
    if (display)
        renderClear(&stderr.interface) catch {};
}

fn setupTerminal() bool {
    const stderr = std.fs.File.stderr();
    if (!stderr.isTty()) return false;

    if (builtin.target.os.tag == .windows) {
        const CP_UTF8 = 65001;
        if (std.os.windows.kernel32.SetConsoleOutputCP(CP_UTF8) == 0)
            return false;
        if (!stderr.getOrEnableAnsiEscapeSupport())
            return false;
    }
    return true;
}

fn handleMessage(state: *State, message: *const Message) void {
    switch (message.*) {
        .add => |*msg| {
            const slot = findSlotById(state, .null);
            state.ids[slot.index()] = msg.id;
            const parent_slot = findSlotById(state, msg.parent);
            state.nodes[slot.index()] = .{
                .parent = parent_slot,
                .prev_sibling = .null,
                .next_sibling = .null,
                .first_child = .null,
                .last_child = .null,
                .text = .{0} ++ .{undefined} ** (Node.max_text_len - 1),
                .progress_style = .initial,
                .max = null,
                .progress = 0,
            };
            if (state.nodes[parent_slot.index()].first_child == .null) {
                state.nodes[parent_slot.index()].first_child = slot;
            } else {
                const old_last_child = state.nodes[parent_slot.index()].last_child;
                std.debug.assert(state.nodes[old_last_child.index()].next_sibling == .null);
                state.nodes[old_last_child.index()].next_sibling = slot;
                state.nodes[slot.index()].prev_sibling = old_last_child;
            }
            state.nodes[parent_slot.index()].last_child = slot;
        },
        .remove => |*msg| {
            const slot = findSlotById(state, msg.id);
            state.ids[slot.index()] = .null;
            const node = &state.nodes[slot.index()];
            if (node.prev_sibling != .null)
                state.nodes[node.prev_sibling.index()].next_sibling = node.next_sibling;
            if (node.next_sibling != .null)
                state.nodes[node.next_sibling.index()].prev_sibling = node.prev_sibling;
            if (slot == state.nodes[node.parent.index()].last_child)
                state.nodes[node.parent.index()].last_child = node.prev_sibling;
            if (slot == state.nodes[node.parent.index()].first_child)
                state.nodes[node.parent.index()].first_child = node.next_sibling;
            node.* = undefined;
        },
        .set_text => |*msg| {
            const slot = findSlotById(state, msg.id);
            state.nodes[slot.index()].text = msg.text;
        },
        .set_progress_style => |*msg| {
            const slot = findSlotById(state, msg.id);
            state.nodes[slot.index()].progress_style = msg.style;
        },
        .set_max => |*msg| {
            const slot = findSlotById(state, msg.id);
            state.nodes[slot.index()].max = msg.max;
        },
        .add_progress => |*msg| {
            const slot = findSlotById(state, msg.id);
            std.debug.assert(state.nodes[slot.index()].max != null);
            state.nodes[slot.index()].progress += msg.amount;
        },
        .terminate => unreachable, // handled by caller
        .debug => |*msg| {
            var end_barrier: std.atomic.Value(u32) = .init(0);
            msg.state.* = state;
            msg.end_barrier.* = &end_barrier;

            // Signal to the caller that the state was populated
            msg.start_barrier.store(1, .release);
            std.Thread.Futex.wake(msg.start_barrier, 1);

            // Wait for the caller to be finished with the state
            futexWaitNotEqual(&end_barrier, 0, .monotonic);
        },
    }
}

fn findSlotById(state: *const State, needle: NodeId) Slot {
    for (&state.ids, 0..) |id, i|
        if (id == needle)
            return .fromIndex(@intCast(i));
    unreachable;
}

const RenderState = struct {
    nodes: *const [max_nodes]Node,
    out: *std.io.Writer,
    lines: u8,
};

fn renderTree(state: *const State, out: *std.io.Writer) !void {
    var rs: RenderState = .{
        .nodes = &state.nodes,
        .out = out,
        .lines = 0,
    };
    try out.writeAll(ansi.sync_begin ++ ansi.erase_in_display_to_end);
    try renderNode(&rs, .fromIndex(0));
    try out.print(ansi.cursor_up_fmt ++ ansi.sync_end, .{rs.lines});
    try out.flush();
}

fn renderClear(out: *std.io.Writer) !void {
    try out.writeAll(ansi.erase_in_display_to_end);
    try out.flush();
}

fn renderNode(rs: *RenderState, slot: Slot) !void {
    if (rs.lines == max_lines) return;

    const node = &rs.nodes[slot.index()];
    if (node.max) |max|
        try renderProgressBar(rs.out, node.progress_style, node.progress, max);
    try rs.out.writeAll(std.mem.sliceTo(&node.text, 0));
    try rs.out.writeByte('\n');
    rs.lines += 1;

    if (node.first_child != .null)
        try renderNode(rs, node.first_child);
    if (node.next_sibling != .null)
        try renderNode(rs, node.next_sibling);
}

fn renderProgressBar(out: *std.io.Writer, style: ProgressStyle, progress: u32, max: u32) !void {
    const width = 32;
    const empty_char = "∙";
    const filled_char = "⚾";
    const filled_char_width = 2;

    const raw_cells = @as(u64, progress) * (width + 1) / max;
    const filled_cells = @min(raw_cells, width);
    const filled_count = filled_cells / filled_char_width;
    const empty_count = width - filled_count * filled_char_width;

    try out.writeByte('[');
    try out.splatBytesAll(filled_char, filled_count);
    try out.splatBytesAll(empty_char, empty_count);
    try out.writeAll("] ");
    if (style == .bar_bytes)
        try out.print("{:5.1} MB / {:5.1} MB ", .{
            @as(f32, @floatFromInt(progress)) / 1024 / 1024,
            @as(f32, @floatFromInt(max)) / 1024 / 1024,
        });
}

const ansi = struct {
    const csi = "\x1b[";

    const cursor_up_fmt = csi ++ "{}A";
    const erase_in_display_to_end = csi ++ "J";

    const sync_begin = csi ++ "?2026h";
    const sync_end = csi ++ "?2026l";
};
