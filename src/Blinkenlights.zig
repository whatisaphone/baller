const builtin = @import("builtin");
const std = @import("std");

const sync = @import("sync.zig");

const Blinkenlights = @This();

const max_nodes = sync.max_concurrency + 3; // 3 additional rows for total, file, room
const max_lines = 10;

mutex: std.Thread.Mutex,
nodes: [max_nodes]Node,
terminate: std.atomic.Value(u32),
thread: std.Thread,

pub fn initAndStart(self: *Blinkenlights) !void {
    self.mutex = .{};
    self.nodes[0] = .{
        .parent = .null,
        .prev_sibling = .null,
        .next_sibling = .null,
        .first_child = .null,
        .last_child = .null,
        .text = .{0} ++ .{undefined} ** (Node.max_text_len - 1) ++ .{undefined},
        .progress_style = .initial,
        .max = null,
        .progress = 0,
    };
    for (self.nodes[1..]) |*node|
        node.* = .{
            .parent = .null,
            .prev_sibling = undefined,
            .next_sibling = undefined,
            .first_child = undefined,
            .last_child = undefined,
            .text = undefined,
            .progress_style = undefined,
            .max = undefined,
            .progress = undefined,
        };
    self.terminate = .init(0);
    self.thread = try std.Thread.spawn(.{}, threadEntry, .{self});
}

pub fn stop(self: *Blinkenlights) void {
    self.terminate.store(1, .monotonic);
    std.Thread.Futex.wake(&self.terminate, 1);
    self.thread.join();
}

pub fn addNode(self: *Blinkenlights, parent: NodeId) NodeId {
    self.mutex.lock();
    defer self.mutex.unlock();

    const index = for (self.nodes[1..], 1..) |node, i| {
        if (node.parent == .null) break i;
    } else unreachable;
    const id: NodeId = .fromIndex(@intCast(index));

    self.nodes[id.index()] = .{
        .parent = parent,
        .prev_sibling = .null,
        .next_sibling = .null,
        .first_child = .null,
        .last_child = .null,
        .text = .{0} ++ .{undefined} ** (Node.max_text_len - 1) ++ .{undefined},
        .progress_style = .initial,
        .max = null,
        .progress = 0,
    };
    if (self.nodes[parent.index()].first_child == .null) {
        self.nodes[parent.index()].first_child = id;
    } else {
        const old_last_child = self.nodes[parent.index()].last_child;
        std.debug.assert(self.nodes[old_last_child.index()].next_sibling == .null);
        self.nodes[old_last_child.index()].next_sibling = id;
        self.nodes[id.index()].prev_sibling = old_last_child;
    }
    self.nodes[parent.index()].last_child = id;

    return id;
}

pub fn removeNode(self: *Blinkenlights, id: NodeId) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    if (node.prev_sibling != .null)
        self.nodes[node.prev_sibling.index()].next_sibling = node.next_sibling;
    if (node.next_sibling != .null)
        self.nodes[node.next_sibling.index()].prev_sibling = node.prev_sibling;
    if (id == self.nodes[node.parent.index()].last_child)
        self.nodes[node.parent.index()].last_child = node.prev_sibling;
    if (id == self.nodes[node.parent.index()].first_child)
        self.nodes[node.parent.index()].first_child = node.next_sibling;
    node.* = undefined;
    node.parent = .null;
}

pub fn setText(self: *Blinkenlights, id: NodeId, text: []const u8) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    const len = @min(text.len, Node.max_text_len); // silently truncate text :(
    @memcpy(node.text[0..len], text[0..len]);
    node.text[len] = 0;
}

pub fn setTextPrint(
    self: *Blinkenlights,
    id: NodeId,
    comptime fmt: []const u8,
    args: anytype,
) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    var w: std.io.Writer = .fixed(&node.text);
    w.print(fmt, args) catch {}; // silently truncate text :(
    node.text[w.end] = 0;
}

pub fn setProgressStyle(self: *Blinkenlights, id: NodeId, style: ProgressStyle) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    node.progress_style = style;
}

pub fn setMax(self: *Blinkenlights, id: NodeId, max: u32) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    node.max = max;
}

pub fn addProgress(self: *Blinkenlights, id: NodeId, amount: u32) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    std.debug.assert(node.max != null);
    node.progress += amount;
}

pub fn debugAssertProgressFinished(self: *Blinkenlights, id: NodeId) void {
    if (builtin.mode != .Debug) return;

    self.mutex.lock();
    defer self.mutex.unlock();

    const node = &self.nodes[id.index()];
    std.testing.expectEqual(node.progress, node.max.?) catch unreachable;
}

const NodeIndex = u8;

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

const Node = struct {
    const max_text_len = 23;

    // tree relationships
    parent: NodeId,
    prev_sibling: NodeId,
    next_sibling: NodeId,
    first_child: NodeId,
    last_child: NodeId,
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
    var stderr_buf: [4096]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&stderr_buf);

    if (!setupTerminal()) return;

    while (true) {
        if (std.Thread.Futex.timedWait(&self.terminate, 0, 50 * std.time.ns_per_ms))
            break
        else |_| {}

        self.mutex.lock();
        defer self.mutex.unlock();

        self.renderTree(&stderr.interface) catch {};
    }

    renderClear(&stderr.interface) catch {};
}

fn setupTerminal() bool {
    const stderr = std.fs.File.stderr();
    if (!stderr.isTty()) return false;

    if (builtin.target.os.tag == .windows) {
        if (!stderr.getOrEnableAnsiEscapeSupport())
            return false;
    }
    return true;
}

const RenderState = struct {
    nodes: *const [max_nodes]Node,
    out: *std.io.Writer,
    lines: u8,
};

fn renderTree(self: *Blinkenlights, out: *std.io.Writer) !void {
    var rs: RenderState = .{
        .nodes = &self.nodes,
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

fn renderNode(rs: *RenderState, id: NodeId) !void {
    if (rs.lines == max_lines) return;

    const node = &rs.nodes[id.index()];
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
    const chars = if (builtin.target.os.tag == .windows) struct {
        // conhost.exe can't render emojis, so on windows use plain old ascii
        const empty = "-";
        const filled = "#";
        const filled_width = 1;
    } else struct {
        const empty = "∙";
        const filled = "⚾";
        const filled_width = 2;
    };

    const raw_cells = @as(u64, progress) * (width + 1) / max;
    const filled_cells = @min(raw_cells, width);
    const filled_count = filled_cells / chars.filled_width;
    const empty_count = width - filled_count * chars.filled_width;

    try out.writeByte('[');
    try out.splatBytesAll(chars.filled, filled_count);
    try out.splatBytesAll(chars.empty, empty_count);
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
