const std = @import("std");

const ArrayMap = @import("array_map.zig").ArrayMap;
const utils = @import("utils.zig");

const Symbols = @This();

const first_room = 1;
const first_local_script = 2048;

pub const ScriptId = union(enum) {
    global: u32,
    local: struct { room: u8, number: u32 },
};

pub const Script = struct {
    name: ?[]const u8 = null,
    /// Lookup table from number to name
    locals: ArrayMap([]const u8) = .{},

    fn deinit(self: *Script, allocator: std.mem.Allocator) void {
        self.locals.deinit(allocator);
    }
};

const Room = struct {
    scripts: ArrayMap(Script) = .{},

    fn deinit(self: *Room, allocator: std.mem.Allocator) void {
        var i = self.scripts.len();
        while (i > 0) {
            i -= 1;
            const script = self.scripts.getPtr(i) orelse continue;
            script.deinit(allocator);
        }
        self.scripts.deinit(allocator);
    }
};

/// Lookup table from number to name
globals: ArrayMap([]const u8) = .{},
/// Map from name to number
global_names: std.StringArrayHashMapUnmanaged(u16) = .{},
scripts: ArrayMap(Script) = .{},
rooms: ArrayMap(Room) = .{},

pub fn deinit(self: *Symbols, allocator: std.mem.Allocator) void {
    var i = self.rooms.len();
    while (i > 0) {
        i -= 1;
        const room = self.rooms.getPtr(i) orelse continue;
        room.deinit(allocator);
    }
    self.rooms.deinit(allocator);

    i = self.scripts.len();
    while (i > 0) {
        i -= 1;
        const script = self.scripts.getPtr(i) orelse continue;
        script.deinit(allocator);
    }
    self.scripts.deinit(allocator);

    self.global_names.deinit(allocator);
    self.globals.deinit(allocator);
}

pub fn parse(allocator: std.mem.Allocator, ini_text: []const u8) !Symbols {
    var result = Symbols{};
    errdefer result.deinit(allocator);

    var line_number: u32 = 0;
    var lines = std.mem.splitScalar(u8, ini_text, '\n');
    while (lines.next()) |line| {
        line_number += 1;
        parseLine(allocator, line, &result) catch {
            try std.io.getStdErr().writer().print(
                "error on line {}\n",
                .{line_number},
            );
            return error.Reported;
        };
    }
    return result;
}

const Cx = struct {
    allocator: std.mem.Allocator,
    key_parts: std.mem.SplitIterator(u8, .scalar),
    value: []const u8,
    result: *Symbols,
};

fn parseLine(allocator: std.mem.Allocator, full_line: []const u8, result: *Symbols) !void {
    var line = full_line;

    // chop off comments
    const comment_start = std.mem.indexOfScalar(u8, full_line, ';');
    if (comment_start) |i|
        line = line[0..i];

    line = std.mem.trim(u8, line, " ");

    if (line.len == 0) // skip empty lines
        return;

    const eq = std.mem.indexOfScalar(u8, line, '=') orelse return error.BadData;
    const key = std.mem.trimRight(u8, line[0..eq], " ");
    const value = std.mem.trimLeft(u8, line[eq + 1 ..], " ");

    var cx = Cx{
        .allocator = allocator,
        .key_parts = std.mem.splitScalar(u8, key, '.'),
        .value = value,
        .result = result,
    };
    const part = cx.key_parts.first();
    if (std.mem.eql(u8, part, "global"))
        try handleGlobal(&cx)
    else if (std.mem.eql(u8, part, "script"))
        try handleGlobalScript(&cx)
    else if (std.mem.eql(u8, part, "room"))
        try handleRoom(&cx)
    else
        return error.BadData;
}

fn handleGlobal(cx: *Cx) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    try cx.result.globals.putNew(cx.allocator, number, cx.value);

    const entry = try cx.result.global_names.getOrPut(cx.allocator, cx.value);
    if (entry.found_existing)
        return error.BadData;
    entry.value_ptr.* = number;
}

fn handleGlobalScript(cx: *Cx) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    const script_entry = try cx.result.scripts.getOrPut(cx.allocator, number);
    if (script_entry.* == null)
        script_entry.* = .{};
    const script = &script_entry.*.?;

    try handleScript(cx, script);
}

fn handleScript(cx: *Cx, script: *Script) !void {
    const part = cx.key_parts.next() orelse
        return handleScriptName(cx, script);
    if (std.mem.eql(u8, part, "local"))
        return handleScriptLocal(cx, script)
    else
        return error.BadData;
}

fn handleScriptName(cx: *Cx, script: *Script) !void {
    if (script.name != null)
        return error.BadData;
    script.name = cx.value;
}

fn handleScriptLocal(cx: *Cx, script: *Script) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    try script.locals.putNew(cx.allocator, number, cx.value);
}

fn handleRoom(cx: *Cx) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u8, number_str, 10);

    const index = number - first_room;
    const room_entry = try cx.result.rooms.getOrPut(cx.allocator, index);
    if (room_entry.* == null)
        room_entry.* = .{};
    const room = &room_entry.*.?;

    const part = cx.key_parts.next() orelse
        return error.BadData;
    if (std.mem.eql(u8, part, "script"))
        try handleRoomScript(cx, room)
    else
        return error.BadData;
}

fn handleRoomScript(cx: *Cx, room: *Room) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    const index = number - first_local_script;
    const script_entry = try room.scripts.getOrPut(cx.allocator, index);
    if (script_entry.* == null)
        script_entry.* = .{};
    const script = &script_entry.*.?;

    try handleScript(cx, script);
}

pub fn getScript(self: *const Symbols, id: ScriptId) ?*const Script {
    return switch (id) {
        .global => |num| self.scripts.getPtr(num),
        .local => |s| {
            const room = self.rooms.get(s.room - first_room) orelse return null;
            return room.scripts.getPtr(s.number - first_local_script);
        },
    };
}
