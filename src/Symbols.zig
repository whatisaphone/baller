const std = @import("std");

const ArrayMap = @import("array_map.zig").ArrayMap;
const games = @import("games.zig");
const utils = @import("utils.zig");

const Symbols = @This();

const first_room = 1;

pub const ScriptId = union(enum) {
    global: u32,
    enter: struct { room: u8 },
    exit: struct { room: u8 },
    local: struct { room: u8, number: u32 },

    pub fn room(self: ScriptId) ?u8 {
        return switch (self) {
            .global => null,
            inline .enter, .exit, .local => |id| id.room,
        };
    }
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
    /// Lookup table from number to name
    vars: ArrayMap([]const u8) = .{},
    /// Map from name to number
    var_names: std.StringHashMapUnmanaged(u16) = .{},
    enter: Script = .{},
    exit: Script = .{},
    scripts: ArrayMap(Script) = .{},

    fn deinit(self: *Room, allocator: std.mem.Allocator) void {
        var i = self.scripts.len();
        while (i > 0) {
            i -= 1;
            const script = self.scripts.getPtr(i) orelse continue;
            script.deinit(allocator);
        }
        self.scripts.deinit(allocator);

        self.exit.deinit(allocator);
        self.enter.deinit(allocator);
        self.var_names.deinit(allocator);
        self.vars.deinit(allocator);
    }
};

game: games.Game,
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

pub fn parse(
    allocator: std.mem.Allocator,
    game: games.Game,
    ini_text: []const u8,
) !Symbols {
    var result = Symbols{
        .game = game,
    };
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
    if (std.mem.eql(u8, part, "var"))
        try handleRoomVar(cx, room)
    else if (std.mem.eql(u8, part, "script"))
        try handleRoomScript(cx, room)
    else if (std.mem.eql(u8, part, "enter"))
        try handleScript(cx, &room.enter)
    else if (std.mem.eql(u8, part, "exit"))
        try handleScript(cx, &room.exit)
    else
        return error.BadData;
}

fn handleRoomVar(cx: *Cx, room: *Room) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    try room.vars.putNew(cx.allocator, number, cx.value);

    const entry = try room.var_names.getOrPut(cx.allocator, cx.value);
    if (entry.found_existing)
        return error.BadData;
    entry.value_ptr.* = number;
}

fn handleRoomScript(cx: *Cx, room: *Room) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    const index = try std.math.sub(u16, number, games.firstLocalScript(cx.result.game));
    const script_entry = try room.scripts.getOrPut(cx.allocator, index);
    if (script_entry.* == null)
        script_entry.* = .{};
    const script = &script_entry.*.?;

    try handleScript(cx, script);
}

pub fn getScript(self: *const Symbols, id: ScriptId) ?*const Script {
    return switch (id) {
        .global => |num| self.scripts.getPtr(num),
        .enter => |s| {
            const room = self.getRoom(s.room) orelse return null;
            return &room.enter;
        },
        .exit => |s| {
            const room = self.getRoom(s.room) orelse return null;
            return &room.exit;
        },
        .local => |s| {
            const room = self.getRoom(s.room) orelse return null;
            const index = s.number - games.firstLocalScript(self.game);
            return room.scripts.getPtr(index);
        },
    };
}

pub fn getRoom(self: *const Symbols, number: u8) ?*const Room {
    return self.rooms.getPtr(number - first_room);
}
