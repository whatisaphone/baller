const std = @import("std");

const ArrayMap = @import("array_map.zig").ArrayMap;
const BlockId = @import("block_id.zig").BlockId;
const games = @import("games.zig");
const keyed = @import("keyed.zig");
const lang = @import("lang.zig");
const lexer = @import("lexer.zig");
const utils = @import("utils.zig");

const Symbols = @This();

pub const max_name_len = 255;

const first_room = 1;

pub const ScriptId = union(enum) {
    global: u32,
    enter: struct { room: u8 },
    exit: struct { room: u8 },
    local: struct { room: u8, number: u32 },
    object: struct { room: u8, number: u16, verb: u8 },
};

const Variable = struct {
    name: ?[]const u8 = null,
    type: TypeIndex = null_type,
};

pub const Script = struct {
    name: ?[]const u8 = null,
    params: ?u8 = null,
    locals: ArrayMap(Variable) = .empty,

    fn deinit(self: *Script, allocator: std.mem.Allocator) void {
        self.locals.deinit(allocator);
    }
};

const Room = struct {
    vars: ArrayMap(Variable) = .empty,
    /// Map from name to number
    var_names: std.StringHashMapUnmanaged(u16) = .empty,
    enter: Script = .{},
    exit: Script = .{},
    scripts: ArrayMap(Script) = .empty,

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

const Enum = struct {
    /// Sorted by value
    entries: std.ArrayList(EnumEntry),

    const empty: Enum = .{ .entries = .empty };

    fn deinit(self: *Enum, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }
};

pub const EnumEntry = struct {
    name: []const u8,
    value: i32,

    pub fn orderByValue(value: i32, other: EnumEntry) std.math.Order {
        return std.math.order(value, other.value);
    }
};

const MapIndex = keyed.Key(enum(u16) {});

const Map = struct {
    /// Sorted by value
    entries: std.ArrayList(MapEntry),

    const empty: Map = .{ .entries = .empty };

    fn deinit(self: *Map, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }
};

pub const MapEntry = struct {
    value: i32,
    name: ?[]const u8,
    type: TypeIndex,

    pub fn orderByValue(value: i32, other: MapEntry) std.math.Order {
        return std.math.order(value, other.value);
    }
};

game: games.Game,
types: std.ArrayList(Type) = .empty,
globals: ArrayMap(Variable) = .empty,
/// Map from name to number
global_names: std.StringArrayHashMapUnmanaged(u16) = .empty,
scripts: ArrayMap(Script) = .empty,
rooms: ArrayMap(Room) = .empty,
enums: std.ArrayList(Enum) = .empty,
/// Map from enum name to index within `enums`
enum_names: std.StringArrayHashMapUnmanaged(u16) = .empty,
maps: keyed.List(MapIndex, Map) = .empty,
map_names: std.StringArrayHashMapUnmanaged(MapIndex) = .empty,
sounds: ArrayMap([]const u8) = .empty,
costumes: ArrayMap([]const u8) = .empty,
charsets: ArrayMap([]const u8) = .empty,
images: ArrayMap([]const u8) = .empty,
talkies: ArrayMap([]const u8) = .empty,

pub fn init(allocator: std.mem.Allocator, game: games.Game) !Symbols {
    var result: Symbols = .{ .game = game };
    errdefer result.deinit(allocator);

    try result.addInternedTypes(allocator);

    return result;
}

pub fn deinit(self: *Symbols, allocator: std.mem.Allocator) void {
    self.talkies.deinit(allocator);
    self.images.deinit(allocator);
    self.charsets.deinit(allocator);
    self.costumes.deinit(allocator);
    self.sounds.deinit(allocator);
    self.map_names.deinit(allocator);

    for (self.maps.list.items) |*m|
        m.deinit(allocator);
    self.maps.deinit(allocator);

    self.enum_names.deinit(allocator);

    for (self.enums.items) |*e|
        e.deinit(allocator);
    self.enums.deinit(allocator);

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
    self.types.deinit(allocator);
}

pub fn parse(self: *Symbols, allocator: std.mem.Allocator, ini_text: []const u8) !void {
    // Major hack to allow forward references, so I don't have to rewrite
    // everything. Keep parsing the file over and over until the number of
    // errors converges.
    var parsed_lines: std.DynamicBitSetUnmanaged = try .initEmpty(allocator, 0);
    defer parsed_lines.deinit(allocator);

    var prev_error_count: u32 = 0;
    while (true) {
        const error_count = try parsePass(self, allocator, ini_text, .parsing, &parsed_lines);
        if (error_count == prev_error_count) break;
        prev_error_count = error_count;
    }

    if (prev_error_count == 0) return;

    const error_count = try parsePass(self, allocator, ini_text, .reporting, &parsed_lines);
    std.debug.assert(error_count == prev_error_count);
    return error.Reported;
}

const ParsePass = enum {
    parsing,
    reporting,
};

pub fn parsePass(
    self: *Symbols,
    allocator: std.mem.Allocator,
    ini_text: []const u8,
    pass: ParsePass,
    parsed_lines: *std.DynamicBitSetUnmanaged,
) !u32 {
    var error_count: u32 = 0;
    var line_index: u32 = 0;
    var lines = std.mem.splitScalar(u8, ini_text, '\n');
    while (lines.next()) |line| : (line_index += 1) {
        try utils.bitSetEnsureAddressable(allocator, parsed_lines, line_index, false);
        const already_parsed = parsed_lines.isSet(line_index);
        if (already_parsed) continue;

        if (parseLine(allocator, line, self)) {
            parsed_lines.set(line_index);
            continue;
        } else |_| {}

        error_count += 1;
        if (pass == .reporting) {
            const line_number = line_index + 1;

            var out_buf: [4096]u8 = undefined;
            var out = std.fs.File.stderr().writer(&out_buf);
            try out.interface.print("error on line {}\n", .{line_number});
            try out.interface.flush();
        }
    }
    return error_count;
}

const Cx = struct {
    allocator: std.mem.Allocator,
    key_parts: std.mem.SplitIterator(u8, .scalar),
    value: []const u8,
    result: *Symbols,
};

fn parseLine(allocator: std.mem.Allocator, full_line: []const u8, result: *Symbols) !void {
    var line = full_line;

    // chop off windows cr
    if (line.len != 0 and line[line.len - 1] == '\r')
        line = line[0 .. line.len - 1];

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

    var cx: Cx = .{
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
    else if (std.mem.eql(u8, part, "enum"))
        try handleEnum(&cx)
    else if (std.mem.eql(u8, part, "map"))
        try handleMap(&cx)
    else if (std.mem.eql(u8, part, "sound"))
        try handleSimpleGlob(&cx, &cx.result.sounds)
    else if (std.mem.eql(u8, part, "costume"))
        try handleSimpleGlob(&cx, &cx.result.costumes)
    else if (std.mem.eql(u8, part, "charset"))
        try handleSimpleGlob(&cx, &cx.result.charsets)
    else if (std.mem.eql(u8, part, "image"))
        try handleSimpleGlob(&cx, &cx.result.images)
    else if (std.mem.eql(u8, part, "talkie"))
        try handleSimpleGlob(&cx, &cx.result.talkies)
    else
        return error.BadData;
}

fn handleGlobal(cx: *Cx) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    const variable = try parseVariable(cx, cx.value);
    try cx.result.globals.putNew(cx.allocator, number, variable);

    if (variable.name) |name| {
        const entry = try cx.result.global_names.getOrPut(cx.allocator, name);
        if (entry.found_existing)
            return error.BadData;
        entry.value_ptr.* = number;
    }
}

fn handleGlobalScript(cx: *Cx) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    const script_entry = try cx.result.scripts.getOrPut(cx.allocator, number);
    if (script_entry.* == null)
        script_entry.* = .{};
    const script = &script_entry.*.?;

    try handleScript(cx, script, true);
}

fn handleScript(cx: *Cx, script: *Script, can_have_name_and_params: bool) !void {
    if (cx.key_parts.next()) |_| return error.BadData;

    const name_str, const after_name_opt = split(cx.value, '(');
    script.name = if (name_str.len == 0) null else name_str;
    if (!can_have_name_and_params and script.name != null) return error.BadData;
    if (script.name) |n| try validateSymbolName(n);

    const after_name = after_name_opt orelse return;
    const params, const after_params_opt = split(after_name, ')');
    const after_params = after_params_opt orelse return error.BadData;

    if (!can_have_name_and_params and params.len != 0) return error.BadData;

    var it = std.mem.tokenizeScalar(u8, params, ' ');
    while (it.next()) |s| {
        const v = try parseVariable(cx, s);
        try script.locals.putNew(cx.allocator, script.locals.len(), v);
    }
    script.params = std.math.cast(u8, script.locals.len()) orelse return error.BadData;

    it = std.mem.tokenizeScalar(u8, after_params, ' ');
    while (it.next()) |s| {
        const v = try parseVariable(cx, s);
        try script.locals.putNew(cx.allocator, script.locals.len(), v);
    }
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
        try handleScript(cx, &room.enter, false)
    else if (std.mem.eql(u8, part, "exit"))
        try handleScript(cx, &room.exit, false)
    else
        return error.BadData;
}

fn handleRoomVar(cx: *Cx, room: *Room) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    const variable = try parseVariable(cx, cx.value);
    try room.vars.putNew(cx.allocator, number, variable);

    if (variable.name) |name| {
        const entry = try room.var_names.getOrPut(cx.allocator, name);
        if (entry.found_existing)
            return error.BadData;
        entry.value_ptr.* = number;
    }
}

fn handleRoomScript(cx: *Cx, room: *Room) !void {
    const number_str = cx.key_parts.next() orelse return error.BadData;
    const number = try std.fmt.parseInt(u16, number_str, 10);

    const index = try std.math.sub(u16, number, games.firstLocalScript(cx.result.game));
    const script_entry = try room.scripts.getOrPut(cx.allocator, index);
    if (script_entry.* == null)
        script_entry.* = .{};
    const script = &script_entry.*.?;

    try handleScript(cx, script, true);
}

fn handleEnum(cx: *Cx) !void {
    const enum_name = cx.key_parts.next() orelse return error.BadData;

    const value_str = cx.key_parts.next() orelse return error.BadData;
    const value = try std.fmt.parseInt(i32, value_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    const item_name = cx.value;
    try validateSymbolName(item_name);

    const enum_entry = try cx.result.enum_names.getOrPut(cx.allocator, enum_name);
    if (!enum_entry.found_existing) {
        const enum_index = std.math.cast(u16, cx.result.enums.items.len) orelse
            return error.BadData;
        const the_enum = try cx.result.enums.addOne(cx.allocator);
        the_enum.* = .empty;
        enum_entry.value_ptr.* = enum_index;
    }
    const the_enum = &cx.result.enums.items[enum_entry.value_ptr.*];

    const value_index = std.sort.lowerBound(EnumEntry, the_enum.entries.items, value, EnumEntry.orderByValue);
    if (value_index != the_enum.entries.items.len and the_enum.entries.items[value_index].value == value)
        return error.BadData;
    try the_enum.entries.insert(cx.allocator, value_index, .{ .name = item_name, .value = value });
}

fn handleMap(cx: *Cx) !void {
    const map_name = cx.key_parts.next() orelse return error.BadData;

    const value_str = cx.key_parts.next() orelse return error.BadData;
    const value = try std.fmt.parseInt(i32, value_str, 10);

    if (cx.key_parts.next()) |_| return error.BadData;

    // not actually a variable, but we parse it the same way
    const mapping = try parseVariable(cx, cx.value);

    const map_entry = try cx.result.map_names.getOrPut(cx.allocator, map_name);
    if (!map_entry.found_existing)
        map_entry.value_ptr.* = try cx.result.maps.append(cx.allocator, .empty);
    const map = cx.result.maps.at(map_entry.value_ptr.*);

    const entry_index = std.sort.lowerBound(MapEntry, map.entries.items, value, MapEntry.orderByValue);
    if (entry_index != map.entries.items.len and map.entries.items[entry_index].value == value)
        return error.BadData;
    try map.entries.insert(cx.allocator, entry_index, .{
        .value = value,
        .type = mapping.type,
        .name = mapping.name,
    });
}

fn parseVariable(cx: *Cx, value: []const u8) !Variable {
    var result: Variable = .{};
    const name_str, const type_str_opt = split(value, ':');
    result.name = if (name_str.len == 1 and name_str[0] == '_') null else name_str;
    if (result.name) |n| try validateSymbolName(n);
    if (type_str_opt) |type_str|
        result.type = try parseType(cx, type_str);
    return result;
}

fn handleSimpleGlob(cx: *Cx, map: *ArrayMap([]const u8)) !void {
    const num_str = cx.key_parts.next() orelse return error.BadData;
    const num = std.fmt.parseInt(u16, num_str, 10) catch return error.BadData;

    if (cx.key_parts.next()) |_| return error.BadData;

    const name = cx.value;
    try validateSymbolName(name);

    try map.putNew(cx.allocator, num, name);
}

fn validateSymbolName(name: []const u8) !void {
    if (name.len == 0) return error.BadData;
    if (name.len > max_name_len) return error.BadData;

    if (!lexer.isIdentStart(name[0]))
        return error.BadData;
    for (name[1..]) |c|
        if (!lexer.isIdentContinue(c))
            return error.BadData;
}

pub const InternedType = enum {
    room,
    script,
    sound,
    costume,
    charset,
    image,
    talkie,
    FileMode,
    SaveLoad,
};

fn addInternedTypes(self: *Symbols, allocator: std.mem.Allocator) !void {
    try self.types.append(allocator, .room);
    try self.types.append(allocator, .script);
    try self.types.append(allocator, .sound);
    try self.types.append(allocator, .costume);
    try self.types.append(allocator, .charset);
    try self.types.append(allocator, .image);
    try self.types.append(allocator, .talkie);
    try self.parse(allocator,
        \\enum.FileMode.1=FOR-READ
        \\enum.FileMode.2=FOR-WRITE
        \\enum.FileMode.6=FOR-APPEND
        \\enum.SaveLoad.1=SAVE
        \\enum.SaveLoad.2=LOAD
    );
    try self.types.append(allocator, .{ .@"enum" = 0 });
    try self.types.append(allocator, .{ .@"enum" = 1 });
}

pub fn getInternedType(typ: InternedType) TypeIndex {
    return @intFromEnum(typ);
}

fn parseType(cx: *Cx, s: []const u8) !TypeIndex {
    if (s.len != 0 and s[0] == '[') {
        var rest = s[1..];
        var down = null_type;
        var across = null_type;
        blk: {
            const istr, rest = try splitAlways(rest, ']');
            if (istr.len == 0) break :blk;
            across = try parseType(cx, istr);
        }
        blk: {
            if (rest.len == 0) break :blk;
            if (rest[0] != '[') break :blk;
            down = across;
            across = null_type;
            const istr, rest = try splitAlways(rest[1..], ']');
            if (istr.len == 0) break :blk;
            across = try parseType(cx, istr);
        }
        const value = blk: {
            if (rest.len == 0) break :blk null_type;
            if (rest[0] == '[') return error.BadData;
            break :blk try parseType(cx, rest);
        };
        return addType(cx, .{ .array = .{ .down = down, .across = across, .value = value } });
    } else if (std.mem.eql(u8, s, "Char"))
        return addType(cx, .char)
    else if (std.mem.eql(u8, s, "Room"))
        return addType(cx, .room)
    else if (std.mem.eql(u8, s, "Script"))
        return addType(cx, .script)
    else if (std.mem.eql(u8, s, "Image"))
        return addType(cx, .image)
    else if (std.mem.eql(u8, s, "Sound"))
        return addType(cx, .sound)
    else if (cx.result.enum_names.get(s)) |e|
        return addType(cx, .{ .@"enum" = e })
    else if (cx.result.map_names.get(s)) |m|
        return addType(cx, .{ .map = m })
    else
        return error.BadData;
}

fn addType(cx: *Cx, typ: Type) !TypeIndex {
    const index = std.math.cast(TypeIndex, cx.result.types.items.len) orelse
        return error.BadData;
    try cx.result.types.append(cx.allocator, typ);
    return index;
}

fn split(str: []const u8, ch: u8) struct { []const u8, ?[]const u8 } {
    const i = std.mem.indexOfScalar(u8, str, ch) orelse return .{ str, null };
    return .{
        std.mem.trimRight(u8, str[0..i], " "),
        std.mem.trimLeft(u8, str[i + 1 ..], " "),
    };
}

fn splitAlways(str: []const u8, ch: u8) !struct { []const u8, []const u8 } {
    const a, const b = split(str, ch);
    if (b == null) return error.BadData;
    return .{ a, b.? };
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
        .object => null,
    };
}

pub fn getVariable(
    self: *const Symbols,
    room_number: u8,
    script_id: ScriptId,
    variable: lang.Variable,
) ?*const Variable {
    const kind, const num = variable.decode() catch return null;
    switch (kind) {
        .global => return self.globals.getPtr(num),
        .local => {
            const script_symbol = self.getScript(script_id) orelse return null;
            return script_symbol.locals.getPtr(num);
        },
        .room => {
            const room = self.getRoom(room_number) orelse return null;
            return room.vars.getPtr(num);
        },
    }
}

const FormatVariableName = struct {
    symbols: *const Symbols,
    room_number: u8,
    script_id: ScriptId,
    variable: lang.Variable,

    pub fn format(f: *const FormatVariableName, w: *std.io.Writer) !void {
        // If there's a name, write the name
        if (f.symbols.getVariable(f.room_number, f.script_id, f.variable)) |v|
            if (v.name) |name|
                return w.writeAll(name);

        // Otherwise fall back to a generated name
        const kind, const number = f.variable.decode() catch return error.WriteFailed;
        const prefix = prefix: switch (kind) {
            .global => "global",
            .room => "room",
            .local => {
                const num_params = num_params: {
                    const symbol = f.symbols.getScript(f.script_id) orelse break :num_params 0;
                    break :num_params symbol.params orelse 0;
                };
                break :prefix if (number < num_params) "arg" else "local";
            },
        };
        try w.print("{s}{}", .{ prefix, number });
    }
};

pub fn fmtVariableName(
    self: *const Symbols,
    room_number: u8,
    script_id: ScriptId,
    variable: lang.Variable,
) FormatVariableName {
    return .{
        .symbols = self,
        .room_number = room_number,
        .script_id = script_id,
        .variable = variable,
    };
}

pub fn getRoom(self: *const Symbols, number: u8) ?*const Room {
    return self.rooms.getPtr(number - first_room);
}

const FormatScriptName = struct {
    symbols: *const Symbols,
    room_number: u8,
    script_number: u32,

    pub fn format(f: *const FormatScriptName, w: *std.io.Writer) !void {
        if (f.symbols.getScriptName(f.room_number, f.script_number)) |name| {
            try w.writeAll(name);
            return;
        }
        // If not found, generate a default name
        const prefix = if (f.script_number < games.firstLocalScript(f.symbols.game)) "scr" else "lsc";
        try w.print("{s}{}", .{ prefix, f.script_number });
    }
};

pub fn fmtScriptName(self: *const Symbols, room_number: u8, script_number: u32) FormatScriptName {
    return .{ .symbols = self, .room_number = room_number, .script_number = script_number };
}

fn getScriptName(self: *const Symbols, room_number: u8, script_number: u32) ?[]const u8 {
    const id: ScriptId = if (script_number < games.firstLocalScript(self.game))
        .{ .global = script_number }
    else
        .{ .local = .{ .room = room_number, .number = script_number } };
    const script = self.getScript(id) orelse return null;
    return script.name;
}

const FormatGlobName = struct {
    symbols: *const Symbols,
    kind: GlobKind,
    glob_number: u32,

    pub fn format(f: *const FormatGlobName, w: *std.io.Writer) !void {
        switch (f.symbols.getGlobName(f.kind, f.glob_number)) {
            .string => |s| try w.writeAll(s),
            .prefixed => |p| try w.print("{s}{}", .{ p, f.glob_number }),
        }
    }
};

pub fn fmtGlobName(self: *const Symbols, kind: GlobKind, glob_number: u32) FormatGlobName {
    return .{ .symbols = self, .kind = kind, .glob_number = glob_number };
}

fn getGlobName(self: *const Symbols, kind: GlobKind, glob_number: u32) union(enum) {
    string: []const u8,
    prefixed: []const u8,
} {
    switch (kind) {
        .room_image, .room => unreachable,
        .script => if (self.scripts.getPtr(glob_number)) |s|
            if (s.name) |n| return .{ .string = n },
        .sound => if (self.sounds.get(glob_number)) |s|
            return .{ .string = s },
        .costume => if (self.costumes.get(glob_number)) |s|
            return .{ .string = s },
        .charset => if (self.charsets.get(glob_number)) |s|
            return .{ .string = s },
        .image => if (self.images.get(glob_number)) |s|
            return .{ .string = s },
        .talkie => if (self.talkies.get(glob_number)) |s|
            return .{ .string = s },
    }

    const prefix = switch (kind) {
        .room_image, .room => unreachable,
        .script => "scr",
        .sound => "sound",
        .costume => "costume",
        .charset => "charset",
        .image => "image",
        .talkie => "talkie",
    };
    return .{ .prefixed = prefix };
}

pub const TypeIndex = u16;
pub const null_type: TypeIndex = 0xffff;

pub const Type = union(enum) {
    char,
    room,
    script,
    /// Index within `symbols.enums`
    @"enum": u16,
    map: MapIndex,
    array: struct { down: TypeIndex, across: TypeIndex, value: TypeIndex },
    sound,
    costume,
    charset,
    image,
    talkie,
};

pub const GlobKind = enum {
    room_image,
    room,
    script,
    sound,
    costume,
    charset,
    image,
    talkie,

    pub fn fromBlockId(block_id: BlockId) ?GlobKind {
        return switch (block_id) {
            .RMIM => .room_image,
            .RMDA => .room,
            .SCRP => .script,
            .SOUN, .DIGI, .TALK, .WSOU => .sound,
            .AKOS => .costume,
            .CHAR => .charset,
            .AWIZ, .MULT => .image,
            .TLKE => .talkie,
            else => null,
        };
    }

    pub fn hasName(self: GlobKind) bool {
        return switch (self) {
            .room_image, .room => false,
            .script, .sound, .costume, .charset, .image, .talkie => true,
        };
    }
};
