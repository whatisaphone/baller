const std = @import("std");

const Symbols = @import("Symbols.zig");
const lang = @import("lang.zig");

pub fn assemble(
    allocator: std.mem.Allocator,
    language_stuff: struct {
        *const lang.Language,
        *const std.StringHashMapUnmanaged(std.BoundedArray(u8, 2)),
    },
    asm_str: []const u8,
    symbols: *const Symbols,
    id: Symbols.ScriptId,
) !std.ArrayListUnmanaged(u8) {
    const language, const inss = language_stuff;

    // map from label name to offset
    var label_offsets = std.StringHashMapUnmanaged(u16){};
    defer label_offsets.deinit(allocator);

    var label_fixups = std.ArrayListUnmanaged(Fixup){};
    defer label_fixups.deinit(allocator);

    var locals = std.ArrayListUnmanaged([]const u8){};
    defer locals.deinit(allocator);

    var bytecode = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 256);
    errdefer bytecode.deinit(allocator);

    var lines = std.mem.tokenizeScalar(u8, asm_str, '\n');
    while (lines.next()) |line_full| {
        var line = std.mem.trim(u8, line_full, " ");

        if (line.len == 0) // skip blank lines
            continue;
        if (line[0] == ';') // skip comments
            continue;

        // If we find a label, store its offset so jumps can be fixed up later.
        if (std.mem.indexOfScalar(u8, line, ' ') == null and
            line[line.len - 1] == ':')
        {
            const label_name = line[0 .. line.len - 1];
            const entry = try label_offsets.getOrPut(allocator, label_name);
            if (entry.found_existing)
                return error.BadData;
            entry.value_ptr.* = @intCast(bytecode.items.len);
            continue;
        }

        const ins_name, var rest = try tokenizeKeyword(line);

        if (std.mem.eql(u8, ins_name, ".db")) {
            const byte = try std.fmt.parseInt(u8, rest, 10);
            try bytecode.append(allocator, byte);
            continue;
        } else if (std.mem.eql(u8, ins_name, ".local")) {
            const local_name, rest = try tokenizeKeyword(rest);
            if (rest.len != 0)
                return error.BadData;
            try locals.append(allocator, local_name);
            continue;
        }

        const ins_bytes = inss.get(ins_name) orelse return error.BadData;
        try bytecode.appendSlice(allocator, ins_bytes.slice());

        const opcode = switch (ins_bytes.len) {
            1 => language.opcodes[ins_bytes.get(0)],
            2 => blk: {
                const nest_start = language.opcodes[ins_bytes.get(0)].nested;
                break :blk language.opcodes[nest_start << 8 | ins_bytes.get(1)];
            },
            else => unreachable,
        };

        for (opcode.ins.operands.slice()) |op| {
            switch (op) {
                .u8 => {
                    const int, rest = try tokenizeInt(u8, rest);
                    try bytecode.append(allocator, int);
                },
                .i16 => {
                    const int, rest = try tokenizeInt(i16, rest);
                    try bytecode.writer(allocator).writeInt(i16, int, .little);
                },
                .i32 => {
                    const int, rest = try tokenizeInt(i32, rest);
                    try bytecode.writer(allocator).writeInt(i32, int, .little);
                },
                .relative_offset => {
                    const label_name, rest = try tokenizeKeyword(rest);
                    try label_fixups.append(allocator, .{
                        .offset = @intCast(bytecode.items.len),
                        .label_name = label_name,
                    });
                    _ = try bytecode.addManyAsSlice(allocator, 2);
                },
                .variable => {
                    const variable, rest = try tokenizeVariable(rest, symbols, id);
                    try bytecode.writer(allocator).writeInt(u16, variable.raw, .little);
                },
                .string => {
                    const string, rest = try tokenizeString(rest);
                    try bytecode.appendSlice(allocator, string);
                    try bytecode.append(allocator, 0);
                },
            }
        }
        // Don't allow extra unparsed operands
        if (rest.len != 0)
            return error.BadData;
    }

    for (label_fixups.items) |fixup| {
        const label_offset = label_offsets.get(fixup.label_name) orelse
            return error.BadData;
        const rel32 = @as(i32, @intCast(label_offset)) - @as(i32, @intCast(fixup.offset)) - 2;
        const rel = std.math.cast(i16, rel32) orelse return error.BadData;
        std.mem.writeInt(i16, bytecode.items[fixup.offset..][0..2], rel, .little);
    }

    return bytecode;
}

const Fixup = struct {
    offset: u16,
    label_name: []const u8,
};

fn tokenizeKeyword(str: []const u8) !struct { []const u8, []const u8 } {
    var split = std.mem.tokenizeScalar(u8, str, ' ');
    const keyword = split.next() orelse return error.BadData;
    return .{ keyword, split.rest() };
}

fn tokenizeInt(comptime T: type, str: []const u8) !struct { T, []const u8 } {
    var split = std.mem.tokenizeScalar(u8, str, ' ');
    const int_str = split.next() orelse return error.BadData;
    const rest = split.rest();

    const int = try std.fmt.parseInt(T, int_str, 10);

    return .{ int, rest };
}

fn tokenizeVariable(
    str: []const u8,
    symbols: *const Symbols,
    id: Symbols.ScriptId,
) !struct { lang.Variable, []const u8 } {
    var split = std.mem.tokenizeScalar(u8, str, ' ');
    const var_str = split.next() orelse return error.BadData;
    const rest = split.rest();
    const variable = try parseVariable(var_str, symbols, id);
    return .{ variable, rest };
}

fn parseVariable(var_str: []const u8, symbols: *const Symbols, id: Symbols.ScriptId) !lang.Variable {
    if (symbols.global_names.get(var_str)) |num|
        return lang.Variable.init(.{ .global = num });

    if (symbols.getScript(id)) |script|
        for (0..script.locals.len()) |i|
            if (script.locals.get(i)) |name|
                if (std.mem.eql(u8, name, var_str))
                    return lang.Variable.init(.{ .local = @intCast(i) });

    const kind: lang.Variable.Kind, const num_str =
        if (std.mem.startsWith(u8, var_str, "global"))
        .{ .global, var_str[6..] }
    else if (std.mem.startsWith(u8, var_str, "local"))
        .{ .local, var_str[5..] }
    else if (std.mem.startsWith(u8, var_str, "room"))
        .{ .room, var_str[4..] }
    else
        return error.BadData;

    const num = try std.fmt.parseInt(u14, num_str, 10);

    return switch (kind) {
        .global => lang.Variable.init(.{ .global = num }),
        .local => lang.Variable.init(.{ .local = num }),
        .room => lang.Variable.init(.{ .room = num }),
    };
}

fn tokenizeString(str: []const u8) !struct { []const u8, []const u8 } {
    if (str.len == 0 or str[0] != '"')
        return error.BadData;
    const end = std.mem.indexOfScalarPos(u8, str, 1, '"') orelse return error.BadData;
    return .{ str[1..end], str[end + 1 ..] };
}
