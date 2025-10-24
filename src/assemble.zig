const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const UsageTracker = @import("UsageTracker.zig");
const lang = @import("lang.zig");
const script = @import("script.zig");
const utils = @import("utils.zig");

const horizontal_whitespace = " \t\r";

const Scopes = struct {
    project: *const std.StringHashMapUnmanaged(script.Symbol),
    room: *const std.StringHashMapUnmanaged(script.Symbol),
};

pub fn assemble(
    allocator: std.mem.Allocator,
    diagnostic: *Diagnostic,
    path: []const u8,
    vm: *const lang.Vm,
    asm_str: []const u8,
    project_scope: *const std.StringHashMapUnmanaged(script.Symbol),
    room_scope: *const std.StringHashMapUnmanaged(script.Symbol),
) !std.ArrayListUnmanaged(u8) {
    // map from label name to offset
    var label_offsets: std.StringHashMapUnmanaged(u16) = .empty;
    defer label_offsets.deinit(allocator);

    var label_fixups: std.ArrayListUnmanaged(Fixup) = .empty;
    defer label_fixups.deinit(allocator);

    var locals: utils.TinyArray([]const u8, UsageTracker.max_local_vars) = .empty;

    var bytecode: std.ArrayListUnmanaged(u8) = .empty;
    errdefer bytecode.deinit(allocator);
    try bytecode.ensureTotalCapacity(allocator, asm_str.len / 8);

    var line_number: u32 = 0;
    var lines = std.mem.splitScalar(u8, asm_str, '\n');
    while (lines.next()) |line| {
        line_number += 1;

        assembleLine(
            allocator,
            diagnostic,
            path,
            vm,
            .{ .project = project_scope, .room = room_scope },
            &label_offsets,
            &label_fixups,
            &locals,
            line_number,
            line,
            &bytecode,
        ) catch |err| {
            if (err != error.AddedToDiagnostic)
                diagnostic.errAt(loc(path, line_number), "error", .{});
            return error.AddedToDiagnostic;
        };
    }

    for (label_fixups.items) |fixup| {
        const label_offset = label_offsets.get(fixup.label_name) orelse {
            diagnostic.errAt(
                locUnknownLine(path),
                "label not found: \"{s}\"",
                .{fixup.label_name},
            );
            return error.AddedToDiagnostic;
        };
        const rel32 = @as(i32, @intCast(label_offset)) - @as(i32, @intCast(fixup.offset)) - 2;
        const rel = std.math.cast(i16, rel32) orelse return error.BadData;
        std.mem.writeInt(i16, bytecode.items[fixup.offset..][0..2], rel, .little);
    }

    return bytecode;
}

fn assembleLine(
    allocator: std.mem.Allocator,
    diagnostic: *Diagnostic,
    path: []const u8,
    vm: *const lang.Vm,
    scopes: Scopes,
    label_offsets: *std.StringHashMapUnmanaged(u16),
    label_fixups: *std.ArrayListUnmanaged(Fixup),
    locals: *utils.TinyArray([]const u8, UsageTracker.max_local_vars),
    line_number: u32,
    line_full: []const u8,
    bytecode: *std.ArrayListUnmanaged(u8),
) !void {
    const line = std.mem.trim(u8, line_full, horizontal_whitespace);

    if (line.len == 0) // skip blank lines
        return;
    if (line[0] == ';') // skip comments
        return;

    // If we find a label, store its offset so jumps can be fixed up later.
    if (std.mem.indexOfScalar(u8, line, ' ') == null and
        line[line.len - 1] == ':')
    {
        const label_name = line[0 .. line.len - 1];
        const entry = try label_offsets.getOrPut(allocator, label_name);
        if (entry.found_existing) {
            diagnostic.errAt(
                loc(path, line_number),
                "duplicate label: \"{s}\"",
                .{label_name},
            );
            return error.AddedToDiagnostic;
        }
        entry.value_ptr.* = @intCast(bytecode.items.len);
        return;
    }

    const ins_name, var rest = try tokenizeKeyword(line);

    if (std.mem.eql(u8, ins_name, ".db")) {
        const byte = try std.fmt.parseInt(u8, rest, 10);
        try bytecode.append(allocator, byte);
        return;
    } else if (std.mem.eql(u8, ins_name, ".local")) {
        const local_name, rest = try tokenizeKeyword(rest);
        if (rest.len != 0)
            return error.BadData;
        try locals.append(local_name);
        return;
    }

    const ins = lang.lookup(vm, ins_name) orelse {
        diagnostic.errAt(
            loc(path, line_number),
            "unknown instruction: \"{s}\"",
            .{ins_name},
        );
        return error.AddedToDiagnostic;
    };

    try bytecode.appendSlice(allocator, ins.opcode.slice());

    for (ins.operands.slice()) |op| {
        switch (op) {
            .u8 => {
                const int, rest = try tokenizeInt(u8, rest);
                try bytecode.append(allocator, int);
            },
            .i16 => {
                const int, rest = try tokenizeInt(i16, rest);
                try utils.writeInt(allocator, bytecode, i16, int, .little);
            },
            .i32 => {
                const int, rest = try tokenizeInt(i32, rest);
                try utils.writeInt(allocator, bytecode, i32, int, .little);
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
                const variable, rest = try tokenizeVariable(rest, locals.slice(), scopes);
                try utils.writeInt(allocator, bytecode, u16, variable.raw, .little);
            },
            .string => {
                const string, rest = try tokenizeString(rest);
                try bytecode.appendSlice(allocator, string);
                try bytecode.append(allocator, 0);
            },
        }
    }

    // Don't allow extra unparsed operands

    if (rest.len == 0) // empty string: ok
        return;

    const token, rest = try tokenizeKeyword(rest);
    if (token[0] == ';') // comment: ok
        return;

    // anything else: not ok
    diagnostic.errAt(loc(path, line_number), "too many operands", .{});
    return error.AddedToDiagnostic;
}

const Fixup = struct {
    offset: u16,
    label_name: []const u8,
};

fn tokenizeKeyword(str: []const u8) !struct { []const u8, []const u8 } {
    var split = std.mem.tokenizeAny(u8, str, horizontal_whitespace);
    const keyword = split.next() orelse return error.BadData;
    return .{ keyword, split.rest() };
}

fn tokenizeInt(comptime T: type, str: []const u8) !struct { T, []const u8 } {
    var split = std.mem.tokenizeAny(u8, str, horizontal_whitespace);
    const int_str = split.next() orelse return error.BadData;
    const rest = split.rest();

    const int = try std.fmt.parseInt(T, int_str, 10);

    return .{ int, rest };
}

fn tokenizeVariable(str: []const u8, locals: []const []const u8, scopes: Scopes) !struct { lang.Variable, []const u8 } {
    var split = std.mem.tokenizeAny(u8, str, horizontal_whitespace);
    const var_str = split.next() orelse return error.BadData;
    const rest = split.rest();
    const variable = try parseVariable(var_str, locals, scopes);
    return .{ variable, rest };
}

fn parseVariable(var_str: []const u8, locals: []const []const u8, scopes: Scopes) !lang.Variable {
    for (0.., locals) |i, name|
        if (std.mem.eql(u8, name, var_str))
            return .init(.local, @intCast(i));

    if (scopes.room.get(var_str)) |s| if (s == .variable) return s.variable;
    if (scopes.project.get(var_str)) |s| if (s == .variable) return s.variable;

    const kind: lang.Variable.Kind, const num_str =
        if (std.mem.startsWith(u8, var_str, "local"))
            .{ .local, var_str[5..] }
        else
            return error.BadData;

    const num = try std.fmt.parseInt(u14, num_str, 10);

    return switch (kind) {
        .global => unreachable,
        .local => .init(.local, num),
        .room => unreachable,
    };
}

fn tokenizeString(str: []const u8) !struct { []const u8, []const u8 } {
    if (str.len == 0 or str[0] != '"')
        return error.BadData;
    const end = std.mem.indexOfScalarPos(u8, str, 1, '"') orelse return error.BadData;
    return .{ str[1..end], str[end + 1 ..] };
}

fn loc(path: []const u8, line_number: u32) Diagnostic.Location {
    return .{ .path = path, .loc = .fakeJustLine(line_number) };
}

fn locUnknownLine(path: []const u8) Diagnostic.Location {
    return .{ .path = path, .loc = .fakeJustLine(0) };
}
