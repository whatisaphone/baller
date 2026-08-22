const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const UsageTracker = @import("UsageTracker.zig");
const build = @import("build.zig");
const fs = @import("fs.zig");
const Target = @import("games.zig").Target;

test "helpful error when project is not a project" {
    try testProjectError(
        \\rmim {}
        \\^ projects must begin with a target directive. make sure you're building a project file, usually called project.scu.
        \\
    );
}

test "empty char literal" {
    try testRoomError(
        \\''
        \\^ bad char literal
    );
}

test "unimplemented char literal escapes" {
    try testRoomError(
        \\'\'
        \\^ bad char literal
    );
}

test "char literal too long" {
    try testRoomError(
        \\'oh no'
        \\^ bad char literal
    );
}

test "char literal with newline" {
    try testRoomError(
        \\'
        \\^ bad char literal
    );
}

test "string literal with newline" {
    try testRoomError(
        \\"
        \\ ^ invalid character in string
    );
}

test "script must have number" {
    try testRoomError(
        \\script s {
        \\         ^ expected '@', found '{'
    );
}

test "raw-glob file not found" {
    try testRoomError(
        \\raw-glob DIGI 1 "oops.bin"
        \\^ failed to read file: oops.bin (FileNotFound)
    );
}

test "duplicate disk numbers" {
    try testProjectError(
        \\target sputm99
        \\index {}
        \\disk 1 {}
        \\disk 1 {}
        \\^ duplicate symbol number
    );
}

test "disk number range" {
    std.debug.assert(Ast.min_disk_number == 1);
    std.debug.assert(Ast.max_disk_number == 9);
    try testProjectError(
        \\target sputm99
        \\index {}
        \\disk 0 {}
        \\^ symbol number too small
        \\disk 1 {}
        \\disk 9 {}
        \\disk 10 {}
        \\^ symbol number too big
    );
}

test "duplicate room numbers" {
    try testProjectError(
        \\target sputm99
        \\index {}
        \\disk 1 {
        \\    room 1 "room" "room.scu"
        \\    room 1 "room" "room.scu"
        \\    ^ duplicate room number
        \\}
    );
}

// SCUMM uses room 0 as "no room"
test "forbid room 0" {
    try testProjectError(
        \\target sputm99
        \\index {}
        \\disk 1 {
        \\    room 0 "room" "room.scu"
        \\    ^ out of range
        \\}
    );
}

// SCUMM uses room 255 as a special value in some places, e.g. `put-actor`
test "forbid room 255" {
    try testProjectError(
        \\target sputm99
        \\index {}
        \\disk 1 {
        \\    room 254 "room" "room.scu"
        \\    room 255 "room" "room.scu"
        \\    ^ out of range
        \\}
    );
}

test "duplicate global var numbers" {
    try testProjectError(
        \\target sputm99
        \\index {}
        \\var a@0
        \\var b@0
        \\^ duplicate symbol number
    );
}

test "global var number too big" {
    std.debug.assert(UsageTracker.max_global_vars == 800);
    try testProjectError(
        \\target sputm99
        \\index {}
        \\var a@799
        \\var b@800
        \\^ symbol number too big
    );
}

test "duplicate room var numbers" {
    try testRoomError(
        \\var a@0
        \\var b@0
        \\^ duplicate symbol number
    );
}

test "room var number too big" {
    try testRoomError(
        \\var a@255
        \\var b@256
        \\^ symbol number too big
    );
}

test "duplicate local script numbers" {
    try testRoomError(
        \\local-script a@2048 {}
        \\local-script b@2048 {}
        \\^ duplicate symbol number
    );
}

test "local script number range" {
    std.debug.assert(Target.sputm99.firstLocalScript() == 2048);
    std.debug.assert(UsageTracker.max_local_scripts == 1024);
    // 3072 is 2048 + 1024
    try testRoomError(
        \\local-script a@2047 {}
        \\^ symbol number too small
        \\local-script b@2048 {}
        \\local-script c@3071 {}
        \\local-script d@3072 {}
        \\^ symbol number too big
    );
}

test "duplicate glob numbers" {
    try testRoomError(
        \\raw-glob DIGI a@1 {}
        \\raw-glob DIGI b@1 {}
        \\^ duplicate glob number 1
    );
}

test "shadowing: global var, room var" {
    try testRoomError(
        \\var global_var@0
        \\^ duplicate name: global_var
    );
}

test "shadowing: glob, script" {
    try testRoomError(
        \\raw-glob DIGI s@1 {}
        \\script s@1 {}
        \\^ duplicate name: s
    );
}

test "shadowing: script, local script" {
    try testRoomError(
        \\script s@1 {}
        \\local-script s@2048 {}
        \\^ duplicate name: s
    );
}

test "shadowing: local script, local script" {
    try testRoomError(
        \\local-script s@2048 {}
        \\local-script s@2049 {}
        \\^ duplicate name: s
    );
}

test "shadowing: global var, local var" {
    try testRoomError(
        \\script s@1 {
        \\    var global_var
        \\        ^ duplicate name: global_var
        \\}
    );
}

test "shadowing: local var, local var" {
    try testRoomError(
        \\script s@1 {
        \\    var v
        \\    var v
        \\        ^ duplicate name: v
        \\}
    );
}

test "bad array lhs node" {
    try testRoomError(
        \\var v@0
        \\script s@1 {
        \\    v = 0[0]
        \\        ^ node type not expected in this position
        \\}
    );
}

test "bad array lhs symbol" {
    try testRoomError(
        \\var v@0
        \\script s@1 {
        \\    v = s[0]
        \\        ^ not a variable
        \\}
    );
}

test "bad jump operand" {
    try testRoomError(
        \\script s@1 {
        \\    jump 0
        \\         ^ node type not expected in this position
        \\}
    );
}

test "ins wrong arg count" {
    try testRoomError(
        \\script s@1 {
        \\    print-image 0 0
        \\    ^ expected 1 args, found 2
        \\}
    );
}

test "ins variadic too few args" {
    try testRoomError(
        \\script s@1 {
        \\    start-script
        \\    ^ expected at least 1 args, found 0
        \\}
    );
}

test "compound wrong arg count" {
    try testRoomError(
        \\script s@1 {
        \\    sprite.init 0 0
        \\    ^ expected 1 args, found 2
        \\}
    );
}

test "node in forbidden position" {
    try testRoomError(
        \\script s@1 {
        \\    0 = 0
        \\    ^ node type not expected in this position
        \\}
    );
}

test "forbid statements in expression position" {
    try testRoomError(
        \\script s@1 {
        \\    global_var = start-script s
        \\                 ^ this instruction does not return a value
        \\}
    );
}

test "forbid expressions in statement position" {
    try testRoomError(
        \\script s@1 {
        \\    call-script s
        \\    ^ this instruction cannot appear at statement level
        \\}
    );
}

test "duplicate label" {
    try testRoomError(
        \\script s@1 {
        \\    a:
        \\    update-screen
        \\    a:
        \\    ^ duplicate label
        \\}
    );
}

test "case else isn't last" {
    try testRoomError(
        \\script s@1 {
        \\    case 0 {
        \\        else {}
        \\        0 {}
        \\        ^ expected '}', found <integer>
        \\    }
        \\}
    );
}

test "parse multiline binop in parens" {
    try testNoErrors(
        \\script s@1 {
        \\    g1 = (
        \\        g2
        \\        +
        \\        g3
        \\    )
        \\}
    );
}

test "forbid multiline binop outside parens" {
    try testRoomError(
        \\script s@1 {
        \\    g1
        \\    = g2 + g3
        \\    ^ unexpected '='
        \\}
    );
    try testRoomError(
        \\script s@1 {
        \\    g1 =
        \\        ^ unexpected <newline>
        \\    g2 + g3
        \\}
    );
    try testRoomError(
        \\script s@1 {
        \\    g1 = g2
        \\    + g3
        \\    ^ unexpected '+'
        \\}
    );
    try testRoomError(
        \\script s@1 {
        \\    g1 = g2 +
        \\             ^ unexpected <newline>
        \\    g3
        \\}
    );
}

test "parse multiline lists" {
    try testNoErrors(
        \\script s@1 {
        \\    global_var = pick 0 [
        \\        1 2 3
        \\        4 5 6
        \\    ]
        \\}
    );
}

test "verify MAXS against symbol numbers" {
    try testErrors(&.{
        .{
            .filename = "project.scu",
            .source =
            \\target sputm90
            \\index {
            \\    maxs "index_MAXS.bin"
            \\    ^ MAXS constrains global variables (count=500) below project usage (max=500)
            \\    ^ MAXS constrains room variables (count=64) below project usage (max=70)
            \\    ^ MAXS constrains local scripts (count=256) below project usage (max=456)
            \\}
            \\disk 1 {
            \\    room 1 "room" "room.scu"
            \\}
            \\var gv@500
            ,
        },
        .{
            .filename = "index_MAXS.bin",
            .binary = baseball1997_maxs,
        },
        .{
            .filename = "room.scu",
            .source =
            \\var rv@70
            \\local-script s@456 {}
            ,
        },
    });
}

/// This is a copy of baseball1997's index_MAXS.bin
const baseball1997_maxs: []const u8 = &.{
    0xf4, 0x01, 0x10, 0x00, 0x40, 0x00, 0xc8, 0x00, 0x64, 0x00, 0x00, 0x00, 0x64, 0x00, 0x20, 0x00,
    0x50, 0x00, 0x1f, 0x00, 0xc5, 0x00, 0x10, 0x07, 0x0c, 0x00, 0x97, 0x01, 0xe2, 0x04, 0x0b, 0x05,
    0x00, 0x01, 0x00, 0x01, 0x00, 0x0c,
};

fn testProjectError(case_str: []const u8) !void {
    try testErrors(&.{
        .{ .filename = "project.scu", .source = case_str },
    });
}

fn testRoomError(case_str: []const u8) !void {
    const project_scu =
        \\target sputm99
        \\index {}
        \\disk 1 {
        \\    room 1 "room" "room.scu"
        \\}
        \\var global_var@0
        \\
    ;
    try testErrors(&.{
        .{ .filename = "project.scu", .binary = project_scu },
        .{ .filename = "room.scu", .source = case_str },
    });
}

const TestFile = struct {
    filename: [:0]const u8,
    // Exactly one of these must be given:
    source: ?[]const u8 = null,
    binary: ?[]const u8 = null,
};

fn testErrors(inputs: []const TestFile) !void {
    const gpa = std.testing.allocator;
    var arena_state: std.heap.ArenaAllocator = .init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const in_path = "/tmp/baller-test-in";
    const build_path = "/tmp/baller-test-build";

    try std.fs.cwd().deleteTree(in_path);
    try std.fs.cwd().deleteTree(build_path);

    try fs.makeDirIfNotExistZ(std.fs.cwd(), in_path);
    var in_dir = try std.fs.cwd().openDirZ(in_path, .{});
    defer in_dir.close();

    var expected_messages: std.ArrayList([]const u8) = .empty;
    for (inputs) |input| {
        std.debug.assert(@intFromBool(input.source != null) + @intFromBool(input.binary != null) == 1);
        if (input.source) |source| {
            const data = try parseSourceAndErrors(arena, input.filename, source, &expected_messages);
            try fs.writeFileZ(in_dir, input.filename, data.items);
        } else if (input.binary) |data| {
            try fs.writeFileZ(in_dir, input.filename, data);
        } else unreachable;
    }

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    const result = build.run(gpa, &diagnostic, .{
        .project_path = in_path ++ "/project.scu",
        .index_path = build_path ++ "/baseball 2001.he0",
        .options = .{
            .awiz_strategy = .max,
            .write_version = true,
        },
    });
    if (result) |_| {} else |err| try std.testing.expect(err == error.AddedToDiagnostic);

    try std.testing.expect(expected_messages.items.len != 0);
    try std.testing.expectEqual(diagnostic.messages.len, expected_messages.items.len);
    var it = diagnostic.messages.iterator(0);
    for (expected_messages.items) |expected_message| {
        const actual_message = it.next().?;
        try std.testing.expectEqual(actual_message.level, .err);
        try std.testing.expectEqualStrings(actual_message.text, expected_message);
    }
}

fn parseSourceAndErrors(
    gpa: std.mem.Allocator,
    filename: []const u8,
    combined: []const u8,
    messages: *std.ArrayList([]const u8),
) !std.ArrayList(u8) {
    var source: std.ArrayList(u8) = try .initCapacity(gpa, combined.len);
    errdefer source.deinit(gpa);

    var line_number: usize = 0;
    var it = std.mem.splitScalar(u8, combined, '\n');
    while (it.next()) |line| : (line_number += 1) {
        // For each line, check if it matches the pattern of a message. If not,
        // append it to the source lines.
        msg: {
            const caret_pos = std.mem.indexOfNone(u8, line, " ") orelse break :msg;
            if (line[caret_pos] != '^') break :msg;
            const text_pos = std.mem.indexOfNonePos(u8, line, caret_pos + 1, " ") orelse break :msg;

            const message = try std.fmt.allocPrint(
                gpa,
                "{s}:{}:{}: {s}",
                .{ filename, line_number, caret_pos + 1, line[text_pos..] },
            );
            errdefer gpa.free(message);
            try messages.append(gpa, message);
            line_number -= 1; // follow output line numbers, not input
            continue;
        }

        try source.appendSlice(gpa, line);
        try source.append(gpa, '\n');
    }

    return source;
}

fn testNoErrors(room_scu: []const u8) !void {
    const gpa = std.testing.allocator;

    const in_path = "/tmp/baller-test-in";
    const build_path = "/tmp/baller-test-build";

    try std.fs.cwd().deleteTree(in_path);
    try std.fs.cwd().deleteTree(build_path);

    try fs.makeDirIfNotExistZ(std.fs.cwd(), in_path);
    var in_dir = try std.fs.cwd().openDirZ(in_path, .{});
    defer in_dir.close();

    const project_scu =
        \\target sputm99
        \\index {}
        \\disk 1 {
        \\    room 1 "room" "room.scu"
        \\}
        \\var global_var@0
        \\var g1@1
        \\var g2@2
        \\var g3@3
        \\
    ;
    try fs.writeFileZ(in_dir, "project.scu", project_scu);

    try fs.writeFileZ(in_dir, "room.scu", room_scu);

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    _ = try build.run(gpa, &diagnostic, .{
        .project_path = in_path ++ "/project.scu",
        .index_path = build_path ++ "/baseball 2001.he0",
        .options = .{
            .awiz_strategy = .max,
            .write_version = true,
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}
