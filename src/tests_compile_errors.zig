const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const build = @import("build.zig");
const fs = @import("fs.zig");

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
        \\^ invalid character in string
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
        \\    sprite-select 0 0
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

fn testRoomError(case_str: []const u8) !void {
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
        \\
    ;
    try fs.writeFileZ(in_dir, "project.scu", project_scu);

    var case = try parseSourceAndError(gpa, case_str);
    defer case.source.deinit(gpa);
    defer case.message.deinit(gpa);

    try fs.writeFileZ(in_dir, "room.scu", case.source.items);

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
    try std.testing.expect(diagnostic.messages.len != 0);

    const message = diagnostic.messages.at(0);
    try std.testing.expectEqual(message.level, .err);
    try std.testing.expectEqualStrings(message.text, case.message.items);
}

fn parseSourceAndError(gpa: std.mem.Allocator, combined: []const u8) !struct {
    source: std.ArrayListUnmanaged(u8),
    message: std.ArrayListUnmanaged(u8),
} {
    var source: std.ArrayListUnmanaged(u8) = try .initCapacity(gpa, combined.len);
    errdefer source.deinit(gpa);

    var message: std.ArrayListUnmanaged(u8) = try .initCapacity(gpa, combined.len);
    errdefer message.deinit(gpa);

    var line_number: usize = 0;
    var it = std.mem.splitScalar(u8, combined, '\n');
    while (it.next()) |line| : (line_number += 1) {
        // For each line, check if it matches the pattern of a message. If not,
        // append it to the source lines.
        msg: {
            const caret_pos = std.mem.indexOfNone(u8, line, " ") orelse break :msg;
            if (line[caret_pos] != '^') break :msg;
            const text_pos = std.mem.indexOfNonePos(u8, line, caret_pos + 1, " ") orelse break :msg;

            std.debug.assert(message.items.len == 0);
            try message.writer(gpa).print(
                "room.scu:{}:{}: {s}",
                .{ line_number, caret_pos + 1, line[text_pos..] },
            );
            continue;
        }

        try source.appendSlice(gpa, line);
        try source.append(gpa, '\n');
    }

    return .{ .source = source, .message = message };
}
