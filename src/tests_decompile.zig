const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const beginBlockAl = @import("block_writer.zig").beginBlockAl;
const endBlockAl = @import("block_writer.zig").endBlockAl;
const build = @import("build.zig");
const extract = @import("extract.zig");
const fs = @import("fs.zig");
const utils = @import("utils.zig");

test "global var auto name" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v = 0
        \\}
    ,
        \\
    ,
        \\script scr1@1 {
        \\    global0 = 0
        \\}
    );
}

test "global var with symbol" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v = 0
        \\}
    ,
        \\global.0 = foo
    ,
        \\script scr1@1 {
        \\    foo = 0
        \\}
    );
}

test "room var auto name" {
    try testRoundTrip(
        \\
    ,
        \\var v@0
        \\
        \\script s@1 {
        \\    v = 0
        \\}
    ,
        \\
    ,
        \\var room0@0
        \\
        \\script scr1@1 {
        \\    room0 = 0
        \\}
    );
}

test "room var with symbol" {
    try testRoundTrip(
        \\
    ,
        \\var v@0
        \\
        \\script s@1 {
        \\    v = 0
        \\}
    ,
        \\room.1.var.0 = foo
    ,
        \\var foo@0
        \\
        \\script scr1@1 {
        \\    foo = 0
        \\}
    );
}

test "type array index" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0] = 0
        \\    return (v[0] == 0)
        \\}
    ,
        \\enum.Index.0 = INDEX
        \\global.0 = foo:[Index]
    ,
        \\script scr1@1 {
        \\    foo[INDEX] = 0
        \\    return (foo[INDEX] == 0)
        \\}
    );
}

test "type array value" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0] = 0
        \\    return (v[0] == 0)
        \\}
    ,
        \\enum.Value.0 = VALUE
        \\global.0 = foo:[]Value
    ,
        \\script scr1@1 {
        \\    foo[0] = VALUE
        \\    return (foo[0] == 0)
        // TODO: return (foo[0] == VALUE)
        \\}
    );
}

test "type array all" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0] = 0
        \\    return (v[0] == 0)
        \\}
    ,
        \\enum.Index.0 = INDEX
        \\enum.Value.0 = VALUE
        \\global.0 = foo:[Index]Value
    ,
        \\script scr1@1 {
        \\    foo[INDEX] = VALUE
        \\    return (foo[INDEX] == 0)
        // TODO: return (foo[INDEX] == VALUE)
        \\}
    );
}

test "type 2d array row" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0][0] = 0
        \\    return (v[0][0] == 0)
        \\}
    ,
        \\enum.Row.0 = ROW
        \\global.0 = foo:[Row][]
    ,
        \\script scr1@1 {
        \\    foo[ROW][0] = 0
        \\    return (foo[ROW][0] == 0)
        \\}
    );
}

test "type 2d array column" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0][0] = 0
        \\    return (v[0][0] == 0)
        \\}
    ,
        \\enum.Col.0 = COL
        \\global.0 = foo:[][Col]
    ,
        \\script scr1@1 {
        \\    foo[0][COL] = 0
        \\    return (foo[0][COL] == 0)
        \\}
    );
}

test "type 2d array value" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0][0] = 0
        \\    return (v[0][0] == 0)
        \\}
    ,
        \\enum.Value.0 = VALUE
        \\global.0 = foo:[][]Value
    ,
        \\script scr1@1 {
        \\    foo[0][0] = VALUE
        \\    return (foo[0][0] == 0)
        // TODO: return (foo[0][0] == VALUE)
        \\}
    );
}

test "type 2d array all" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v[0][0] = 0
        \\    return (v[0][0] == 0)
        \\}
    ,
        \\enum.Row.0 = ROW
        \\enum.Col.0 = COL
        \\enum.Value.0 = VALUE
        \\global.0 = foo:[Row][Col]Value
    ,
        \\script scr1@1 {
        \\    foo[ROW][COL] = VALUE
        \\    return (foo[ROW][COL] == 0)
        // TODO: return (foo[ROW][COL] == VALUE)
        \\}
    );
}

test "type script args" {
    try testRoundTrip(
        \\
    ,
        \\script s@1 {
        \\    start-script func 0 0
        \\}
        \\
        \\script func@2 {
        \\}
    ,
        \\enum.Foo.0 = FOO
        \\enum.Bar.0 = BAR
        \\script.2 = f(foo:Foo bar:Bar)
    ,
        \\script scr1@1 {
        \\    start-script f FOO BAR
        \\}
        \\
        \\script f@2 {
        \\}
    );
}

test "type room" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    current-room 1
        \\    v = 1
        \\}
    ,
        \\global.0 = foo:Room
    ,
        \\script scr1@1 {
        \\    current-room test-room
        \\    foo = test-room
        \\}
    );
}

test "open-file consts" {
    try testRoundTrip(
        \\var v@0
    ,
        \\script s@1 {
        \\    v = open-file "" 1
        \\}
    ,
        \\
    ,
        \\script scr1@1 {
        \\    global0 = open-file "" FOR-READ
        \\}
    );
}

test "saveload consts" {
    try testRoundTrip(
        \\
    ,
        \\script s@1 {
        \\    saveload-game 1 ""
        \\}
    ,
        \\
    ,
        \\script scr1@1 {
        \\    saveload-game SAVE ""
        \\}
    );
}

/// Compile a project and decompile, then check that the decompilation matches
/// the expected output.
fn testRoundTrip(
    comptime build_project_scu: []const u8,
    comptime build_room_scu: []const u8,
    extract_symbols_ini: []const u8,
    comptime expected_room_scu: []const u8,
) !void {
    const gpa = std.testing.allocator;

    const in_path = "/tmp/baller-test-in";
    const build_path = "/tmp/baller-test-build";
    const out_path = "/tmp/baller-test-out";

    // Write project

    try fs.makeDirIfNotExistZ(std.fs.cwd(), in_path);
    var in_dir = try std.fs.cwd().openDirZ(in_path, .{});
    defer in_dir.close();

    const full_build_project_scu =
        \\target sputm99
        \\index {
        \\    maxs "index_MAXS.bin"
        \\    index-block DIRI
        \\    index-block DIRR
        \\    index-block DIRS
        \\    index-block DIRN
        \\    index-block DIRC
        \\    index-block DIRF
        \\    index-block DIRM
        \\    index-block DIRT
        \\    index-block DLFL
        \\    index-block DISK
        \\    index-block RNAM
        \\    raw-block DOBJ "empty.bin"
        \\    raw-block AARY "empty.bin"
        \\    raw-block INIB "empty.bin"
        \\}
        \\disk 1 {
        \\    room 1 "test-room" "test-room.scu"
        \\}
        \\
    ++ build_project_scu ++ "\n";
    try fs.writeFileZ(in_dir, "project.scu", full_build_project_scu);
    try fs.writeFileZ(in_dir, "index_MAXS.bin", @as(*const [44]u8, &@splat(0)));
    try fs.writeFileZ(in_dir, "empty.bin", &.{});

    const full_build_room_scu =
        \\raw-glob RMIM 1 "empty.bin"
        \\rmda {
        \\    raw-block PALS "pals.bin"
        \\}
        \\
    ++ build_room_scu;
    try fs.writeFileZ(in_dir, "test-room.scu", full_build_room_scu);
    try fs.writeFileZ(in_dir, "pals.bin", &buildDummyPals());

    // Compile

    var diagnostic: Diagnostic = .init(gpa);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    try build.run(gpa, &diagnostic, .{
        .project_path = in_path ++ "/project.scu",
        .index_path = build_path ++ "/baseball 2001.he0",
        .options = .{
            .awiz_strategy = .max,
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();

    // Decompile

    diagnostic.deinit();
    diagnostic = .init(gpa);

    try fs.writeFileZ(in_dir, "symbols.ini", extract_symbols_ini);

    _ = try extract.run(gpa, &diagnostic, .{
        .index_path = build_path ++ "/baseball 2001.he0",
        .output_path = out_path,
        .symbols_path = in_path ++ "/symbols.ini",
        .options = .{
            .script = .decompile,
            .annotate = false,
            .rmim = .decode,
            .scrp = .decode,
            .encd = .decode,
            .excd = .decode,
            .lscr = .decode,
            .lsc2 = .decode,
            .obim = .decode,
            .obcd = .decode,
            .digi = .decode,
            .talk = .decode,
            .awiz = .decode,
            .mult = .decode,
            .akos = .decode,
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();

    // Compare output against expected

    const actual_room_scu = try fs.readFileZ(gpa, std.fs.cwd(), out_path ++ "/test-room.scu");
    defer gpa.free(actual_room_scu);

    const full_expected_room_scu =
        \\raw-glob RMIM 1 "test-room/RMIM_0001.bin"
        \\rmda {
        \\    raw-block PALS "test-room/PALS.bin"
        \\}
        \\
        \\
    ++ expected_room_scu ++ "\n\n\n";

    try std.testing.expectEqualStrings(actual_room_scu, full_expected_room_scu);
}

fn buildDummyPals() [796]u8 {
    const na = utils.null_allocator;

    var result: [796]u8 = undefined;
    var out: std.ArrayListUnmanaged(u8) = .initBuffer(&result);

    const wrap_start = beginBlockAl(na, &out, .WRAP) catch unreachable;

    const offs_start = beginBlockAl(na, &out, .OFFS) catch unreachable;
    out.writer(na).writeInt(u32, 12, .little) catch unreachable;
    endBlockAl(&out, offs_start);

    const apal_start = beginBlockAl(na, &out, .APAL) catch unreachable;
    out.appendNTimes(na, 0, 0x300) catch unreachable;
    endBlockAl(&out, apal_start);

    endBlockAl(&out, wrap_start);

    std.debug.assert(out.items.len == out.capacity);
    return result;
}
