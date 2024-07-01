const std = @import("std");

const test_options = @import("test_options");

// Extract and rebuild, and verify the output is identical to the original.
test "round trip" {
    const allocator = std.testing.allocator;

    const extract_dir = "/tmp/baller-test-baseball-2001-extract";
    const build_dir = "/tmp/baller-test-baseball-2001-build";

    var extract = std.process.Child.init(&.{
        test_options.exe_path,
        "extract",
        "src/fixtures/baseball2001/baseball 2001.he0",
        extract_dir,
    }, allocator);
    const extract_term = try extract.spawnAndWait();
    if (!(extract_term == .Exited and extract_term.Exited == 0))
        return error.ExitStatus;

    var build = std.process.Child.init(&.{
        test_options.exe_path,
        "build",
        extract_dir ++ "/project.txt",
        build_dir ++ "/baseball 2001.he0",
    }, allocator);
    const build_term = try build.spawnAndWait();
    if (!(build_term == .Exited and build_term.Exited == 0))
        return error.ExitStatus;

    try expectFilesEqual(
        "src/fixtures/baseball2001/baseball 2001.he0",
        build_dir ++ "/baseball 2001.he0",
    );
    try expectFilesEqual(
        "src/fixtures/baseball2001/baseball 2001.(a)",
        build_dir ++ "/baseball 2001.(a)",
    );
    try expectFilesEqual(
        "src/fixtures/baseball2001/baseball 2001.(b)",
        build_dir ++ "/baseball 2001.(b)",
    );
}

fn expectFilesEqual(path1: [*:0]const u8, path2: [*:0]const u8) !void {
    if (!try filesEqual(path1, path2))
        return error.TestExpectedEqual;
}

fn filesEqual(path1: [*:0]const u8, path2: [*:0]const u8) !bool {
    const file1 = try std.fs.cwd().openFileZ(path1, .{});
    defer file1.close();

    const file2 = try std.fs.cwd().openFileZ(path2, .{});
    defer file2.close();

    return streamsEqual(file1.reader(), file2.reader());
}

fn streamsEqual(s1: anytype, s2: anytype) !bool {
    var buf1: [4096]u8 = undefined;
    var buf2: [4096]u8 = undefined;
    while (true) {
        const len = try s1.read(&buf1);
        if (len == 0)
            return try s2.read(&buf2) == 0;

        try s2.readNoEof(buf2[0..len]);
        if (!std.mem.eql(u8, buf1[0..len], buf2[0..len]))
            return false;
    }
}
