const std = @import("std");

const build = @import("build.zig");
const extract = @import("extract.zig");

// Extract and rebuild, and verify the output is identical to the original.
test "Backyard Baseball 2001 round trip raw" {
    const allocator = std.testing.allocator;

    const extract_dir = "/tmp/baller-test-baseball-2001-extract";
    const build_dir = "/tmp/baller-test-baseball-2001-build";

    try extract.run(allocator, &.{
        .input_path = "src/fixtures/baseball2001/baseball 2001.he0",
        .output_path = extract_dir,
        .raw = true,
    });

    try build.run(allocator, &.{
        .project_txt_path = extract_dir ++ "/project.txt",
        .output_path = build_dir ++ "/baseball 2001.he0",
    });

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

test "Backyard Baseball 2001 round trip decode/encode" {
    const allocator = std.testing.allocator;

    const extract_dir = "/tmp/baller-test-baseball-2001-extract";
    const build_dir = "/tmp/baller-test-baseball-2001-build";

    try extract.run(allocator, &.{
        .input_path = "src/fixtures/baseball2001/baseball 2001.he0",
        .output_path = extract_dir,
        .raw = false,
    });

    try build.run(allocator, &.{
        .project_txt_path = extract_dir ++ "/project.txt",
        .output_path = build_dir ++ "/baseball 2001.he0",
    });

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
