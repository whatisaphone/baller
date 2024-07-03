const std = @import("std");

const build = @import("build.zig");
const extract = @import("extract.zig");
const io = @import("io.zig");

// Extract and rebuild every supported game, and verify the output is identical
// to the original.

test "Backyard Baseball 1997 fixture integrity" {
    try expectFileHashEquals(
        "src/fixtures/baseball1997/BASEBALL.HE0",
        "6b701f415251a1d25fe91fb28c78193ac01b382b4292c377b0fd01e30a61c5da",
    );
    try expectFileHashEquals(
        "src/fixtures/baseball1997/BASEBALL.HE1",
        "f3e910f120433d318e30dae75e0fbd418f08338966cbf129487909e1f3cf5cd8",
    );
}

test "Backyard Baseball 1997 round trip raw" {
    const allocator = std.testing.allocator;

    const extract_dir = "/tmp/baller-test-baseball-1997-extract";
    const build_dir = "/tmp/baller-test-baseball-1997-build";

    try extract.run(allocator, &.{
        .input_path = "src/fixtures/baseball1997/BASEBALL.HE0",
        .output_path = extract_dir,
        .raw = true,
    });

    try build.run(allocator, &.{
        .project_txt_path = extract_dir ++ "/project.txt",
        .output_path = build_dir ++ "/BASEBALL.HE0",
    });

    try expectFilesEqual(
        "src/fixtures/baseball1997/BASEBALL.HE0",
        build_dir ++ "/BASEBALL.HE0",
    );
    try expectFilesEqual(
        "src/fixtures/baseball1997/BASEBALL.HE1",
        build_dir ++ "/BASEBALL.HE1",
    );
}

test "Backyard Baseball 1997 round trip decode/encode" {
    const allocator = std.testing.allocator;

    const extract_dir = "/tmp/baller-test-baseball-1997-extract";
    const build_dir = "/tmp/baller-test-baseball-1997-build";

    try extract.run(allocator, &.{
        .input_path = "src/fixtures/baseball1997/BASEBALL.HE0",
        .output_path = extract_dir,
        .raw = false,
    });

    try build.run(allocator, &.{
        .project_txt_path = extract_dir ++ "/project.txt",
        .output_path = build_dir ++ "/BASEBALL.HE0",
    });

    try expectFilesEqual(
        "src/fixtures/baseball1997/BASEBALL.HE0",
        build_dir ++ "/BASEBALL.HE0",
    );
    try expectFilesEqual(
        "src/fixtures/baseball1997/BASEBALL.HE1",
        build_dir ++ "/BASEBALL.HE1",
    );
}

test "Backyard Baseball 2001 fixture integrity" {
    try expectFileHashEquals(
        "src/fixtures/baseball2001/baseball 2001.he0",
        "c89d5c17c58db55652b31828a4b27edfa9810dbfddcada428b8b2bf5fb85a5b9",
    );
    try expectFileHashEquals(
        "src/fixtures/baseball2001/baseball 2001.(a)",
        "a2bd2d171c47a320fe31dd2e40cfcbecae01d46c5ecebc362fc998e4f0cb73ff",
    );
    try expectFileHashEquals(
        "src/fixtures/baseball2001/baseball 2001.(b)",
        "dde0397d5c658f2acffdebb5b58b0d2770707619092a4319354b37512b513038",
    );
}

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

fn expectFileHashEquals(path: [*:0]const u8, comptime expected_hex: *const [64]u8) !void {
    var expected_hash: [32]u8 = undefined;
    _ = try std.fmt.hexToBytes(&expected_hash, expected_hex);

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    const file = try std.fs.cwd().openFileZ(path, .{});
    defer file.close();

    try io.copy(file, hasher.writer());

    const actual_hash = hasher.finalResult();
    try std.testing.expectEqualSlices(u8, &actual_hash, &expected_hash);
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
