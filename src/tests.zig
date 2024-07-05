const std = @import("std");

const build = @import("build.zig");
const extract = @import("extract.zig");
const io = @import("io.zig");

// Extract and rebuild every supported game, and verify the output is identical
// to the original.

const fixture_hashes = .{
    .@"baseball1997/BASEBALL.HE0" = "6b701f415251a1d25fe91fb28c78193ac01b382b4292c377b0fd01e30a61c5da",
    .@"baseball1997/BASEBALL.HE1" = "f3e910f120433d318e30dae75e0fbd418f08338966cbf129487909e1f3cf5cd8",
    .@"baseball2001/baseball 2001.he0" = "c89d5c17c58db55652b31828a4b27edfa9810dbfddcada428b8b2bf5fb85a5b9",
    .@"baseball2001/baseball 2001.(a)" = "a2bd2d171c47a320fe31dd2e40cfcbecae01d46c5ecebc362fc998e4f0cb73ff",
    .@"baseball2001/baseball 2001.(b)" = "dde0397d5c658f2acffdebb5b58b0d2770707619092a4319354b37512b513038",
};

test "fixture integrity" {
    inline for (std.meta.fields(@TypeOf(fixture_hashes))) |field| {
        const path = "src/fixtures/" ++ field.name;
        const expected_hash_hex = @field(fixture_hashes, field.name);
        try expectFileHashEquals(path, expected_hash_hex);
    }
}

test "Backyard Baseball 1997 round trip raw" {
    try testRoundTrip("baseball1997", "BASEBALL.HE0", true, &.{
        "BASEBALL.HE0",
        "BASEBALL.HE1",
    });
}

test "Backyard Baseball 1997 round trip decode/encode" {
    try testRoundTrip("baseball1997", "BASEBALL.HE0", false, &.{
        "BASEBALL.HE0",
        "BASEBALL.HE1",
    });
}

test "Backyard Baseball 2001 round trip raw" {
    try testRoundTrip("baseball2001", "baseball 2001.he0", true, &.{
        "baseball 2001.he0",
        "baseball 2001.(a)",
        "baseball 2001.(b)",
    });
}

test "Backyard Baseball 2001 round trip decode/encode" {
    try testRoundTrip("baseball2001", "baseball 2001.he0", false, &.{
        "baseball 2001.he0",
        "baseball 2001.(a)",
        "baseball 2001.(b)",
    });
}

fn testRoundTrip(
    comptime fixture_dir: []const u8,
    comptime index_name: []const u8,
    raw: bool,
    comptime fixture_names: []const []const u8,
) !void {
    const allocator = std.testing.allocator;

    const extract_dir = "/tmp/baller-test-extract";
    const build_dir = "/tmp/baller-build";

    // best effort cleanup
    defer std.fs.cwd().deleteTree(extract_dir) catch {};
    defer std.fs.cwd().deleteTree(build_dir) catch {};

    try extract.run(allocator, &.{
        .input_path = "src/fixtures/" ++ fixture_dir ++ "/" ++ index_name,
        .output_path = extract_dir,
        .raw = raw,
    });

    try build.run(allocator, &.{
        .project_txt_path = extract_dir ++ "/project.txt",
        .output_path = build_dir ++ "/" ++ index_name,
    });

    inline for (fixture_names) |name| {
        try expectFileHashEquals(
            build_dir ++ "/" ++ name,
            @field(fixture_hashes, fixture_dir ++ "/" ++ name),
        );
    }
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
