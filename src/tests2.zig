const std = @import("std");

const build2 = @import("build2.zig");
const extract2 = @import("extract2.zig");
const fs = @import("fs.zig");
const fixture_hashes = @import("tests.zig").fixture_hashes;

// Extract and rebuild every supported game, and verify the output is identical
// to the original.

test "Backyard Baseball 2001 round trip" {
    try extract2.run(std.testing.allocator, .{
        .index_path = "src/fixtures/baseball2001/baseball 2001.he0",
        .output_path = "/tmp/bb2001",
        .options = .{
            .awiz = .decode,
        },
    });

    try build2.run(std.testing.allocator, .{
        .project_path = "/tmp/bb2001/project.scu",
        .index_path = "/tmp/bb2001build/baseball 2001.he0",
        .options = .{
            .awiz_strategy = .original,
        },
    });

    var output_dir = try std.fs.cwd().openDirZ("/tmp/bb2001build", .{});
    defer output_dir.close();

    inline for (.{ "baseball 2001.he0", "baseball 2001.(a)", "baseball 2001.(b)" }) |name| {
        const expected_hex = @field(fixture_hashes, "baseball2001/" ++ name);
        try expectFileHashEquals(output_dir, name, expected_hex);
    }
}

fn expectFileHashEquals(
    dir: std.fs.Dir,
    path: [*:0]const u8,
    comptime expected_hex: *const [64]u8,
) !void {
    var expected_hash: [32]u8 = undefined;
    _ = try std.fmt.hexToBytes(&expected_hash, expected_hex);

    var hasher: std.crypto.hash.sha2.Sha256 = .init(.{});
    try fs.readFileIntoZ(dir, path, hasher.writer());
    const actual_hash = hasher.finalResult();

    try std.testing.expectEqualSlices(u8, &actual_hash, &expected_hash);
}
