const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const build = @import("build.zig");
const extract = @import("extract.zig");
const fs = @import("fs.zig");
const fixture_hashes = @import("tests.zig").fixture_hashes;

// Extract and rebuild every supported game, and verify the output is identical
// to the original.

const Game = struct {
    fixture_dir: []const u8,
    index_name: [:0]const u8,
    fixture_names: []const [:0]const u8,
};

const baseball1997: Game = .{
    .fixture_dir = "baseball1997",
    .index_name = "BASEBALL.HE0",
    .fixture_names = &.{"BASEBALL.HE1"},
};
test "Backyard Baseball 1997 round trip raw" {
    try testRoundTrip(baseball1997, .raw);
}
test "Backyard Baseball 1997 round trip decode" {
    try testRoundTrip(baseball1997, .decode);
}

const soccer: Game = .{
    .fixture_dir = "soccer",
    .index_name = "SOCCER.HE0",
    .fixture_names = &.{"SOCCER.(A)"},
};
test "Backyard Soccer round trip raw" {
    try testRoundTrip(soccer, .raw);
}
test "Backyard Soccer round trip decode" {
    try testRoundTrip(soccer, .decode);
}

const football: Game = .{
    .fixture_dir = "football",
    .index_name = "FOOTBALL.HE0",
    .fixture_names = &.{ "FOOTBALL.(A)", "FOOTBALL.(B)" },
};
test "Backyard Football round trip raw" {
    try testRoundTrip(football, .raw);
}
test "Backyard Football round trip decode" {
    try testRoundTrip(football, .decode);
}

const baseball2001: Game = .{
    .fixture_dir = "baseball2001",
    .index_name = "baseball 2001.he0",
    .fixture_names = &.{ "baseball 2001.(a)", "baseball 2001.(b)" },
};
test "Backyard Baseball 2001 round trip raw" {
    try testRoundTrip(baseball2001, .raw);
}
test "Backyard Baseball 2001 round trip decode" {
    try testRoundTrip(baseball2001, .decode);
}

const basketball: Game = .{
    .fixture_dir = "basketball",
    .index_name = "Basketball.he0",
    .fixture_names = &.{ "Basketball.(a)", "Basketball.(b)" },
};
test "Backyard Basketball round trip raw" {
    try testRoundTrip(basketball, .raw);
}
test "Backyard Basketball round trip decode" {
    try testRoundTrip(basketball, .decode);
}

fn testRoundTrip(comptime game: Game, options: enum { raw, decode }) !void {
    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();

    const extract_path = "/tmp/" ++ game.fixture_dir;
    const build_path = extract_path ++ "build";

    try extract.run(std.testing.allocator, &diagnostic, .{
        .index_path = "src/fixtures/" ++ game.fixture_dir ++ "/" ++ game.index_name,
        .output_path = extract_path,
        .options = switch (options) {
            .raw => .{
                .rmim = .raw,
                .scrp = .raw,
                .encd = .raw,
                .excd = .raw,
                .lsc2 = .raw,
                .awiz = .raw,
                .mult = .raw,
                .akos = .raw,
            },
            .decode => .{
                .rmim = .decode,
                .scrp = .decode,
                .encd = .decode,
                .excd = .decode,
                .lsc2 = .decode,
                .awiz = .decode,
                .mult = .decode,
                .akos = .decode,
            },
        },
    });

    try build.run(std.testing.allocator, &diagnostic, .{
        .project_path = extract_path ++ "/project.scu",
        .index_path = build_path ++ "/" ++ game.index_name,
        .options = .{
            .awiz_strategy = .original,
        },
    });

    var output_dir = try std.fs.cwd().openDirZ(build_path, .{});
    defer output_dir.close();

    inline for (.{game.index_name} ++ game.fixture_names) |name| {
        errdefer std.debug.print("{s}\n", .{name});
        const expected_hex = @field(fixture_hashes, game.fixture_dir ++ "/" ++ name);
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

    if (!std.mem.eql(u8, &actual_hash, &expected_hash))
        return error.TestExpectedEqual;
}
