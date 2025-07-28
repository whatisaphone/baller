const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const fs = @import("fs.zig");
const talkie_build = @import("talkie_build.zig");
const talkie_extract = @import("talkie_extract.zig");

// Extract and rebuild every supported game, and verify the output is identical
// to the original.

pub const fixture_hashes = .{
    .@"baseball1997/BASEBALL.HE0" = "6b701f415251a1d25fe91fb28c78193ac01b382b4292c377b0fd01e30a61c5da",
    .@"baseball1997/BASEBALL.HE1" = "f3e910f120433d318e30dae75e0fbd418f08338966cbf129487909e1f3cf5cd8",
    .@"baseball1997/BASEBALL.HE2" = "216db30c5810b063aaf718defdf67ee11c2d6aaa7bf2cf64c7101e507f65647a",
    .@"baseball1997/BASEBALL.HE4" = "3c8e3ac672454a52d9b1d5b2bc64f5614200ef22952d679ee539a44c306351e7",
    .@"baseball2001/baseball 2001.he0" = "c89d5c17c58db55652b31828a4b27edfa9810dbfddcada428b8b2bf5fb85a5b9",
    .@"baseball2001/baseball 2001.(a)" = "a2bd2d171c47a320fe31dd2e40cfcbecae01d46c5ecebc362fc998e4f0cb73ff",
    .@"baseball2001/baseball 2001.(b)" = "dde0397d5c658f2acffdebb5b58b0d2770707619092a4319354b37512b513038",
    .@"baseball2001/baseball 2001.he2" = "583be6ee0ad30bdd1d4a78ddc006155d77f567a6c5467b22d8871c137e974927",
    .@"baseball2001/baseball 2001.he4" = "dfcb1bfb07cfeb2fb37d3feb99a0b3e19b44856921da58c8b3e784bb4b082a34",
    .@"soccer/SOCCER.HE0" = "44d5de043628bbe5db166f00e8cb9b0398800dc30b0868ae0ce4c8eba96ec9f9",
    .@"soccer/SOCCER.(A)" = "8cdd016013493bbc22bda8e3fa3d62ece7a61e4ee346fd85d7288e3de385bf79",
    .@"soccer/SOCCER.HE2" = "2e5cd139e44274652d94ed1af09995f20426be4741d13abb4c9075356bdbe073",
    .@"soccer/SOCCER.HE4" = "e312bd96d2f5daeaa1b8dbefac6e4811a0a6bfea599a8dce070071c78dfd7a45",
    .@"football/FOOTBALL.HE0" = "5fbec205047ce8da3d42b2c48652183ef30bcf0d3e1aa233cda9745317bbb59d",
    .@"football/FOOTBALL.(A)" = "4d406cbe8243f84bac39a743495b6377879afc9d7a187e203d7f58cd8bc59742",
    .@"football/FOOTBALL.(B)" = "42cf15534b9a707ec1cec54a09d30211da07b226a084e802c3b049701751a2f1",
    .@"football/FOOTBALL.HE2" = "fc5ab3c7d917eaaa9cb2e7cee6ca73bef0958d939e6349498bb17ae752f691ef",
    .@"football/football.he4" = "e6f0f9a41d4e96b3a67d74bbe338ff847446d6af75235717b48380a67cd4db7e",
    .@"basketball/Basketball.he0" = "9a767514bc5bf00743648111d6bf31e886e8f4fc8401be30447ee69126c997b2",
    .@"basketball/Basketball.(a)" = "b90c8b7c4d906d568647cd049d15b0c360a5945418c2807fcd22dcdc64decb84",
    .@"basketball/Basketball.(b)" = "7551d6465bd4cc8e0c0b97357ca28825d18bb2d645ccf30c5f56a810abd62e4c",
    .@"basketball/Basketball.he2" = "d5c7cba5ecf5849452b45f781e700781d3c98da242d865337e0d964c21f31a97",
    .@"basketball/Basketball.he4" = "6308b5070a2308ec5ebba62cc3d2ddc2a8ae2bc5eac56624178351d4f961ace1",
};

test "fixture integrity" {
    inline for (std.meta.fields(@TypeOf(fixture_hashes))) |field| {
        const path = "src/fixtures/" ++ field.name;
        const expected_hash_hex = @field(fixture_hashes, field.name);
        try expectFileHashEquals(path, expected_hash_hex);
    }
}

test "Backyard Baseball 1997 talkies round trip" {
    try testRoundTripTalkies("baseball1997", "BASEBALL.HE2");
}

test "Backyard Baseball 2001 talkies round trip" {
    try testRoundTripTalkies("baseball2001", "baseball 2001.he2");
}

test "Backyard Soccer talkies round trip" {
    try testRoundTripTalkies("soccer", "SOCCER.HE2");
}

test "Backyard Football talkies round trip" {
    try testRoundTripTalkies("football", "FOOTBALL.HE2");
}

test "Backyard Basketball talkies round trip" {
    try testRoundTripTalkies("basketball", "Basketball.he2");
}

fn testRoundTripTalkies(
    comptime fixture_dir: []const u8,
    comptime fixture_name: []const u8,
) !void {
    const allocator = std.testing.allocator;

    // Build temp dirs for this test

    const tmp = try getTempDir(allocator);
    defer freeTempDir(allocator, tmp);

    const extract_dir = try concatZ(allocator, &.{ tmp, "/baller-test-extract" });
    defer allocator.free(extract_dir);

    const output_path = try concatZ(allocator, &.{ tmp, "/baller-test-build.he2" });
    defer allocator.free(output_path);

    // best effort cleanup
    defer std.fs.cwd().deleteTree(extract_dir) catch {};
    defer std.fs.cwd().deleteFileZ(output_path) catch {};

    // Extract

    try talkie_extract.run(allocator, &.{
        .input_path = "src/fixtures/" ++ fixture_dir ++ "/" ++ fixture_name,
        .output_path = extract_dir,
    });

    // Build

    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();

    const manifest_path = try concatZ(allocator, &.{ extract_dir, "/talkies.txt" });
    defer allocator.free(manifest_path);

    try talkie_build.run(allocator, &diagnostic, &.{
        .manifest_path = manifest_path,
        .output_path = output_path,
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();

    // Ensure the output file matches

    try expectFileHashEquals(
        output_path,
        @field(fixture_hashes, fixture_dir ++ "/" ++ fixture_name),
    );
}

fn concatZ(allocator: std.mem.Allocator, strs: []const []const u8) ![:0]const u8 {
    return std.mem.concatWithSentinel(allocator, u8, strs, 0);
}

fn getTempDir(allocator: std.mem.Allocator) ![]const u8 {
    if (builtin.os.tag != .windows)
        return "/tmp";

    var env = try std.process.getEnvMap(allocator);
    defer env.deinit();

    const tmp = env.get("TEMP") orelse return error.TestUnexpectedResult;
    return allocator.dupe(u8, tmp);
}

fn freeTempDir(allocator: std.mem.Allocator, str: []const u8) void {
    if (builtin.os.tag != .windows)
        return;
    allocator.free(str);
}

fn expectFileHashEquals(path: [*:0]const u8, comptime expected_hex: *const [64]u8) !void {
    var expected_hash: [32]u8 = undefined;
    _ = try std.fmt.hexToBytes(&expected_hash, expected_hex);

    var hasher: std.crypto.hash.sha2.Sha256 = .init(.{});

    try fs.readFileIntoZ(std.fs.cwd(), path, hasher.writer());

    const actual_hash = hasher.finalResult();
    try std.testing.expectEqualSlices(u8, &actual_hash, &expected_hash);
}
