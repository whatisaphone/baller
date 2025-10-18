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

    .@"soccer-mls/SoccerMLS.he0" = "6417ed5728936176313cd3c3bf77ac97aa7964d1c186cbdcf9f0427ab9aaf3b7",
    .@"soccer-mls/SoccerMLS.(a)" = "d74316ce633220a9092e8b2a7121b72d17097e5f8aee2dfc8a282cf2e7bf6591",
    .@"soccer-mls/SoccerMLS.he2" = "baf2d3682ff8d2b67044d6f681311fda013648ba9ba4a3550e4faa555251f0e6",
    .@"soccer-mls/SoccerMLS.he4" = "b7b1b12a4a01be32b91ee345989473944b1223855da5e457c033777c5ed245e7",

    .@"football2002/Football2002.HE0" = "94410e75954919abf67ee593378184ac9aec724454564277c0305e87ca683890",
    .@"football2002/Football2002.(a)" = "e9b4770c2117ad3ba5445578780f2ef6bc531fc4d4f8e2d15da5c30e50035ea8",
    .@"football2002/Football2002.(b)" = "3755dda4b8d5dc1d0fcb00c282aec83e00148a1fc98c85e41bae06dff3cb2e6f",
    .@"football2002/Football2002.HE2" = "3e8d96a1f527bcc01ff7a43f1957fe7942a02b4acc3746947b195f8debf39836",
    .@"football2002/Football2002.HE4" = "4a7e8fa0989aabaf1d831a0e6c648b172b91f5ecc3e7def972aa721f0814bbf5",

    .@"basketball/Basketball.he0" = "9a767514bc5bf00743648111d6bf31e886e8f4fc8401be30447ee69126c997b2",
    .@"basketball/Basketball.(a)" = "b90c8b7c4d906d568647cd049d15b0c360a5945418c2807fcd22dcdc64decb84",
    .@"basketball/Basketball.(b)" = "7551d6465bd4cc8e0c0b97357ca28825d18bb2d645ccf30c5f56a810abd62e4c",
    .@"basketball/Basketball.he2" = "d5c7cba5ecf5849452b45f781e700781d3c98da242d865337e0d964c21f31a97",
    .@"basketball/Basketball.he4" = "6308b5070a2308ec5ebba62cc3d2ddc2a8ae2bc5eac56624178351d4f961ace1",

    .@"baseball2003/baseball2003.HE0" = "bb7cc66c0011e44180202a5515fdf0019ebbb9f364af2256c444d802175f27a6",
    .@"baseball2003/baseball2003.(a)" = "736fc80081ddcd5ed4c33f172e8a67bfc8e0b5a6c84bf6f2b4457c4756c2b903",
    .@"baseball2003/baseball2003.(b)" = "b9ab3a830ec987be87855a611476145554a8d4370e4cd2220f07803604f7abdf",
    .@"baseball2003/baseball2003.he2" = "de60f3422c2530e52d3e86418a458a9d71c9f388cf17c4e36533b44c96af0547",
    .@"baseball2003/baseball2003.he4" = "1d2e69e04a0f5b996fbd75422e1e0780e324e8f0cc34437e39ad152173ad3233",
    .@"baseball2003/Baseball2003.he9" = "3ad3481c7d7d60a7e287114a4040e63cd915370337d10fa2fe54997195560aaa",

    .@"soccer2004/Soccer2004.(a)" = "24e7e11f3d1a332bcb832df48d766e192d93a0123dbcc33640e6f1fafdef79fa",
    .@"soccer2004/Soccer2004.(b)" = "63a7cca59a2322ce6fa56f481fa210161ffc7f3bbe8d58d863498da319c61576",
    .@"soccer2004/Soccer2004.HE0" = "92dd8266b047ffa4925a1a57845165c6f9af97c176dd60519afb87896566df9a",
    .@"soccer2004/soccer2004.he2" = "d53c23ce18aed0a53a856a47c0ad2aebd66c88e780c5ba0fb4ee19e040d7ddb0",
    .@"soccer2004/soccer2004.he4" = "2e06e8e1376db08f84878e247a5e27a49808cdf4d7bab8a3ea06eb016ae929be",
    .@"soccer2004/soccer2004.he9" = "0061d2d8053985d47efd125c7c22b91d00b0cbbef3c7ec28f2167a4c886df79e",
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

test "Backyard Soccer MLS talkies round trip" {
    try testRoundTripTalkies("soccer-mls", "SoccerMLS.he2");
}

test "Backyard Football 2002 talkies round trip" {
    try testRoundTripTalkies("football2002", "Football2002.HE2");
}

test "Backyard Basketball talkies round trip" {
    try testRoundTripTalkies("basketball", "Basketball.he2");
}

test "Backyard Baseball 2003 talkies round trip" {
    try testRoundTripTalkies("baseball2003", "baseball2003.he2");
}

test "Backyard Soccer 2004 talkies round trip" {
    try testRoundTripTalkies("soccer2004", "soccer2004.he2");
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
