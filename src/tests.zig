const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const blockId = @import("block_id.zig").blockId;
const blockIdToStr = @import("block_id.zig").blockIdToStr;
const parseBlockId = @import("block_id.zig").parseBlockId;
const build = @import("build.zig");
const extract = @import("extract.zig");
const fs = @import("fs.zig");
const talkie_build = @import("talkie_build.zig");
const talkie_extract = @import("talkie_extract.zig");

// Extract and rebuild every supported game, and verify the output is identical
// to the original.

const fixture_hashes = .{
    .@"baseball1997/BASEBALL.HE0" = "6b701f415251a1d25fe91fb28c78193ac01b382b4292c377b0fd01e30a61c5da",
    .@"baseball1997/BASEBALL.HE1" = "f3e910f120433d318e30dae75e0fbd418f08338966cbf129487909e1f3cf5cd8",
    .@"baseball1997/BASEBALL.HE2" = "216db30c5810b063aaf718defdf67ee11c2d6aaa7bf2cf64c7101e507f65647a",
    .@"baseball2001/baseball 2001.he0" = "c89d5c17c58db55652b31828a4b27edfa9810dbfddcada428b8b2bf5fb85a5b9",
    .@"baseball2001/baseball 2001.(a)" = "a2bd2d171c47a320fe31dd2e40cfcbecae01d46c5ecebc362fc998e4f0cb73ff",
    .@"baseball2001/baseball 2001.(b)" = "dde0397d5c658f2acffdebb5b58b0d2770707619092a4319354b37512b513038",
    .@"baseball2001/baseball 2001.he2" = "583be6ee0ad30bdd1d4a78ddc006155d77f567a6c5467b22d8871c137e974927",
    .@"soccer/SOCCER.HE0" = "44d5de043628bbe5db166f00e8cb9b0398800dc30b0868ae0ce4c8eba96ec9f9",
    .@"soccer/SOCCER.(A)" = "8cdd016013493bbc22bda8e3fa3d62ece7a61e4ee346fd85d7288e3de385bf79",
    .@"soccer/SOCCER.HE2" = "2e5cd139e44274652d94ed1af09995f20426be4741d13abb4c9075356bdbe073",
    .@"football/FOOTBALL.HE0" = "5fbec205047ce8da3d42b2c48652183ef30bcf0d3e1aa233cda9745317bbb59d",
    .@"football/FOOTBALL.(A)" = "4d406cbe8243f84bac39a743495b6377879afc9d7a187e203d7f58cd8bc59742",
    .@"football/FOOTBALL.(B)" = "42cf15534b9a707ec1cec54a09d30211da07b226a084e802c3b049701751a2f1",
    .@"football/FOOTBALL.HE2" = "fc5ab3c7d917eaaa9cb2e7cee6ca73bef0958d939e6349498bb17ae752f691ef",
    .@"basketball/Basketball.he0" = "9a767514bc5bf00743648111d6bf31e886e8f4fc8401be30447ee69126c997b2",
    .@"basketball/Basketball.(a)" = "b90c8b7c4d906d568647cd049d15b0c360a5945418c2807fcd22dcdc64decb84",
    .@"basketball/Basketball.(b)" = "7551d6465bd4cc8e0c0b97357ca28825d18bb2d645ccf30c5f56a810abd62e4c",
    .@"basketball/Basketball.he2" = "d5c7cba5ecf5849452b45f781e700781d3c98da242d865337e0d964c21f31a97",
};

test "fixture integrity" {
    inline for (std.meta.fields(@TypeOf(fixture_hashes))) |field| {
        const path = "src/fixtures/" ++ field.name;
        const expected_hash_hex = @field(fixture_hashes, field.name);
        try expectFileHashEquals(path, expected_hash_hex);
    }
}

test "Backyard Baseball 1997 round trip raw" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "baseball1997",
        "BASEBALL.HE0",
        .raw,
        null,
        &.{ "BASEBALL.HE0", "BASEBALL.HE1" },
    );
    defer result.deinit(allocator);
}

test "Backyard Baseball 1997 round trip imperfect" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "baseball1997",
        "BASEBALL.HE0",
        .decode_imperfect,
        null,
        &.{},
    );
    defer result.deinit(allocator);

    try expectBlockStat(&result.block_stats, "AWIZ", 1761, 1812);
    try expectBlockStat(&result.block_stats, "MULT", 184, 184);
}

test "Backyard Baseball 1997 round trip decode/encode" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "baseball1997",
        "BASEBALL.HE0",
        .decode,
        "fixtures/baseball1997-symbols.ini",
        &.{ "BASEBALL.HE0", "BASEBALL.HE1" },
    );
    defer result.deinit(allocator);

    try expectBlockStats(&result.block_stats, &.{
        .{ "RMIM", 16, 30 },
        .{ "RMHD", 0, 30 },
        .{ "CYCL", 0, 30 },
        .{ "TRNS", 0, 30 },
        .{ "PALS", 0, 30 },
        .{ "EXCD", 30, 30 },
        .{ "ENCD", 30, 30 },
        .{ "NLSC", 0, 30 },
        .{ "LSCR", 375, 375 },
        .{ "SCRP", 193, 193 },
        .{ "SOUN", 0, 1805 },
        .{ "CHAR", 0, 11 },
        .{ "AWIZ", 0, 1812 },
        .{ "OBIM", 0, 268 },
        .{ "OBCD", 0, 268 },
        .{ "POLD", 0, 8 },
        .{ "AKOS", 405, 406 },
        .{ "AKCD", 0, 45439 },
        .{ "MULT", 184, 184 },
        .{ "LSC2", 202, 202 },
    });

    try std.testing.expectEqual(result.scripts_with_unknown_byte, 0);
}

test "Backyard Baseball 1997 talkies round trip" {
    try testRoundTripTalkies("baseball1997", "BASEBALL.HE2");
}

test "Backyard Baseball 2001 round trip raw" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "baseball2001",
        "baseball 2001.he0",
        .raw,
        null,
        &.{ "baseball 2001.he0", "baseball 2001.(a)", "baseball 2001.(b)" },
    );
    defer result.deinit(allocator);
}

test "Backyard Baseball 2001 round trip imperfect" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "baseball2001",
        "baseball 2001.he0",
        .decode_imperfect,
        null,
        &.{},
    );
    defer result.deinit(allocator);

    try expectBlockStat(&result.block_stats, "AWIZ", 15130, 15133);
    try expectBlockStat(&result.block_stats, "MULT", 659, 659);
}

// NOTE: this is the main one that tests symbols.
test "Backyard Baseball 2001 round trip decode/encode" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "baseball2001",
        "baseball 2001.he0",
        .decode,
        "fixtures/baseball2001-symbols.ini",
        &.{ "baseball 2001.he0", "baseball 2001.(a)", "baseball 2001.(b)" },
    );
    defer result.deinit(allocator);

    try expectBlockStats(&result.block_stats, &.{
        .{ "RMIM", 5, 37 },
        .{ "RMHD", 0, 37 },
        .{ "CYCL", 0, 37 },
        .{ "TRNS", 0, 37 },
        .{ "PALS", 0, 37 },
        .{ "EXCD", 37, 37 },
        .{ "ENCD", 37, 37 },
        .{ "NLSC", 0, 37 },
        .{ "DIGI", 3652, 3662 },
        .{ "TLKE", 0, 2508 },
        .{ "AKOS", 644, 645 },
        .{ "AKCD", 0, 105312 },
        .{ "AWIZ", 0, 15133 },
        .{ "MULT", 659, 659 },
        .{ "TALK", 529, 617 },
        .{ "CHAR", 0, 7 },
        .{ "LSC2", 1529, 1529 },
        .{ "SCRP", 417, 417 },
        .{ "OBIM", 0, 68 },
        .{ "OBCD", 0, 68 },
        .{ "POLD", 0, 6 },
    });

    try std.testing.expectEqual(result.scripts_with_unknown_byte, 0);
}

test "Backyard Baseball 2001 talkies round trip" {
    try testRoundTripTalkies("baseball2001", "baseball 2001.he2");
}

test "Backyard Soccer round trip raw" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "soccer",
        "SOCCER.HE0",
        .raw,
        null,
        &.{ "SOCCER.HE0", "SOCCER.(A)" },
    );
    defer result.deinit(allocator);
}

test "Backyard Soccer round trip imperfect" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "soccer",
        "SOCCER.HE0",
        .decode_imperfect,
        null,
        &.{},
    );
    defer result.deinit(allocator);

    try expectBlockStat(&result.block_stats, "AWIZ", 2668, 3117);
    try expectBlockStat(&result.block_stats, "MULT", 389, 389);
}

test "Backyard Soccer round trip decode/encode" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "soccer",
        "SOCCER.HE0",
        .decode,
        null,
        &.{ "SOCCER.HE0", "SOCCER.(A)" },
    );
    defer result.deinit(allocator);

    try expectBlockStats(&result.block_stats, &.{
        .{ "RMIM", 22, 29 },
        .{ "RMHD", 0, 29 },
        .{ "CYCL", 0, 29 },
        .{ "TRNS", 0, 29 },
        .{ "PALS", 0, 29 },
        .{ "EXCD", 29, 29 },
        .{ "ENCD", 29, 29 },
        .{ "NLSC", 0, 29 },
        .{ "LSCR", 384, 384 },
        .{ "SCRP", 135, 135 },
        .{ "DIGI", 1594, 1686 },
        .{ "TALK", 141, 2146 },
        .{ "CHAR", 0, 10 },
        .{ "AWIZ", 0, 3117 },
        .{ "OBIM", 0, 197 },
        .{ "OBCD", 0, 197 },
        .{ "POLD", 0, 6 },
        .{ "AKOS", 323, 323 },
        .{ "AKCD", 0, 53304 },
        .{ "MULT", 389, 389 },
        .{ "LSC2", 143, 143 },
    });

    try std.testing.expectEqual(result.scripts_with_unknown_byte, 0);
}

test "Backyard Soccer talkies round trip" {
    try testRoundTripTalkies("soccer", "SOCCER.HE2");
}

test "Backyard Football round trip raw" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "football",
        "FOOTBALL.HE0",
        .raw,
        null,
        &.{ "FOOTBALL.HE0", "FOOTBALL.(A)", "FOOTBALL.(B)" },
    );
    defer result.deinit(allocator);
}

test "Backyard Football round trip imperfect" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "football",
        "FOOTBALL.HE0",
        .decode_imperfect,
        null,
        &.{},
    );
    defer result.deinit(allocator);

    try expectBlockStat(&result.block_stats, "MULT", 1145, 1145);
    try expectBlockStat(&result.block_stats, "AWIZ", 11465, 11594);
}

test "Backyard Football round trip decode/encode" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "football",
        "FOOTBALL.HE0",
        .decode,
        null,
        &.{ "FOOTBALL.HE0", "FOOTBALL.(A)", "FOOTBALL.(B)" },
    );
    defer result.deinit(allocator);

    try expectBlockStats(&result.block_stats, &.{
        .{ "RMIM", 14, 56 },
        .{ "RMHD", 0, 56 },
        .{ "CYCL", 0, 56 },
        .{ "TRNS", 0, 56 },
        .{ "PALS", 0, 56 },
        .{ "EXCD", 56, 56 },
        .{ "ENCD", 56, 56 },
        .{ "NLSC", 0, 56 },
        .{ "DIGI", 554, 1599 },
        .{ "LSC2", 890, 890 },
        .{ "AKOS", 311, 311 },
        .{ "AKCD", 0, 75689 },
        .{ "MULT", 1145, 1145 },
        .{ "AWIZ", 0, 11594 },
        .{ "TALK", 549, 2768 },
        .{ "CHAR", 0, 13 },
        .{ "SCRP", 388, 388 },
        .{ "OBIM", 0, 290 },
        .{ "OBCD", 0, 290 },
        .{ "POLD", 0, 9 },
    });

    try std.testing.expectEqual(result.scripts_with_unknown_byte, 0);
}

test "Backyard Football talkies round trip" {
    try testRoundTripTalkies("football", "FOOTBALL.HE2");
}

test "Backyard Basketball round trip raw" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "basketball",
        "Basketball.he0",
        .raw,
        null,
        &.{ "Basketball.he0", "Basketball.(a)", "Basketball.(b)" },
    );
    defer result.deinit(allocator);
}

test "Backyard Basketball round trip imperfect" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "basketball",
        "Basketball.he0",
        .decode_imperfect,
        null,
        &.{},
    );
    defer result.deinit(allocator);

    try expectBlockStat(&result.block_stats, "MULT", 1245, 1579);
    try expectBlockStat(&result.block_stats, "AWIZ", 15282, 15622);
}

test "Backyard Basketball round trip decode/encode" {
    const allocator = std.testing.allocator;

    var result = try testRoundTrip(
        allocator,
        "basketball",
        "Basketball.he0",
        .decode,
        null,
        &.{ "Basketball.he0", "Basketball.(a)", "Basketball.(b)" },
    );
    defer result.deinit(allocator);

    try expectBlockStats(&result.block_stats, &.{
        .{ "RMIM", 12, 33 },
        .{ "RMHD", 0, 33 },
        .{ "CYCL", 0, 33 },
        .{ "TRNS", 0, 33 },
        .{ "PALS", 0, 33 },
        .{ "EXCD", 33, 33 },
        .{ "ENCD", 33, 33 },
        .{ "NLSC", 0, 33 },
        .{ "WSOU", 3731, 3731 },
        .{ "LSC2", 1142, 1142 },
        .{ "AKOS", 396, 396 },
        .{ "AKCD", 0, 86022 },
        .{ "MULT", 1579, 1579 },
        .{ "AWIZ", 0, 15629 },
        .{ "TLKE", 0, 1487 },
        .{ "SCRP", 663, 663 },
        .{ "CHAR", 0, 5 },
        .{ "OBIM", 0, 12 },
        .{ "OBCD", 0, 12 },
        .{ "POLD", 0, 1 },
    });

    try std.testing.expectEqual(result.scripts_with_unknown_byte, 0);
}

test "Backyard Basketball talkies round trip" {
    try testRoundTripTalkies("basketball", "Basketball.he2");
}

fn testRoundTrip(
    allocator: std.mem.Allocator,
    comptime fixture_dir: []const u8,
    comptime index_name: []const u8,
    mode: enum { raw, decode, decode_imperfect },
    comptime symbols_path: ?[]const u8,
    comptime fixture_names: []const []const u8,
) !extract.Result {
    std.debug.assert((fixture_names.len == 0) == (mode == .decode_imperfect));

    // Build temp dirs for this test

    const tmp = try getTempDir(allocator);
    defer freeTempDir(allocator, tmp);

    const extract_dir = try concatZ(allocator, &.{ tmp, "/baller-test-extract" });
    defer allocator.free(extract_dir);

    const build_dir = try concatZ(allocator, &.{ tmp, "/baller-test-build" });
    defer allocator.free(build_dir);

    // best effort cleanup
    defer std.fs.cwd().deleteTree(extract_dir) catch {};
    defer std.fs.cwd().deleteTree(build_dir) catch {};

    // Extract

    const symbols_text = if (symbols_path) |path| @embedFile(path) else "";

    var result = try extract.run(allocator, &.{
        .input_path = "src/fixtures/" ++ fixture_dir ++ "/" ++ index_name,
        .output_path = extract_dir,
        .rmim_decode = mode != .raw,
        .script_modes = if (mode == .raw) &.{.raw} else &.{ .decode, .raw },
        .sound_modes = if (mode == .raw) &.{.raw} else &.{ .decode, .raw },
        .awiz_modes = if (mode != .decode_imperfect) &.{.raw} else &.{ .decode, .raw },
        .mult_modes = if (mode == .raw) &.{.raw} else &.{ .decode, .raw },
        .akos_modes = if (mode == .raw) &.{.raw} else &.{ .decode, .raw },
        .akcd_modes = if (mode != .decode_imperfect) &.{.raw} else &.{ .decode, .raw },
        .symbols_text = symbols_text,
    });
    errdefer result.deinit(allocator);

    // Build

    const project_txt_path = try concatZ(allocator, &.{ extract_dir, "/project.txt" });
    defer allocator.free(project_txt_path);

    const out_index_path = try concatZ(allocator, &.{ build_dir, "/", index_name });
    defer allocator.free(out_index_path);

    try build.run(allocator, &.{
        .project_txt_path = project_txt_path,
        .output_path = out_index_path,
        .attribution = false,
    });

    // Ensure the output files match

    inline for (fixture_names) |name| {
        const built_file_path = try concatZ(allocator, &.{ build_dir, "/", name });
        defer allocator.free(built_file_path);

        try expectFileHashEquals(
            built_file_path,
            @field(fixture_hashes, fixture_dir ++ "/" ++ name),
        );
    }

    return result;
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

    const manifest_path = try concatZ(allocator, &.{ extract_dir, "/talkies.txt" });
    defer allocator.free(manifest_path);

    try talkie_build.run(allocator, &.{
        .manifest_path = manifest_path,
        .output_path = output_path,
    });

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

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});

    try fs.readFileIntoZ(std.fs.cwd(), path, hasher.writer());

    const actual_hash = hasher.finalResult();
    try std.testing.expectEqualSlices(u8, &actual_hash, &expected_hash);
}

fn expectBlockStats(
    stats: *const std.AutoArrayHashMapUnmanaged(BlockId, extract.BlockStat),
    // block id, decoded, total
    expected: []const struct { []const u8, u32, u32 },
) !void {
    errdefer dumpBlockStats(stats);
    try std.testing.expectEqual(stats.count(), expected.len);
    for (expected) |exp| {
        const block_id_str, const exp_decoded, const exp_total = exp;
        const block_id = parseBlockId(block_id_str) orelse return error.TestUnexpectedResult;
        const stat = stats.getPtr(block_id) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(stat.decoded, exp_decoded);
        try std.testing.expectEqual(stat.total, exp_total);
    }
}

fn expectBlockStat(
    stats: *const std.AutoArrayHashMapUnmanaged(BlockId, extract.BlockStat),
    comptime block_id_str: []const u8,
    expected_decoded: u32,
    expected_total: u32,
) !void {
    const block_id = comptime blockId(block_id_str);
    const stat = stats.getPtr(block_id) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(stat.decoded, expected_decoded);
    try std.testing.expectEqual(stat.total, expected_total);
}

fn dumpBlockStats(
    stats: *const std.AutoArrayHashMapUnmanaged(BlockId, extract.BlockStat),
) void {
    var it = stats.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s}: {any}\n", .{ blockIdToStr(entry.key_ptr), entry.value_ptr });
    }
}
