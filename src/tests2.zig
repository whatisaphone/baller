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
    symbols_path: ?[:0]const u8 = null,
};

const baseball1997: Game = .{
    .fixture_dir = "baseball1997",
    .index_name = "BASEBALL.HE0",
    .fixture_names = &.{"BASEBALL.HE1"},
};
test "Backyard Baseball 1997 round trip raw" {
    try testRoundTrip(baseball1997, .raw, null);
}
test "Backyard Baseball 1997 round trip decode" {
    try testRoundTrip(baseball1997, .decode, &.init(.{
        .scrp_total = 193,
        .scrp_disassemble = 191,
        .scrp_decompile = 2,
        .scrp_raw = 0,
        .excd_total = 30,
        .excd_disassemble = 13,
        .excd_decompile = 17,
        .excd_raw = 0,
        .encd_total = 30,
        .encd_disassemble = 23,
        .encd_decompile = 7,
        .encd_raw = 0,
        .lsc2_total = 202,
        .lsc2_disassemble = 200,
        .lsc2_decompile = 2,
        .lsc2_raw = 0,
        .script_unknown_byte = 0,
    }));
}

const soccer: Game = .{
    .fixture_dir = "soccer",
    .index_name = "SOCCER.HE0",
    .fixture_names = &.{"SOCCER.(A)"},
};
test "Backyard Soccer round trip raw" {
    try testRoundTrip(soccer, .raw, null);
}
test "Backyard Soccer round trip decode" {
    try testRoundTrip(soccer, .decode, &.init(.{
        .scrp_total = 135,
        .scrp_disassemble = 132,
        .scrp_decompile = 3,
        .scrp_raw = 0,
        .excd_total = 29,
        .excd_disassemble = 17,
        .excd_decompile = 12,
        .excd_raw = 0,
        .encd_total = 29,
        .encd_disassemble = 25,
        .encd_decompile = 4,
        .encd_raw = 0,
        .lsc2_total = 143,
        .lsc2_disassemble = 140,
        .lsc2_decompile = 3,
        .lsc2_raw = 0,
        .script_unknown_byte = 0,
    }));
}

const football: Game = .{
    .fixture_dir = "football",
    .index_name = "FOOTBALL.HE0",
    .fixture_names = &.{ "FOOTBALL.(A)", "FOOTBALL.(B)" },
};
test "Backyard Football round trip raw" {
    try testRoundTrip(football, .raw, null);
}
test "Backyard Football round trip decode" {
    try testRoundTrip(football, .decode, &.init(.{
        .scrp_total = 388,
        .scrp_disassemble = 370,
        .scrp_decompile = 18,
        .scrp_raw = 0,
        .excd_total = 56,
        .excd_disassemble = 17,
        .excd_decompile = 39,
        .excd_raw = 0,
        .encd_total = 56,
        .encd_disassemble = 29,
        .encd_decompile = 27,
        .encd_raw = 0,
        .lsc2_total = 890,
        .lsc2_disassemble = 856,
        .lsc2_decompile = 34,
        .lsc2_raw = 0,
        .script_unknown_byte = 0,
    }));
}

const baseball2001: Game = .{
    .fixture_dir = "baseball2001",
    .index_name = "baseball 2001.he0",
    .fixture_names = &.{ "baseball 2001.(a)", "baseball 2001.(b)" },
    .symbols_path = "src/fixtures/baseball2001-symbols.ini",
};
test "Backyard Baseball 2001 round trip raw" {
    try testRoundTrip(baseball2001, .raw, null);
}
test "Backyard Baseball 2001 round trip decode" {
    try testRoundTrip(baseball2001, .decode, &.init(.{
        .scrp_total = 417,
        .scrp_disassemble = 400,
        .scrp_decompile = 17,
        .scrp_raw = 0,
        .excd_total = 37,
        .excd_disassemble = 11,
        .excd_decompile = 26,
        .excd_raw = 0,
        .encd_total = 37,
        .encd_disassemble = 14,
        .encd_decompile = 23,
        .encd_raw = 0,
        .lsc2_total = 1529,
        .lsc2_disassemble = 1403,
        .lsc2_decompile = 126,
        .lsc2_raw = 0,
        .script_unknown_byte = 0,
    }));
}

const basketball: Game = .{
    .fixture_dir = "basketball",
    .index_name = "Basketball.he0",
    .fixture_names = &.{ "Basketball.(a)", "Basketball.(b)" },
};
test "Backyard Basketball round trip raw" {
    try testRoundTrip(basketball, .raw, null);
}
test "Backyard Basketball round trip decode" {
    try testRoundTrip(basketball, .decode, &.init(.{
        .scrp_total = 663,
        .scrp_disassemble = 663,
        .scrp_decompile = 0,
        .scrp_raw = 0,
        .excd_total = 33,
        .excd_disassemble = 30,
        .excd_decompile = 3,
        .excd_raw = 0,
        .encd_total = 33,
        .encd_disassemble = 30,
        .encd_decompile = 3,
        .encd_raw = 0,
        .lsc2_total = 1142,
        .lsc2_disassemble = 1142,
        .lsc2_decompile = 0,
        .lsc2_raw = 0,
        .script_unknown_byte = 0,
    }));
}

fn testRoundTrip(
    comptime game: Game,
    options: enum { raw, decode },
    expected_extract_stats: ?*const std.EnumArray(extract.Stat, u16),
) !void {
    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();

    const extract_path = "/tmp/" ++ game.fixture_dir;
    const build_path = extract_path ++ "build";

    const extract_stats = try extract.run(std.testing.allocator, &diagnostic, .{
        .index_path = "src/fixtures/" ++ game.fixture_dir ++ "/" ++ game.index_name,
        .output_path = extract_path,
        .symbols_path = game.symbols_path,
        .options = switch (options) {
            .raw => .{
                .script = .decompile, // (ignored since everything is .raw)
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
                .script = .decompile,
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

    if (expected_extract_stats) |exp_ex_st|
        for (std.meta.tags(extract.Stat)) |stat| {
            errdefer dumpExtractStats(&extract_stats);
            try std.testing.expectEqual(extract_stats.get(stat), exp_ex_st.get(stat));
        };
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

fn dumpExtractStats(stats: *const std.EnumArray(extract.Stat, u16)) void {
    for (std.meta.tags(extract.Stat)) |stat|
        std.debug.print("{s} = {}\n", .{ @tagName(stat), stats.get(stat) });
}
