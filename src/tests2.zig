const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const build = @import("build.zig");
const dump = @import("dump.zig");
const extract = @import("extract.zig");
const fs = @import("fs.zig");
const io = @import("io.zig");
const fixture_hashes = @import("tests.zig").fixture_hashes;
const saveload_dump = @import("saveload_dump.zig");

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
    .symbols_path = "src/fixtures/baseball1997-symbols.ini",
};
test "Backyard Baseball 1997 round trip raw" {
    _ = try testRoundTrip(baseball1997, .raw);
}
test "Backyard Baseball 1997 round trip decode all" {
    const stats = try testRoundTrip(baseball1997, .decode_all);
    {
        errdefer dumpExtractStats(&stats);
        try expectTwoStatsEq(&stats, .rmim_total, .rmim_decode, 30);
        try expectTwoStatsEq(&stats, .scrp_total, .scrp_decompile, 193);
        try expectTwoStatsEq(&stats, .verb_total, .verb_decompile, 200);
        try expectTwoStatsEq(&stats, .excd_total, .excd_decompile, 30);
        try expectTwoStatsEq(&stats, .encd_total, .encd_decompile, 30);
        try expectTwoStatsEq(&stats, .lscr_total, .lscr_decompile, 375);
        try expectTwoStatsEq(&stats, .lsc2_total, .lsc2_decompile, 202);
    }
}
test "Backyard Baseball 1997 round trip disasm" {
    const stats = try testRoundTrip(baseball1997, .disasm);
    {
        errdefer dumpExtractStats(&stats);
        try std.testing.expectEqual(stats.get(.script_unknown_byte), 0);
    }
}

const soccer: Game = .{
    .fixture_dir = "soccer",
    .index_name = "SOCCER.HE0",
    .fixture_names = &.{"SOCCER.(A)"},
};
test "Backyard Soccer round trip raw" {
    _ = try testRoundTrip(soccer, .raw);
}
test "Backyard Soccer round trip decode all" {
    const stats = try testRoundTrip(soccer, .decode_all);
    {
        errdefer dumpExtractStats(&stats);
        try expectTwoStatsEq(&stats, .rmim_total, .rmim_decode, 29);
        try expectTwoStatsEq(&stats, .scrp_total, .scrp_decompile, 135);
        try expectTwoStatsEq(&stats, .verb_total, .verb_decompile, 191);
        try expectTwoStatsEq(&stats, .excd_total, .excd_decompile, 29);
        try expectTwoStatsEq(&stats, .encd_total, .encd_decompile, 29);
        try expectTwoStatsEq(&stats, .lscr_total, .lscr_decompile, 384);
        try expectTwoStatsEq(&stats, .lsc2_total, .lsc2_decompile, 143);
    }
}
test "Backyard Soccer round trip disasm" {
    const stats = try testRoundTrip(soccer, .disasm);
    {
        errdefer dumpExtractStats(&stats);
        try std.testing.expectEqual(stats.get(.script_unknown_byte), 0);
    }
}

const football: Game = .{
    .fixture_dir = "football",
    .index_name = "FOOTBALL.HE0",
    .fixture_names = &.{ "FOOTBALL.(A)", "FOOTBALL.(B)" },
};
test "Backyard Football round trip raw" {
    _ = try testRoundTrip(football, .raw);
}
test "Backyard Football round trip decode all" {
    const stats = try testRoundTrip(football, .decode_all);
    {
        errdefer dumpExtractStats(&stats);
        try expectTwoStatsEq(&stats, .rmim_total, .rmim_decode, 56);
        try expectTwoStatsEq(&stats, .scrp_total, .scrp_decompile, 388);
        try expectTwoStatsEq(&stats, .verb_total, .verb_decompile, 294);
        try expectTwoStatsEq(&stats, .excd_total, .excd_decompile, 56);
        try expectTwoStatsEq(&stats, .encd_total, .encd_decompile, 56);
        try expectTwoStatsEq(&stats, .lsc2_total, .lsc2_decompile, 890);
    }
}
test "Backyard Football round trip disasm" {
    const stats = try testRoundTrip(football, .disasm);
    {
        errdefer dumpExtractStats(&stats);
        try std.testing.expectEqual(stats.get(.script_unknown_byte), 0);
    }
}

const baseball2001: Game = .{
    .fixture_dir = "baseball2001",
    .index_name = "baseball 2001.he0",
    .fixture_names = &.{ "baseball 2001.(a)", "baseball 2001.(b)" },
    .symbols_path = "src/fixtures/baseball2001-symbols.ini",
};
test "Backyard Baseball 2001 round trip raw" {
    _ = try testRoundTrip(baseball2001, .raw);
}
test "Backyard Baseball 2001 round trip decode all" {
    const stats = try testRoundTrip(baseball2001, .decode_all);
    {
        errdefer dumpExtractStats(&stats);
        try expectTwoStatsEq(&stats, .rmim_total, .rmim_decode, 37);
        try expectTwoStatsEq(&stats, .scrp_total, .scrp_decompile, 417);
        try expectTwoStatsEq(&stats, .verb_total, .verb_decompile, 43);
        try expectTwoStatsEq(&stats, .excd_total, .excd_decompile, 37);
        try expectTwoStatsEq(&stats, .encd_total, .encd_decompile, 37);
        try expectTwoStatsEq(&stats, .lsc2_total, .lsc2_decompile, 1529);
    }
}
test "Backyard Baseball 2001 round trip disasm" {
    const stats = try testRoundTrip(baseball2001, .disasm);
    {
        errdefer dumpExtractStats(&stats);
        try std.testing.expectEqual(stats.get(.script_unknown_byte), 0);
    }
}

const basketball: Game = .{
    .fixture_dir = "basketball",
    .index_name = "Basketball.he0",
    .fixture_names = &.{ "Basketball.(a)", "Basketball.(b)" },
};
test "Backyard Basketball round trip raw" {
    _ = try testRoundTrip(basketball, .raw);
}
test "Backyard Basketball round trip decode all" {
    const stats = try testRoundTrip(basketball, .decode_all);
    {
        errdefer dumpExtractStats(&stats);
        try expectTwoStatsEq(&stats, .rmim_total, .rmim_decode, 33);
        try expectTwoStatsEq(&stats, .scrp_total, .scrp_decompile, 663);
        try expectTwoStatsEq(&stats, .verb_total, .verb_decompile, 11);
        try expectTwoStatsEq(&stats, .excd_total, .excd_decompile, 33);
        try expectTwoStatsEq(&stats, .encd_total, .encd_decompile, 33);
        try expectTwoStatsEq(&stats, .lsc2_total, .lsc2_decompile, 1142);
    }
}
test "Backyard Basketball round trip disasm" {
    const stats = try testRoundTrip(basketball, .disasm);
    {
        errdefer dumpExtractStats(&stats);
        try std.testing.expectEqual(stats.get(.script_unknown_byte), 0);
    }
}

test "dump smoke test" {
    const in_path = "src/fixtures/baseball2001/baseball 2001.(b)";
    const in_file = try std.fs.cwd().openFileZ(in_path, .{});
    defer in_file.close();
    var in_xor = io.xorReader(in_file.reader(), extract.xor_key);
    var in_buf = std.io.bufferedReader(in_xor.reader());
    var in_count = std.io.countingReader(in_buf.reader());
    var in = std.io.limitedReader(in_count.reader(), std.math.maxInt(u32));

    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};
    const diag: Diagnostic.ForBinaryFile = .init(&diagnostic, "-");

    try dump.run(&in, &diag, "/tmp/dump");
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

fn testRoundTrip(
    comptime game: Game,
    options: enum { raw, decode_all, disasm },
) !std.EnumArray(extract.Stat, u16) {
    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    const extract_path = "/tmp/" ++ game.fixture_dir;
    const build_path = extract_path ++ "build";

    const extract_stats = try extract.run(std.testing.allocator, &diagnostic, .{
        .index_path = "src/fixtures/" ++ game.fixture_dir ++ "/" ++ game.index_name,
        .output_path = extract_path,
        .symbols_path = game.symbols_path,
        .options = switch (options) {
            .raw => .{
                .script = .decompile, // (ignored since everything is .raw)
                .annotate = false,
                .rmim = .raw,
                .scrp = .raw,
                .encd = .raw,
                .excd = .raw,
                .lscr = .raw,
                .lsc2 = .raw,
                .obim = .raw,
                .obcd = .raw,
                .digi = .raw,
                .awiz = .raw,
                .mult = .raw,
                .akos = .raw,
            },
            .decode_all => .{
                .script = .decompile,
                .annotate = false,
                .rmim = .decode,
                .scrp = .decode,
                .encd = .decode,
                .excd = .decode,
                .lscr = .decode,
                .lsc2 = .decode,
                .obim = .decode,
                .obcd = .decode,
                .digi = .decode,
                .awiz = .decode,
                .mult = .decode,
                .akos = .decode,
            },
            .disasm => .{
                .script = .disassemble,
                .annotate = false,
                .rmim = .raw,
                .scrp = .decode,
                .encd = .decode,
                .excd = .decode,
                .lscr = .decode,
                .lsc2 = .decode,
                .obim = .raw,
                .obcd = .decode,
                .digi = .raw,
                .awiz = .raw,
                .mult = .raw,
                .akos = .raw,
            },
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();

    diagnostic.deinit();
    diagnostic = .init(std.testing.allocator);

    try build.run(std.testing.allocator, &diagnostic, .{
        .project_path = extract_path ++ "/project.scu",
        .index_path = build_path ++ "/" ++ game.index_name,
        .options = .{
            .awiz_strategy = .original,
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();

    var output_dir = try std.fs.cwd().openDirZ(build_path, .{});
    defer output_dir.close();

    inline for (.{game.index_name} ++ game.fixture_names) |name| {
        errdefer std.debug.print("{s}\n", .{name});
        const expected_hex = @field(fixture_hashes, game.fixture_dir ++ "/" ++ name);
        try expectFileHashEquals(output_dir, name, expected_hex);
    }

    return extract_stats;
}

test "decompile annotate smoke test" {
    const game = &baseball2001;

    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    _ = try extract.run(std.testing.allocator, &diagnostic, .{
        .index_path = "src/fixtures/" ++ game.fixture_dir ++ "/" ++ game.index_name,
        .output_path = "/tmp/" ++ game.fixture_dir,
        .symbols_path = game.symbols_path,
        .options = .{
            .script = .decompile,
            .annotate = true,
            .rmim = .raw,
            .scrp = .decode,
            .encd = .decode,
            .excd = .decode,
            .lscr = .decode,
            .lsc2 = .decode,
            .obim = .raw,
            .obcd = .decode,
            .digi = .raw,
            .awiz = .raw,
            .mult = .raw,
            .akos = .raw,
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

test "disasm annotate smoke test" {
    const game = &baseball2001;

    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    _ = try extract.run(std.testing.allocator, &diagnostic, .{
        .index_path = "src/fixtures/" ++ game.fixture_dir ++ "/" ++ game.index_name,
        .output_path = "/tmp/" ++ game.fixture_dir,
        .symbols_path = game.symbols_path,
        .options = .{
            .script = .disassemble,
            .annotate = true,
            .rmim = .raw,
            .scrp = .decode,
            .encd = .decode,
            .excd = .decode,
            .lscr = .decode,
            .lsc2 = .decode,
            .obim = .raw,
            .obcd = .decode,
            .digi = .raw,
            .awiz = .raw,
            .mult = .raw,
            .akos = .raw,
        },
    });
    try diagnostic.writeToStderrAndPropagateIfAnyErrors();
}

fn expectFileHashEquals(dir: std.fs.Dir, path: [*:0]const u8, expected_hex: *const [64]u8) !void {
    var expected_hash: [32]u8 = undefined;
    _ = try std.fmt.hexToBytes(&expected_hash, expected_hex);

    var hasher: std.crypto.hash.sha2.Sha256 = .init(.{});
    try fs.readFileIntoZ(dir, path, hasher.writer());
    const actual_hash = hasher.finalResult();

    if (!std.mem.eql(u8, &actual_hash, &expected_hash))
        return error.TestExpectedEqual;
}

fn expectTwoStatsEq(
    stats: *const std.EnumArray(extract.Stat, u16),
    stat_a: extract.Stat,
    stat_b: extract.Stat,
    expected: u16,
) !void {
    try std.testing.expectEqual(stats.get(stat_a), expected);
    try std.testing.expectEqual(stats.get(stat_b), expected);
}

fn dumpExtractStats(stats: *const std.EnumArray(extract.Stat, u16)) void {
    for (std.meta.tags(extract.Stat)) |stat|
        std.debug.print("{s} = {}\n", .{ @tagName(stat), stats.get(stat) });
}

test "Backyard Baseball 1997 saveload dump smoke test" {
    try saveloadDumpSmokeTest(
        std.testing.allocator,
        "baseball1997/BASEBALL.HE0",
        "saveload/baseball1997.sg1",
    );
}

test "Backyard Baseball 2001 saveload dump smoke test" {
    try saveloadDumpSmokeTest(
        std.testing.allocator,
        "baseball2001/baseball 2001.he0",
        "saveload/baseball2001.sg",
    );
}

fn saveloadDumpSmokeTest(
    gpa: std.mem.Allocator,
    comptime index_path: []const u8,
    comptime savegame_path: []const u8,
) !void {
    var diagnostic: Diagnostic = .init(std.testing.allocator);
    defer diagnostic.deinit();
    errdefer diagnostic.writeToStderrAndPropagateIfAnyErrors() catch {};

    const sink = try std.fs.cwd().createFileZ("/dev/null", .{});
    defer sink.close();

    try saveload_dump.run(.{
        .gpa = gpa,
        .diagnostic = &diagnostic,
        .index_path = "src/fixtures/" ++ index_path,
        .savegame_path = "src/fixtures/" ++ savegame_path,
        .out = sink.writer(),
    });
}
