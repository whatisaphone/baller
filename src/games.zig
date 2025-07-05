const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const report = @import("report.zig");

pub const Game = enum {
    baseball_1997,
    soccer_1998,
    football_1999,
    baseball_2001,
    basketball,

    pub fn lt(a: Game, b: Game) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }

    pub fn le(a: Game, b: Game) bool {
        return @intFromEnum(a) <= @intFromEnum(b);
    }

    pub fn ge(a: Game, b: Game) bool {
        return @intFromEnum(a) >= @intFromEnum(b);
    }

    pub fn target(self: Game) Target {
        return switch (self) {
            .baseball_1997 => .sputm90,
            .soccer_1998 => .sputm98,
            .football_1999, .baseball_2001 => .sputm99,
            .basketball => .sputm100,
        };
    }
};

pub const Target = enum {
    sputm90,
    sputm98,
    sputm99,
    sputm100,

    pub fn le(a: Target, b: Target) bool {
        return @intFromEnum(a) <= @intFromEnum(b);
    }

    pub fn ge(a: Target, b: Target) bool {
        return @intFromEnum(a) >= @intFromEnum(b);
    }

    // mild hack, i should probably get rid of `Game` and switch to `Target`
    // everywhere
    pub fn pickAnyGame(self: Target) Game {
        return switch (self) {
            .sputm90 => .baseball_1997,
            .sputm98 => .soccer_1998,
            .sputm99 => .baseball_2001,
            .sputm100 => .basketball,
        };
    }
};

pub const longest_index_name_len = "baseball 2001.he0".len;

pub fn detectGameOrFatal(diagnostic: *Diagnostic, index_path: []const u8) !Game {
    const names: []const struct { []const u8, Game } = &.{
        .{ "BASEBALL.HE0", .baseball_1997 },
        .{ "SOCCER.HE0", .soccer_1998 },
        .{ "FOOTBALL.HE0", .football_1999 },
        .{ "baseball 2001.he0", .baseball_2001 },
        .{ "Basketball.he0", .basketball },
    };

    const error_suffix = comptime error_suffix: {
        var error_suffix: []const u8 = "\nsupported filenames:";
        for (names) |p|
            error_suffix = error_suffix ++ "\n- " ++ p[0];
        break :error_suffix error_suffix[0..error_suffix.len];
    };

    const input_name = std.fs.path.basename(index_path);
    for (names) |p|
        if (std.mem.eql(u8, input_name, p[0]))
            return p[1];

    diagnostic.err("index filename not recognized: {s}" ++ error_suffix, .{input_name});
    return error.AddedToDiagnostic;
}

pub fn maxsLen(game: Game) u32 {
    return switch (game) {
        .baseball_1997, .soccer_1998 => 38,
        .football_1999, .baseball_2001, .basketball => 44,
    };
}

pub fn hasTalkies(game: Game) bool {
    return @intFromEnum(game) >= @intFromEnum(Game.football_1999);
}

pub fn hasDisk(game: Game) bool {
    return game != .baseball_1997;
}

pub fn hasIndexInib(game: Game) bool {
    return game != .baseball_1997;
}

pub fn hasIndexSver(game: Game) bool {
    return game == .basketball;
}

pub fn directoryNonPresentLen(target: Target) u32 {
    return if (target == .sputm90)
        0xffff_ffff
    else
        0;
}

pub fn writeMultLen(target: Target) bool {
    return target != .sputm90;
}

pub fn pointPathToDisk(target: Target, path: []u8, disk_number: u8) void {
    // change ".HE0" to ".HE1"
    if (target.le(.sputm90)) {
        std.debug.assert(disk_number == 1);
        path[path.len - 1] = '1';
        return;
    }

    // change ".he0" to ".(a)"
    const lowercase = path[path.len - 2] & 0x20;
    path[path.len - 3] = '(';
    path[path.len - 2] = ('A' | lowercase) - 1 + disk_number;
    path[path.len - 1] = ')';
}

/// change ".he0" to ".he4"
pub fn pointPathToMusic(game: Game, path: []u8) void {
    if (builtin.mode == .Debug) {
        const ext = path[path.len - 4 ..];
        std.debug.assert(std.mem.eql(u8, ext, ".he0") or std.mem.eql(u8, ext, ".HE0"));
    }

    path[path.len - 1] = '4';

    if (game.target() == .sputm99) {
        for (path) |*c|
            c.* = std.ascii.toLower(c.*);
    }
}

pub fn firstLocalScript(game: Game) u16 {
    return if (@intFromEnum(game) <= @intFromEnum(Game.soccer_1998))
        200
    else
        2048;
}
