const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");

pub const Game = enum {
    baseball_1997,
    soccer_1998,
    football_1999,
    baseball_2001,
    soccer_mls,
    football_2002,
    basketball,
    baseball_2003,
    soccer_2004,

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
            .football_1999, .baseball_2001, .soccer_mls => .sputm99,
            .football_2002, .basketball, .baseball_2003 => .sputm100,
            .soccer_2004 => .sputm101,
        };
    }

    const index_names: []const struct { []const u8, Game } = &.{
        .{ "BASEBALL.HE0", .baseball_1997 },
        .{ "SOCCER.HE0", .soccer_1998 },
        .{ "FOOTBALL.HE0", .football_1999 },
        .{ "baseball 2001.he0", .baseball_2001 },
        .{ "SoccerMLS.he0", .soccer_mls },
        .{ "Football2002.HE0", .football_2002 },
        .{ "Basketball.he0", .basketball },
        .{ "baseball2003.HE0", .baseball_2003 },
        .{ "Soccer2004.HE0", .soccer_2004 },
    };

    pub const longest_index_name_len = "baseball 2001.he0".len;

    fn detectGame(input_name: []const u8) ?Game {
        for (index_names) |p|
            if (std.mem.eql(u8, input_name, p[0]))
                return p[1];
        return null;
    }

    pub fn detectGameOrFatal(diagnostic: *Diagnostic, index_path: []const u8) !Game {
        const input_name = std.fs.path.basename(index_path);
        if (detectGame(input_name)) |game|
            return game;

        const error_suffix = comptime error_suffix: {
            var error_suffix: []const u8 = "\nsupported filenames:";
            for (index_names) |p|
                error_suffix = error_suffix ++ "\n- " ++ p[0];
            break :error_suffix error_suffix[0..error_suffix.len];
        };

        diagnostic.err("index filename not recognized: {s}" ++ error_suffix, .{input_name});
        return error.AddedToDiagnostic;
    }
};

pub const Target = enum {
    sputm90,
    sputm98,
    sputm99,
    sputm100,
    sputm101,

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
            .sputm101 => .soccer_2004,
        };
    }

    pub fn maxsLen(target: Target) u32 {
        return if (target.le(.sputm98))
            38
        else
            44;
    }

    pub fn hasTalkies(target: Target) bool {
        return target.ge(.sputm99);
    }

    pub fn hasDisk(target: Target) bool {
        return target != .sputm90;
    }

    pub fn hasIndexInib(target: Target) bool {
        return target != .sputm90;
    }

    pub fn hasIndexSver(target: Target) bool {
        return target.ge(.sputm100);
    }

    pub fn firstLocalScript(target: Target) u16 {
        return if (target.le(.sputm98))
            200
        else
            2048;
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
        const lowercase = path[path.len - 2] & 0x20 != 0 or target.ge(.sputm100);
        path[path.len - 3] = '(';
        path[path.len - 2] = @as(u8, if (lowercase) 'a' else 'A') - 1 + disk_number;
        path[path.len - 1] = ')';
    }
};

/// change ".he0" to ".he4"
pub fn pointPathToMusic(path: []u8) void {
    if (builtin.mode == .Debug) {
        const ext = path[path.len - 4 ..];
        std.debug.assert(std.mem.eql(u8, ext, ".he0") or std.mem.eql(u8, ext, ".HE0"));
    }

    // Hardcode instances where the official releases had inconsistent filename
    // casing:
    if (Game.detectGame(path)) |game| switch (game) {
        .football_1999 => {
            for (path) |*c|
                c.* = std.ascii.toLower(c.*);
        },
        .baseball_2003 => {
            for (path[path.len - 3 ..]) |*c|
                c.* = std.ascii.toLower(c.*);
        },
        .soccer_2004 => {
            for (path) |*c|
                c.* = std.ascii.toLower(c.*);
        },
        else => {},
    };

    path[path.len - 1] = '4';
}
