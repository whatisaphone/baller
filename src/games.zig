const std = @import("std");

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

pub fn detectGameOrFatal(index_path: []const u8) !Game {
    const input_name = std.fs.path.basename(index_path);
    return if (std.mem.eql(u8, input_name, "BASEBALL.HE0"))
        .baseball_1997
    else if (std.mem.eql(u8, input_name, "SOCCER.HE0"))
        .soccer_1998
    else if (std.mem.eql(u8, input_name, "FOOTBALL.HE0"))
        .football_1999
    else if (std.mem.eql(u8, input_name, "baseball 2001.he0"))
        .baseball_2001
    else if (std.mem.eql(u8, input_name, "Basketball.he0"))
        .basketball
    else {
        report.fatal("could not detect game", .{});
        return error.Fatal;
    };
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

pub fn directoryNonPresentLen(game: Game) u32 {
    return if (game == .baseball_1997)
        0xffff_ffff
    else
        0;
}

pub fn writeMultLen(game: Game) bool {
    return game != .baseball_1997;
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

pub fn firstLocalScript(game: Game) u16 {
    return if (@intFromEnum(game) <= @intFromEnum(Game.soccer_1998))
        200
    else
        2048;
}
