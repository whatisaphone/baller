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
};

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

// TODO: this should be determined from the index DISK block (or lack thereof)
pub fn numberOfDisks(game: Game) u8 {
    return switch (game) {
        .football_1999, .baseball_2001, .basketball => 2,
        else => 1,
    };
}

pub fn pointPathToDisk(game: Game, path: []u8, disk_number: u8) void {
    switch (game) {
        .baseball_1997 => {
            std.debug.assert(disk_number == 1);
            // change ".HE0" to ".HE1"
            path[path.len - 1] = '1';
        },
        .soccer_1998, .football_1999 => {
            // change ".he0" to e.g. ".(A)"
            path[path.len - 3] = '(';
            path[path.len - 2] = 'A' - 1 + disk_number;
            path[path.len - 1] = ')';
        },
        .baseball_2001, .basketball => {
            // change ".he0" to e.g. ".(a)"
            path[path.len - 3] = '(';
            path[path.len - 2] = 'a' - 1 + disk_number;
            path[path.len - 1] = ')';
        },
    }
}

pub fn firstLocalScript(game: Game) u16 {
    return if (@intFromEnum(game) <= @intFromEnum(Game.soccer_1998))
        200
    else
        2048;
}
