const std = @import("std");

const games = @import("games.zig");
const lang = @import("lang.zig");

const UsageTracker = @This();

pub const max_global_vars = 768;
pub const max_local_vars = 32;
pub const max_room_vars = 192;

pub const GlobalVars = BitArray(max_global_vars);
pub const LocalVars = BitArray(max_local_vars);
pub const RoomVars = BitArray(max_room_vars);

fn BitArray(bits: comptime_int) type {
    const len = std.math.divCeil(comptime_int, bits, @bitSizeOf(usize)) catch unreachable;
    return [len]usize;
}

game: games.Game,
global_vars: GlobalVars,
local_vars: LocalVars,
room_vars: RoomVars,

pub fn init(game: games.Game) UsageTracker {
    return .{
        .game = game,
        .global_vars = @splat(0),
        .local_vars = @splat(0),
        .room_vars = @splat(0),
    };
}

pub fn track(self: *UsageTracker, variable: lang.Variable) !void {
    const kind, const number = try variable.decode2();
    const max: u16 = switch (kind) {
        .global => max_global_vars,
        .local => max_local_vars,
        .room => max_room_vars,
    };
    if (number >= max) return error.BadData;
    const buf = switch (kind) {
        .global => &self.global_vars,
        .local => &self.local_vars,
        .room => &self.room_vars,
    };
    std.mem.writePackedInt(u1, std.mem.sliceAsBytes(buf), number, 1, .little);
}

pub fn get(buf: []const usize, index: usize) bool {
    return std.mem.readPackedInt(u1, std.mem.sliceAsBytes(buf), index, .little) != 0;
}

/// `lhs` is accessed atomically, `rhs` is not
pub fn atomicUnion(lhs: []usize, rhs: []const usize) void {
    for (lhs, rhs) |*a, b|
        _ = @atomicRmw(usize, a, .Or, b, .monotonic);
}
