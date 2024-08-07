const std = @import("std");

pub fn addUnsignedSigned(
    x: anytype,
    y: anytype,
) ?AddUnsignedSigned(@TypeOf(x), @TypeOf(y)) {
    const Result = AddUnsignedSigned(@TypeOf(x), @TypeOf(y));

    const wide_bits = @typeInfo(Result).Int.bits + 2;
    const Wide = @Type(.{ .Int = .{ .bits = wide_bits, .signedness = .signed } });

    // TODO: is there a better way to do this? this is mildly insane
    const result = @as(Wide, x) + @as(Wide, y);
    return std.math.cast(Result, result);
}

fn AddUnsignedSigned(X: type, Y: type) type {
    const xi = @typeInfo(X).Int;
    const yi = @typeInfo(Y).Int;
    std.debug.assert(xi.signedness == .unsigned);
    std.debug.assert(yi.signedness == .signed);
    const bits = @max(xi.bits, yi.bits);
    return @Type(.{ .Int = .{
        .signedness = .unsigned,
        .bits = bits,
    } });
}

pub fn growArrayList(
    T: type,
    xs: *std.ArrayListUnmanaged(T),
    allocator: std.mem.Allocator,
    minimum_len: usize,
    fill: T,
) !void {
    if (xs.items.len >= minimum_len)
        return;

    try xs.ensureTotalCapacity(allocator, minimum_len);
    @memset(xs.allocatedSlice()[xs.items.len..minimum_len], fill);
    xs.items.len = minimum_len;
}

pub fn growMultiArrayList(
    T: type,
    xs: *std.MultiArrayList(T),
    allocator: std.mem.Allocator,
    minimum_len: usize,
    fill: T,
) !void {
    if (xs.len >= minimum_len)
        return;

    // XXX: This could be more efficient by setting each field array all at once.
    try xs.ensureTotalCapacity(allocator, minimum_len);
    while (xs.len < minimum_len)
        xs.appendAssumeCapacity(fill);
}
