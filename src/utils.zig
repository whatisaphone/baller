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
