const std = @import("std");

pub const null_allocator = std.mem.Allocator{
    .ptr = undefined,
    .vtable = &.{
        .alloc = struct {
            fn alloc(self: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
                _ = .{ self, len, alignment, ret_addr };
                return null;
            }
        }.alloc,

        .resize = struct {
            fn resize(self: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
                _ = .{ self, memory, alignment, new_len, ret_addr };
                return false;
            }
        }.resize,

        .remap = struct {
            fn remap(self: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
                _ = .{ self, memory, alignment, new_len, ret_addr };
                return null;
            }
        }.remap,

        .free = struct {
            fn free(self: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
                _ = .{ self, memory, alignment, ret_addr };
            }
        }.free,
    },
};

pub fn addUnsignedSigned(
    x: anytype,
    y: anytype,
) ?AddUnsignedSigned(@TypeOf(x), @TypeOf(y)) {
    const Result = AddUnsignedSigned(@TypeOf(x), @TypeOf(y));

    const wide_bits = @typeInfo(Result).int.bits + 2;
    const Wide = @Type(.{ .int = .{ .bits = wide_bits, .signedness = .signed } });

    // TODO: is there a better way to do this? this is mildly insane
    const result = @as(Wide, x) + @as(Wide, y);
    return std.math.cast(Result, result);
}

fn AddUnsignedSigned(X: type, Y: type) type {
    const xi = @typeInfo(X).int;
    const yi = @typeInfo(Y).int;
    std.debug.assert(xi.signedness == .unsigned);
    std.debug.assert(yi.signedness == .signed);
    const bits = @max(xi.bits, yi.bits);
    return @Type(.{ .int = .{
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
