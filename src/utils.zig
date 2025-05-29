const std = @import("std");

pub const null_allocator: std.mem.Allocator = .{
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

pub fn hasGuaranteedLayout(T: type) bool {
    return comptime switch (@typeInfo(T)) {
        .int => |i| i.bits >= 8 and std.math.isPowerOfTwo(i.bits),
        .array => |a| hasGuaranteedLayout(a.child),
        .@"struct" => |s| s.layout == .@"extern",
        else => @compileError("TODO"),
    };
}

pub fn add(T: type, a: anytype, b: anytype) ?T {
    const Wide = std.math.IntFittingRange(
        std.math.minInt(@TypeOf(a)) + std.math.minInt(@TypeOf(b)),
        std.math.maxInt(@TypeOf(a)) + std.math.maxInt(@TypeOf(b)),
    );
    return std.math.cast(T, @as(Wide, a) + @as(Wide, b));
}

pub fn SafeUndefined(T: type) type {
    return union {
        undefined: void,
        defined: T,

        pub const undef: @This() = .{ .undefined = {} };

        pub fn setOnce(self: *@This(), value: T) void {
            _ = self.undefined;
            self.* = .{ .defined = value };
        }
    };
}

pub fn SafeManyPointer(ManyPtr: type) type {
    const many_ptr_info = @typeInfo(ManyPtr).pointer;
    std.debug.assert(many_ptr_info.size == .many);
    const Element = many_ptr_info.child;

    var single_ptr_info = many_ptr_info;
    single_ptr_info.size = .one;
    const SinglePtr = @Type(.{ .pointer = single_ptr_info });

    var slice_info = many_ptr_info;
    slice_info.size = .slice;
    const Slice = @Type(.{ .pointer = slice_info });

    const store_len = std.debug.runtime_safety;

    return struct {
        const Self = @This();

        ptr: ManyPtr,
        len: if (store_len) usize else void,

        pub const empty: Self = .init(&.{});

        pub fn init(items: Slice) Self {
            return .{
                .ptr = items.ptr,
                .len = if (store_len) items.len,
            };
        }

        pub fn use(self: Self) if (store_len) Slice else ManyPtr {
            return if (store_len)
                self.ptr[0..self.len]
            else
                self.ptr;
        }

        pub fn slice(self: Self, len: usize) Slice {
            if (store_len)
                std.debug.assert(len == self.len);
            return self.ptr[0..len];
        }

        pub fn get(self: Self, index: usize) Element {
            return self.use()[index];
        }

        pub fn getPtr(self: Self, index: usize) SinglePtr {
            return &self.use()[index];
        }

        pub fn plus(self: Self, count: usize) Self {
            const ptr = self.ptr + count;
            const len = if (store_len) self.len - count;
            return .{ .ptr = ptr, .len = len };
        }
    };
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

pub fn growBoundedArray(
    /// `*std.BoundedArray(any, any)`
    xs: anytype,
    minimum_len: usize,
    fill: @typeInfo(@FieldType(@typeInfo(@TypeOf(xs)).pointer.child, "buffer")).array.child,
) void {
    if (xs.len >= minimum_len) return;
    @memset(xs.buffer[xs.len..minimum_len], fill);
    xs.len = minimum_len;
}
