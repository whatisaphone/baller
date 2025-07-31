const std = @import("std");

pub fn Key(Template: type) type {
    return enum(Index) {
        const Self = @This();

        const Index = @typeInfo(Template).@"enum".tag_type;
        comptime {
            std.debug.assert(@typeInfo(Template).@"enum".fields.len == 0);
        }

        _,

        pub fn fromIndex(idx: Index) Self {
            return @enumFromInt(idx);
        }

        pub fn fromUsize(idx: usize) Self {
            return .fromIndex(@intCast(idx));
        }

        pub fn index(self: Self) Index {
            return @intFromEnum(self);
        }

        pub fn wrap(self: Self) Optional {
            return .fromIndex(self.index());
        }

        pub const Optional = enum(Index) {
            null = std.math.maxInt(Index),
            _,

            fn fromIndex(idx: Index) Optional {
                std.debug.assert(idx != @intFromEnum(Optional.null));
                return @enumFromInt(idx);
            }

            pub fn index(self: Optional) Index {
                return @intFromEnum(self);
            }

            pub fn unwrap(self: Optional) ?Self {
                return switch (self) {
                    .null => null,
                    else => .fromIndex(self.index()),
                };
            }
        };
    };
}

/// A dense array, like `ArrayList`, but indices are a wrapper type instead of
/// `usize`.
pub fn List(K: type, V: type) type {
    return struct {
        const Self = @This();

        list: std.ArrayListUnmanaged(V),

        pub const empty: Self = .{ .list = .empty };

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            self.list.deinit(gpa);
        }

        pub fn len(self: *const Self) K.Index {
            return @intCast(self.list.items.len);
        }

        pub fn at(self: anytype, key: K) switch (@TypeOf(self)) {
            *const Self => *const V,
            *Self => *V,
            else => unreachable,
        } {
            return &self.list.items[key.index()];
        }

        pub fn get(self: *const Self, key: K) V {
            return self.at(key).*;
        }

        pub fn append(self: *Self, gpa: std.mem.Allocator, value: V) !K {
            const key: K = .fromUsize(self.len());
            try self.list.append(gpa, value);
            return key;
        }

        pub fn ensureTotalCapacity(
            self: *Self,
            gpa: std.mem.Allocator,
            new_capacity: usize,
        ) !void {
            try self.list.ensureTotalCapacity(gpa, new_capacity);
        }
    };
}
