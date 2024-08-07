const std = @import("std");

const utils = @import("utils.zig");

pub fn ArrayMap(V: type) type {
    return struct {
        const Self = @This();

        items: std.ArrayListUnmanaged(?V) = .{},

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.items.deinit(allocator);
        }

        pub fn len(self: *const Self) usize {
            return self.items.items.len;
        }

        pub fn get(self: *const Self, index: usize) ?V {
            const ptr = self.getPtr(index) orelse return null;
            return ptr.*;
        }

        pub fn getPtr(self: *const Self, index: usize) ?*V {
            if (index >= self.items.items.len)
                return null;
            const ptr = &self.items.items[index];
            if (ptr.* == null)
                return null;
            return &ptr.*.?;
        }

        pub fn getOrPut(self: *Self, allocator: std.mem.Allocator, index: usize) !*?V {
            try utils.growArrayList(?V, &self.items, allocator, index + 1, null);
            return &self.items.items[index];
        }

        pub fn putNew(self: *Self, allocator: std.mem.Allocator, index: usize, value: V) !void {
            const ptr = try self.getOrPut(allocator, index);
            if (ptr.* != null)
                return error.AlreadyExists;
            ptr.* = value;
        }
    };
}
