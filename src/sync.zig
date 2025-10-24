const std = @import("std");

const utils = @import("utils.zig");

pub fn Channel(T: type, capacity: usize) type {
    return struct {
        const Self = @This();

        // yeah this could be better
        mutex: std.Thread.Mutex,
        queue: utils.TinyArray(T, capacity),
        queue_not_empty: std.Thread.Condition,
        queue_not_full: std.Thread.Condition,

        pub const init: Self = .{
            .mutex = .{},
            .queue = .empty,
            .queue_not_empty = .{},
            .queue_not_full = .{},
        };

        pub fn send(self: *Self, value: T) void {
            {
                self.mutex.lock();
                defer self.mutex.unlock();

                while (self.queue.len == self.queue.theCapacity())
                    self.queue_not_full.wait(&self.mutex);

                self.queue.append(value) catch unreachable;
            }
            self.queue_not_empty.signal();
        }

        pub fn receive(self: *Self) T {
            const result = result: {
                self.mutex.lock();
                defer self.mutex.unlock();

                while (self.queue.len == 0)
                    self.queue_not_empty.wait(&self.mutex);

                break :result self.queue.orderedRemove(0);
            };
            self.queue_not_full.signal();
            return result;
        }
    };
}

pub fn OrderedReceiver(T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        channel: *Channel(OrderedEvent(T), capacity),
        // TODO: optimize mem usage
        buffer: std.ArrayListUnmanaged(?T),
        index: u16,

        pub fn init(channel: *Channel(OrderedEvent(T), capacity)) Self {
            return .{
                .channel = channel,
                .buffer = .empty,
                .index = 0,
            };
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            self.buffer.deinit(gpa);
        }

        pub fn next(self: *Self, gpa: std.mem.Allocator) !T {
            while (true) {
                if (self.index < self.buffer.items.len) {
                    if (self.buffer.items[self.index]) |result| {
                        self.index += 1;
                        return result;
                    }
                }

                try self.receive(gpa);
            }
        }

        fn receive(self: *Self, gpa: std.mem.Allocator) !void {
            const event = self.channel.receive();
            try utils.growArrayList(?T, &self.buffer, gpa, event.index + 1, null);
            std.debug.assert(self.buffer.items[event.index] == null);
            self.buffer.items[event.index] = event.payload;
        }
    };
}

pub fn OrderedEvent(T: type) type {
    return struct {
        index: u16,
        payload: T,
    };
}
