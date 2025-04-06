const std = @import("std");

pub fn Channel(T: type, capacity: usize) type {
    return struct {
        const Self = @This();

        // yeah this could be better
        mutex: std.Thread.Mutex,
        queue: std.BoundedArray(T, capacity),
        queue_not_empty: std.Thread.Condition,
        queue_not_full: std.Thread.Condition,

        pub const init: Self = .{
            .mutex = .{},
            .queue = .{},
            .queue_not_empty = .{},
            .queue_not_full = .{},
        };

        pub fn send(self: *Self, value: T) void {
            {
                self.mutex.lock();
                defer self.mutex.unlock();

                while (self.queue.len == self.queue.capacity())
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
