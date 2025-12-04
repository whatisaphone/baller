const std = @import("std");

const Deque = @import("deque.zig").Deque;
const utils = @import("utils.zig");

pub const max_concurrency = 32;

/// This differs from the one in std, in that it runs jobs in the same order
/// they're spawned.
pub const ThreadPool = struct {
    gpa: std.mem.Allocator,
    threads: utils.TinyArray(std.Thread, max_concurrency + 1),
    /// guards `queue`
    mutex: std.Thread.Mutex,
    queue: Deque(Job),
    terminate: std.atomic.Value(bool),
    wake: std.Thread.Condition,

    const Job = struct {
        run: *const fn (*const anyopaque) void,
        cx: *const anyopaque,
    };

    pub fn init(self: *ThreadPool, gpa: std.mem.Allocator, n_jobs: usize) !void {
        self.* = .{
            .gpa = gpa,
            .threads = .empty,
            .mutex = .{},
            .queue = .empty,
            .terminate = .init(false),
            .wake = .{},
        };
        errdefer self.deinit();

        for (0..n_jobs) |_| {
            const thread = try std.Thread.spawn(.{}, worker, .{self});
            self.threads.appendAssumeCapacity(thread);
        }
    }

    pub fn deinit(self: *ThreadPool) void {
        self.terminate.store(true, .monotonic);
        self.wake.broadcast();
        for (self.threads.slice()) |thread|
            thread.join();
        self.queue.deinit(self.gpa);
    }

    pub fn spawn(self: *ThreadPool, comptime func: anytype, args: anytype) !void {
        const Args = @TypeOf(args);

        const Runner = struct {
            pool: *const ThreadPool,
            args: Args,

            fn run(f: *const anyopaque) void {
                const runner: *const @This() = @ptrCast(@alignCast(f));
                @call(.auto, func, runner.args);
                runner.pool.gpa.destroy(runner);
            }
        };

        const runner = try self.gpa.create(Runner);
        errdefer self.gpa.destroy(runner);
        runner.* = .{ .pool = self, .args = args };

        {
            self.mutex.lock();
            defer self.mutex.unlock();
            try self.queue.pushBack(self.gpa, .{ .run = Runner.run, .cx = runner });
        }
        self.wake.signal();
    }

    fn worker(self: *ThreadPool) void {
        while (true) {
            const job = job: {
                self.mutex.lock();
                defer self.mutex.unlock();

                while (true) {
                    if (self.terminate.load(.monotonic))
                        return;
                    if (self.queue.popFront()) |job|
                        break :job job;
                    self.wake.wait(&self.mutex);
                }
            };
            job.run(job.cx);
        }
    }
};

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
        buffer: std.ArrayList(?T),
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
