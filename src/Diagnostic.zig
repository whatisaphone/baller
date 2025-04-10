const builtin = @import("builtin");
const std = @import("std");

const Diagnostic = @This();

const enable_trace = builtin.mode == .Debug;
const live_spew = builtin.mode == .Debug;

const Message = struct {
    level: Level,
    text: []const u8,
};

const Level = enum {
    err,
    info,
    trace,

    fn char(self: Level) u8 {
        return switch (self) {
            .err => 'E',
            .info => 'I',
            .trace => 'T',
        };
    }

    fn spewPrefix(self: Level) []const u8 {
        return switch (self) {
            .err => "[E]",
            .info => "[I]",
            .trace => "   ",
        };
    }
};

mutex: std.Thread.Mutex,
arena: std.heap.ArenaAllocator,
messages: std.SegmentedList(Message, 4),

pub fn init(gpa: std.mem.Allocator) Diagnostic {
    return .{
        .mutex = .{},
        .arena = std.heap.ArenaAllocator.init(gpa),
        .messages = .{},
    };
}

pub fn deinit(self: *Diagnostic) void {
    self.arena.deinit();
}

pub fn err(self: *Diagnostic, comptime fmt: []const u8, args: anytype) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    self.formatAndAdd(.err, fmt, args);
}

pub fn zigErr(self: *Diagnostic, comptime fmt: []const u8, args: anytype, zig_err: anytype) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    self.formatAndAdd(.err, fmt, args ++ .{@errorName(zig_err)});

    if (live_spew) if (@errorReturnTrace()) |trace|
        std.debug.dumpStackTrace(trace.*);
}

fn formatAndAdd(self: *Diagnostic, level: Level, comptime fmt: []const u8, args: anytype) void {
    const text = std.fmt.allocPrint(self.arena.allocator(), fmt, args) catch oom();
    self.add(level, text);
}

fn add(self: *Diagnostic, level: Level, text: []const u8) void {
    if (live_spew) {
        var buf = std.io.bufferedWriter(std.io.getStdErr().writer());
        buf.writer().print("[{}] ", .{std.Thread.getCurrentId()}) catch @panic("spew");
        buf.writer().print("{s} {s}\n", .{ level.spewPrefix(), text }) catch @panic("spew");
        buf.flush() catch @panic("spew");
    }

    // traces are not stored, only spewed
    if (level == .trace) {
        ensureFree(&self.arena, text);
        return;
    }

    self.messages.append(self.arena.allocator(), .{
        .level = level,
        .text = text,
    }) catch oom();
}

fn ensureFree(arena: *std.heap.ArenaAllocator, memory: anytype) void {
    const old_end = arena.state.end_index;
    arena.allocator().free(memory);
    std.debug.assert(arena.state.end_index != old_end);
}

pub fn writeToStderr(self: *Diagnostic) !void {
    const out_file = std.io.getStdErr();
    var out = std.io.bufferedWriter(out_file.writer());

    if (live_spew)
        try out.writer().writeByte('\n');

    var total: std.EnumArray(Level, u32) = .initFill(0);
    var it = self.messages.iterator(0);
    while (it.next()) |message| {
        try out.writer().print("[{c}] {s}\n", .{ message.level.char(), message.text });
        total.set(message.level, total.get(message.level) + 1);
    }

    if (total.get(.err) != 0 or total.get(.info) != 0) {
        try out.writer().print("{} error", .{total.get(.err)});
        if (total.get(.err) != 1)
            try out.writer().writeByte('s');

        if (total.get(.info) != 0) {
            try out.writer().print(", {} info", .{total.get(.info)});
            if (total.get(.info) != 1)
                try out.writer().writeByte('s');
        }

        try out.writer().writeByte('\n');
        try out.flush();
    }

    if (total.get(.err) != 0)
        return error.Reported;
}

fn oom() noreturn {
    @panic("OutOfMemory");
}

pub const ForBinaryFile = struct {
    diagnostic: *Diagnostic,
    path: []const u8,
    offset: u32,
    cap_level: bool,

    pub fn err(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        self.formatAndAdd(.err, offset, fmt, args);
    }

    pub fn zigErr(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
        zig_err: anytype,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        self.formatAndAdd(.err, offset, fmt, args ++ .{@errorName(zig_err)});

        if (live_spew) if (@errorReturnTrace()) |err_trace|
            std.debug.dumpStackTrace(err_trace.*);
    }

    pub fn info(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        self.formatAndAdd(.info, offset, fmt, args);
    }

    pub fn trace(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        if (!enable_trace) return;

        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        self.formatAndAdd(.trace, offset, fmt, args);
    }

    fn formatAndAdd(
        self: *const ForBinaryFile,
        level: Level,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        const text_count =
            std.fmt.count("{s}:{x:0>8}: ", .{ self.path, self.offset + offset }) +
            std.fmt.count(fmt, args);
        const text = self.diagnostic.arena.allocator().alloc(u8, text_count) catch oom();
        var fba = std.heap.FixedBufferAllocator.init(text);
        _ = std.fmt.allocPrint(
            fba.allocator(),
            "{s}:{x:0>8}: ",
            .{ self.path, self.offset + offset },
        ) catch unreachable;
        _ = std.fmt.allocPrint(fba.allocator(), fmt, args) catch unreachable;
        std.debug.assert(fba.end_index == text_count);

        const effective_level: Level = if (self.cap_level and level == .err) .info else level;
        self.diagnostic.add(effective_level, text);
    }
};

pub const ForTextFile = struct {
    diagnostic: *Diagnostic,
    path: []const u8,

    pub fn err(
        self: *const ForTextFile,
        line: u32,
        column: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.formatAndAdd(.err, line, column, fmt, args);
    }

    pub fn formatAndAdd(
        self: *const ForTextFile,
        level: Level,
        line: u32,
        column: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        const text_count =
            std.fmt.count("{s}:{}:{}: ", .{ self.path, line, column }) +
            std.fmt.count(fmt, args);
        const text = self.diagnostic.arena.allocator().alloc(u8, text_count) catch oom();
        var fba = std.heap.FixedBufferAllocator.init(text);
        _ = std.fmt.allocPrint(
            fba.allocator(),
            "{s}:{}:{}: ",
            .{ self.path, line, column },
        ) catch unreachable;
        _ = std.fmt.allocPrint(fba.allocator(), fmt, args) catch unreachable;
        std.debug.assert(fba.end_index == text_count);

        self.diagnostic.add(level, text);
    }
};
