const builtin = @import("builtin");
const std = @import("std");

const BlockId = @import("block_id.zig").BlockId;
const Loc = @import("lexer.zig").Loc;

const Diagnostic = @This();

const live_spew = builtin.mode == .Debug;
pub const enable_trace = live_spew and !builtin.is_test;

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
            .trace => unreachable,
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

mutex: Mutex,
arena: std.heap.ArenaAllocator,
messages: std.SegmentedList(Message, 4),

pub fn init(gpa: std.mem.Allocator) Diagnostic {
    return .{
        .mutex = .init,
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

    if (live_spew) if (@errorReturnTrace()) |err_trace|
        std.debug.dumpStackTrace(err_trace.*);
}

pub fn trace(self: *Diagnostic, comptime fmt: []const u8, args: anytype) void {
    if (!enable_trace) return;

    self.mutex.lock();
    defer self.mutex.unlock();

    self.formatAndAdd(.trace, fmt, args);
}

fn formatAndAdd(self: *Diagnostic, level: Level, comptime fmt: []const u8, args: anytype) void {
    const text = std.fmt.allocPrint(self.arena.allocator(), fmt, args) catch oom();
    self.add(level, .fromOwnedSlice(text));
}

fn add(self: *Diagnostic, level: Level, text: std.ArrayListUnmanaged(u8)) void {
    if (live_spew) {
        var buf = std.io.bufferedWriter(std.io.getStdErr().writer());
        buf.writer().print("[{}] ", .{std.Thread.getCurrentId()}) catch @panic("spew");
        buf.writer().print("{s} {s}\n", .{ level.spewPrefix(), text.items }) catch @panic("spew");
        buf.flush() catch @panic("spew");
    }

    // traces are not stored, only spewed
    if (level == .trace) {
        const old_end = self.arena.state.end_index;
        var text_mut = text;
        text_mut.deinit(self.arena.allocator());
        // assert the memory was actually freed
        std.debug.assert(self.arena.state.end_index != old_end);
        return;
    }

    self.messages.append(self.arena.allocator(), .{
        .level = level,
        .text = text.items,
    }) catch oom();
}

pub fn writeToStderrAndPropagateIfAnyErrors(self: *Diagnostic) !void {
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
    parent: ?*const ForBinaryFile,
    offset: u32,
    section: Section,
    cap_level: bool,

    const Section = union(enum) {
        string: []const u8,
        block_id: BlockId,
        glob: struct { BlockId, u16 },
    };

    pub fn init(diagnostic: *Diagnostic, string: []const u8) ForBinaryFile {
        return .{
            .diagnostic = diagnostic,
            .parent = null,
            .offset = 0,
            .section = .{ .string = string },
            .cap_level = false,
        };
    }

    pub fn child(self: *const ForBinaryFile, offset: u32, section: Section) ForBinaryFile {
        return .{
            .diagnostic = self.diagnostic,
            .parent = self,
            .offset = self.offset + offset,
            .section = section,
            .cap_level = false,
        };
    }

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
        var text: std.ArrayListUnmanaged(u8) = .empty;
        const writer = text.writer(self.diagnostic.arena.allocator());
        self.writeSection(writer) catch oom();
        writer.print(":0x{x:0>8}: ", .{self.offset + offset}) catch oom();
        writer.print(fmt, args) catch oom();

        const effective_level: Level = if (self.cap_level and level == .err) .info else level;
        self.diagnostic.add(effective_level, text);
    }

    fn writeSection(self: *const ForBinaryFile, writer: anytype) !void {
        if (self.parent) |parent| {
            try parent.writeSection(writer);
            try writer.writeByte(':');
        }
        switch (self.section) {
            .string => |s| try writer.writeAll(s),
            .block_id => |id| try writer.writeAll(id.str()),
            .glob => |id_and_num| try writer.print("{}_{:0>4}", id_and_num),
        }
    }
};

pub const ForTextFile = struct {
    diagnostic: *Diagnostic,
    path: []const u8,

    pub fn init(diagnostic: *Diagnostic, path: []const u8) ForTextFile {
        return .{
            .diagnostic = diagnostic,
            .path = path,
        };
    }

    pub fn err(
        self: *const ForTextFile,
        loc: Loc,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        self.formatAndAdd(.err, loc, fmt, args);
    }

    pub fn zigErr(
        self: *const ForTextFile,
        loc: Loc,
        comptime fmt: []const u8,
        args: anytype,
        zig_err: anytype,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        self.formatAndAdd(.err, loc, fmt, args ++ .{@errorName(zig_err)});

        if (live_spew) if (@errorReturnTrace()) |err_trace|
            std.debug.dumpStackTrace(err_trace.*);
    }

    pub fn formatAndAdd(
        self: *const ForTextFile,
        level: Level,
        loc: Loc,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        const text_count =
            std.fmt.count("{s}:{}:{}: ", .{ self.path, loc.line, loc.column }) +
            std.fmt.count(fmt, args);
        const text = self.diagnostic.arena.allocator().alloc(u8, text_count) catch oom();
        var fba = std.heap.FixedBufferAllocator.init(text);
        _ = std.fmt.allocPrint(
            fba.allocator(),
            "{s}:{}:{}: ",
            .{ self.path, loc.line, loc.column },
        ) catch unreachable;
        _ = std.fmt.allocPrint(fba.allocator(), fmt, args) catch unreachable;
        std.debug.assert(fba.end_index == text_count);

        self.diagnostic.add(level, .fromOwnedSlice(text));
    }
};

// In debug builds, integrate with debug lock so logs aren't interleaved with
// panics. In release builds, use our own mutex to avoid bringing in
// std.Progress.
const Mutex = if (builtin.mode == .Debug)
    struct {
        const init: Mutex = .{};

        fn lock(_: Mutex) void {
            std.debug.lockStdErr();
        }

        fn unlock(_: Mutex) void {
            std.debug.unlockStdErr();
        }
    }
else
    struct {
        mutex: std.Thread.Mutex,

        const init: Mutex = .{ .mutex = .{} };

        fn lock(self: *Mutex) void {
            self.mutex.lock();
        }

        fn unlock(self: *Mutex) void {
            self.mutex.unlock();
        }
    };
