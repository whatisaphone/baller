const builtin = @import("builtin");
const std = @import("std");

const Ast = @import("Ast.zig");
const Project = @import("Project.zig");
const BlockId = @import("block_id.zig").BlockId;
const Loc = @import("lexer.zig").Loc;

const Diagnostic = @This();

const live_spew = builtin.mode == .Debug;
pub const enable_trace = live_spew and !builtin.is_test;

pub const Location = struct {
    path: []const u8,
    loc: Loc,

    pub fn node(file: *const Project.SourceFile, node_index: Ast.NodeIndex) Location {
        const token_index = file.ast.node_tokens.get(node_index);
        const token = file.lex.tokens.at(token_index);
        return .{
            .path = file.path,
            .loc = token.span.start,
        };
    }
};

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
        .arena = .init(gpa),
        .messages = .{},
    };
}

pub fn deinit(self: *Diagnostic) void {
    self.arena.deinit();
}

pub fn err(self: *Diagnostic, comptime fmt: []const u8, args: anytype) void {
    self.formatAndAdd(.err, delayedFormat(fmt, args).thunk());
}

pub fn errAt(self: *Diagnostic, loc: ?Location, comptime fmt: []const u8, args: anytype) void {
    if (loc) |yes_loc| {
        const diag: ForTextFile = .init(self, yes_loc.path);
        diag.err(yes_loc.loc, fmt, args);
    } else {
        self.err(fmt, args);
    }
}

pub fn zigErr(self: *Diagnostic, comptime fmt: []const u8, args: anytype, zig_err: anytype) void {
    self.formatAndAdd(.err, delayedFormat(fmt, args ++ .{@errorName(zig_err)}).thunk());

    if (live_spew) if (@errorReturnTrace()) |err_trace|
        std.debug.dumpStackTrace(err_trace.*);
}

pub fn trace(self: *Diagnostic, comptime fmt: []const u8, args: anytype) void {
    if (!enable_trace) return;

    self.formatAndAdd(.trace, delayedFormat(fmt, args).thunk());
}

fn formatAndAdd(self: *Diagnostic, level: Level, message: FormatThunk) void {
    self.mutex.lock();
    defer self.mutex.unlock();

    const text = std.fmt.allocPrint(self.arena.allocator(), "{f}", .{message}) catch oom();
    self.add(level, .fromOwnedSlice(text));
}

fn add(self: *Diagnostic, level: Level, text: std.ArrayList(u8)) void {
    if (live_spew) {
        var buf: [4096]u8 = undefined;
        var writer = std.fs.File.stderr().writer(&buf);
        const w = &writer.interface;
        w.print("[{}] ", .{std.Thread.getCurrentId()}) catch @panic("spew");
        w.print("{s} {s}\n", .{ level.spewPrefix(), text.items }) catch @panic("spew");
        w.flush() catch @panic("spew");
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

pub fn writeToStderrAndPropagateIfAnyErrors(self: *const Diagnostic) !void {
    var buf: [4096]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buf);
    const out = &writer.interface;

    if (live_spew)
        try out.writeByte('\n');

    var total: std.EnumArray(Level, u32) = .initFill(0);
    var it = self.messages.constIterator(0);
    while (it.next()) |message| {
        try out.print("[{c}] {s}\n", .{ message.level.char(), message.text });
        total.set(message.level, total.get(message.level) + 1);
    }

    if (total.get(.err) != 0 or total.get(.info) != 0) {
        try out.print("{} error", .{total.get(.err)});
        if (total.get(.err) != 1)
            try out.writeByte('s');

        if (total.get(.info) != 0) {
            try out.print(", {} info", .{total.get(.info)});
            if (total.get(.info) != 1)
                try out.writeByte('s');
        }

        try out.writeByte('\n');
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
        self.formatAndAdd(.err, offset, delayedFormat(fmt, args).thunk());
    }

    pub fn zigErr(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
        zig_err: anytype,
    ) void {
        self.formatAndAdd(.err, offset, delayedFormat(fmt, args ++ .{@errorName(zig_err)}).thunk());

        if (live_spew) if (@errorReturnTrace()) |err_trace|
            std.debug.dumpStackTrace(err_trace.*);
    }

    pub fn info(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.formatAndAdd(.info, offset, delayedFormat(fmt, args).thunk());
    }

    pub fn trace(
        self: *const ForBinaryFile,
        offset: u32,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        if (!enable_trace) return;

        self.formatAndAdd(.trace, offset, delayedFormat(fmt, args).thunk());
    }

    fn formatAndAdd(
        self: *const ForBinaryFile,
        level: Level,
        offset: u32,
        message: FormatThunk,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        var text: std.io.Writer.Allocating = .init(self.diagnostic.arena.allocator());
        errdefer comptime unreachable;
        const writer = &text.writer;
        self.writeSection(writer) catch oom();
        writer.print(":0x{x:0>8}: {f}", .{ self.offset + offset, message }) catch oom();

        const effective_level: Level = if (self.cap_level and level == .err) .info else level;
        self.diagnostic.add(effective_level, text.toArrayList());
    }

    fn writeSection(self: *const ForBinaryFile, writer: *std.io.Writer) !void {
        if (self.parent) |parent| {
            try parent.writeSection(writer);
            try writer.writeByte(':');
        }
        switch (self.section) {
            .string => |s| try writer.writeAll(s),
            .block_id => |id| try writer.writeAll(id.str()),
            .glob => |id_and_num| try writer.print("{f}_{:0>4}", id_and_num),
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
        self.formatAndAdd(.err, loc, delayedFormat(fmt, args).thunk());
    }

    pub fn zigErr(
        self: *const ForTextFile,
        loc: Loc,
        comptime fmt: []const u8,
        args: anytype,
        zig_err: anytype,
    ) void {
        self.formatAndAdd(.err, loc, delayedFormat(fmt, args ++ .{@errorName(zig_err)}).thunk());

        if (live_spew) if (@errorReturnTrace()) |err_trace|
            std.debug.dumpStackTrace(err_trace.*);
    }

    fn formatAndAdd(
        self: *const ForTextFile,
        level: Level,
        loc: Loc,
        message: FormatThunk,
    ) void {
        self.diagnostic.mutex.lock();
        defer self.diagnostic.mutex.unlock();

        var text: std.io.Writer.Allocating = .init(self.diagnostic.arena.allocator());
        errdefer comptime unreachable;
        const w = &text.writer;
        w.print("{s}:{}:{}: {f}", .{ self.path, loc.line, loc.column, message }) catch oom();

        self.diagnostic.add(level, text.toArrayList());
    }
};

fn delayedFormat(comptime fmt: []const u8, args: anytype) DelayedFormat(fmt, @TypeOf(args)) {
    return .{ .args = args };
}

fn DelayedFormat(fmt: []const u8, Args: type) type {
    return struct {
        args: Args,

        fn thunk(self: *const @This()) FormatThunk {
            return .{ .f = formatOpaque, .cx = &self.args };
        }

        fn formatOpaque(cx: *const anyopaque, w: *std.io.Writer) !void {
            const args: *const Args = @ptrCast(@alignCast(cx));
            try w.print(fmt, args.*);
        }
    };
}

const FormatThunk = struct {
    f: *const fn (*const anyopaque, w: *std.io.Writer) std.io.Writer.Error!void,
    cx: *const anyopaque,

    pub fn format(self: FormatThunk, w: *std.io.Writer) !void {
        try self.f(self.cx, w);
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
