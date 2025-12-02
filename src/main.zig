const builtin = @import("builtin");
const std = @import("std");

const build_options = @import("build_options");

const build = @import("build.zig");
const dump = @import("dump.zig");
const extract = @import("extract.zig");
const saveload_dump = @import("saveload_dump.zig");
const talkie_build = @import("talkie_build.zig");
const talkie_extract = @import("talkie_extract.zig");

pub fn main() u8 {
    const err = if (runCli()) return 0 else |err| err;

    switch (err) {
        error.CommandLine => {
            std.fs.File.stderr().writeAll(usage) catch {};
        },
        error.CommandLineReported, error.Reported => {},
        else => { // TODO: handle all errors downstream, then get rid of this else
            std.fs.File.stderr().writeAll("unexpected error\n") catch {};
            if (builtin.mode == .Debug)
                if (@errorReturnTrace()) |trace|
                    std.debug.dumpStackTrace(trace.*);
        },
    }
    return 1;
}

fn runCli() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const allocator = if (std.valgrind.runningOnValgrind() != 0)
        // Valgrind relies on instrumenting the C allocator to detect leaks
        if (builtin.link_libc)
            std.heap.c_allocator
        else
            @panic("running under valgrind but compiled without valgrind support")
    else if (std.debug.runtime_safety)
        debug_allocator.allocator()
    else
        std.heap.smp_allocator;
    defer if (std.debug.runtime_safety)
        std.debug.assert(debug_allocator.deinit() == .ok);

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 1 + 1)
        return error.CommandLine;

    const command = args[1];

    if (std.mem.eql(u8, command, "help") or
        std.mem.eql(u8, command, "-h") or
        std.mem.eql(u8, command, "--help"))
    {
        try std.fs.File.stdout().writeAll(usage);
    } else if (std.mem.eql(u8, command, "version") or
        std.mem.eql(u8, command, "-v") or
        std.mem.eql(u8, command, "--version"))
    {
        try std.fs.File.stdout().writeAll(build_options.version);
    } else if (std.mem.eql(u8, command, "build")) {
        try build.runCli(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "dump")) {
        try dump.runCli(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "extract")) {
        try extract.runCli(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "saveload")) {
        if (args.len < 1 + 2)
            return error.CommandLine;
        const subcommand = args[2];
        if (std.mem.eql(u8, subcommand, "dump")) {
            try saveload_dump.runCli(allocator, args[3..]);
        } else {
            return error.CommandLine;
        }
    } else if (std.mem.eql(u8, command, "talkie")) {
        if (args.len < 1 + 2)
            return error.CommandLine;
        const subcommand = args[2];
        if (std.mem.eql(u8, subcommand, "build")) {
            try talkie_build.runCli(allocator, args[3..]);
        } else if (std.mem.eql(u8, subcommand, "extract")) {
            try talkie_extract.runCli(allocator, args[3..]);
        } else {
            return error.CommandLine;
        }
    } else {
        return error.CommandLine;
    }
}

pub const panic = if (std.debug.runtime_safety)
    // this is the default, as seen in std.builtin.panic
    std.debug.FullPanic(std.debug.defaultPanic)
else
    // in release builds, just abort. this is here to avoid bloating the binary
    // with code for printing stack traces. based on std.debug.no_panic
    struct {
        pub fn call(_: []const u8, _: ?usize) noreturn {
            @branchHint(.cold);
            std.fs.File.stderr().writeAll("unexpected error\n") catch {};
            std.process.abort();
        }

        pub fn sentinelMismatch(_: anytype, _: anytype) noreturn {
            call(undefined, undefined);
        }

        pub fn unwrapError(_: anyerror) noreturn {
            call(undefined, undefined);
        }

        pub fn outOfBounds(_: usize, _: usize) noreturn {
            call(undefined, undefined);
        }

        pub fn startGreaterThanEnd(_: usize, _: usize) noreturn {
            call(undefined, undefined);
        }

        pub fn inactiveUnionField(_: anytype, _: anytype) noreturn {
            call(undefined, undefined);
        }

        pub fn sliceCastLenRemainder(_: usize) noreturn {
            call(undefined, undefined);
        }

        pub fn reachedUnreachable() noreturn {
            call(undefined, undefined);
        }

        pub fn unwrapNull() noreturn {
            call(undefined, undefined);
        }

        pub fn castToNull() noreturn {
            call(undefined, undefined);
        }

        pub fn incorrectAlignment() noreturn {
            call(undefined, undefined);
        }

        pub fn invalidErrorCode() noreturn {
            call(undefined, undefined);
        }

        pub fn integerOutOfBounds() noreturn {
            call(undefined, undefined);
        }

        pub fn integerOverflow() noreturn {
            call(undefined, undefined);
        }

        pub fn shlOverflow() noreturn {
            call(undefined, undefined);
        }

        pub fn shrOverflow() noreturn {
            call(undefined, undefined);
        }

        pub fn divideByZero() noreturn {
            call(undefined, undefined);
        }

        pub fn exactDivisionRemainder() noreturn {
            call(undefined, undefined);
        }

        pub fn integerPartOutOfBounds() noreturn {
            call(undefined, undefined);
        }

        pub fn corruptSwitch() noreturn {
            call(undefined, undefined);
        }

        pub fn shiftRhsTooBig() noreturn {
            call(undefined, undefined);
        }

        pub fn invalidEnumValue() noreturn {
            call(undefined, undefined);
        }

        pub fn forLenMismatch() noreturn {
            call(undefined, undefined);
        }

        pub fn copyLenMismatch() noreturn {
            call(undefined, undefined);
        }

        pub fn memcpyAlias() noreturn {
            call(undefined, undefined);
        }

        pub fn noreturnReturned() noreturn {
            call(undefined, undefined);
        }
    };

const usage =
    \\Baller <https://github.com/whatisaphone/baller> licensed under AGPL 3.0
    \\
    \\A modding tool for Backyard Sports games.
    \\
    \\Usage:
    \\
    \\baller help: Show help
    \\
    \\baller version: Show version
    \\
    \\baller extract <index> <output>
    \\
    \\    <index>       Path to index file ending in .he0
    \\    <output>      Path to output directory
    \\    --symbols=
    \\    --script=disassemble|decompile
    \\    --annotate=yes|no
    \\    --music=yes|no
    \\    --rmim=raw|decode
    \\    --scrp=raw|decode
    \\    --encd=raw|decode
    \\    --excd=raw|decode
    \\    --lscr=raw|decode
    \\    --lsc2=raw|decode
    \\    --obim=raw|decode
    \\    --obcd=raw|decode
    \\    --digi=raw|decode
    \\    --talk=raw|decode
    \\    --awiz=raw|decode
    \\    --mult=raw|decode
    \\    --akos=raw|decode
    \\    --tlke=raw|decode
    \\
    \\baller build <project> <output>
    \\
    \\    <project>     Path to project.txt, typically generated by baller
    \\                  extract
    \\    <output>      Path to output file ending in .he0
    \\    --awiz=original|max
    \\    --write-version=yes|no
    \\
    \\baller talkie extract <input> <output>
    \\
    \\    <input>       Path to talkie file ending in .he2
    \\    <output>      Path to output directory
    \\
    \\baller talkie build <manifest> <output>
    \\
    \\    <manifest>    Path to talkies.txt, typically generated by baller
    \\                  talkie extract
    \\    <output>      Path to output file ending in .he2
    \\
    \\baller dump <output>
    \\
    \\                  (reads from stdin)
    \\    <output>      Path to output directory
    \\    -x=|--xor=    XOR key
    \\
    \\baller saveload dump <index> <savegame>
    \\
    \\                  (writes to stdout)
    \\    <index>       Path to index file ending in .he0
    \\    <savegame>    Path to savegame file, usually ending in .sg
    \\
;

comptime {
    // Sorry, IBM mainframe users
    std.debug.assert(builtin.cpu.arch.endian() == .little);
}

test {
    _ = @import("tests.zig");
    _ = @import("tests2.zig");
    _ = @import("tests_compile_errors.zig");
    _ = @import("tests_decompile.zig");
}
