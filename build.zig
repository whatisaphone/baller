const std = @import("std");

const version = "0.6.4";

pub fn build(b: *std.Build) void {
    const test_filters = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match any filter",
    ) orelse &.{};

    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    /////////
    // exe

    {
        const exe = b.addExecutable(.{
            .name = "baller",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });

        const options = b.addOptions();
        options.addOption([]const u8, "version", "dev");
        exe.root_module.addOptions("build_options", options);

        b.installArtifact(exe);

        const run = b.addRunArtifact(exe);
        run.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run.addArgs(args);
        }

        const step = b.step("run", "Run the app");
        step.dependOn(&run.step);
    }

    //////////////////
    // exe+valgrind

    {
        const exe = b.addExecutable(.{
            .name = "baller",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = baselineTarget(b, &target.query),
                .optimize = optimize,
                .link_libc = true,
            }),
            .use_llvm = true, // https://codeberg.org/ziglang/zig/issues/31272
        });

        const options = b.addOptions();
        options.addOption([]const u8, "version", "dev");
        exe.root_module.addOptions("build_options", options);

        const run = b.addSystemCommand(&.{
            "valgrind",
            "--leak-check=full",
            "--error-exitcode=1",
            "--exit-on-first-error=yes",
            "--track-origins=yes",
            "--",
        });
        run.addArtifactArg(exe);
        if (b.args) |args| {
            run.addArgs(args);
        }

        const step = b.step("run:valgrind", "Run the app under valgrind");
        step.dependOn(&run.step);
    }

    ///////////
    // tests

    {
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
            .filters = test_filters,
        });

        const options = b.addOptions();
        options.addOption([]const u8, "version", "test");
        tests.root_module.addOptions("build_options", options);

        const run = b.addRunArtifact(tests);
        run.has_side_effects = true;

        const step = b.step("test", "Run unit tests");
        step.dependOn(&run.step);
    }

    ////////////////////
    // tests+valgrind

    {
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = baselineTarget(b, &target.query),
                .optimize = optimize,
                .link_libc = true,
            }),
            .use_llvm = true, // https://codeberg.org/ziglang/zig/issues/31272
            .filters = test_filters,
        });

        const options = b.addOptions();
        options.addOption([]const u8, "version", "test");
        tests.root_module.addOptions("build_options", options);

        const run = b.addSystemCommand(&.{
            "valgrind",
            "--leak-check=full",
            "--error-exitcode=1",
            "--exit-on-first-error=yes",
            "--track-origins=yes",
            "--",
        });
        run.addFileArg(tests.getEmittedBin());

        const step = b.step("test:valgrind", "Run unit tests under valgrind");
        step.dependOn(&run.step);
    }

    /////////////
    // release

    const release = b.step("release", "Prepare release builds for all supported platforms");
    const release_targets = [_]std.Target.Query{
        .{ .os_tag = .linux, .cpu_arch = .x86_64 },
        .{ .os_tag = .linux, .cpu_arch = .aarch64 },
        .{ .os_tag = .linux, .cpu_arch = .riscv64 },
        .{ .os_tag = .windows, .cpu_arch = .x86_64 },
        .{ .os_tag = .windows, .cpu_arch = .aarch64 },
        .{ .os_tag = .macos, .cpu_arch = .x86_64 },
        .{ .os_tag = .macos, .cpu_arch = .aarch64 },
    };
    for (release_targets) |target_query| {
        const release_target = b.resolveTargetQuery(target_query);
        const release_exe = b.addExecutable(.{
            .name = "baller",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = release_target,
                .optimize = .ReleaseFast,
                .strip = true,
            }),
        });

        const release_exe_options = b.addOptions();
        release_exe_options.addOption([]const u8, "version", version);
        release_exe.root_module.addOptions("build_options", release_exe_options);

        const install = b.addInstallArtifact(release_exe, .{
            .dest_sub_path = b.fmt("{s}-{s}-{s}-{s}{s}", .{
                release_exe.name,
                version,
                @tagName(release_target.result.os.tag),
                @tagName(release_target.result.cpu.arch),
                release_target.result.exeFileExt(),
            }),
        });
        release.dependOn(&install.step);
    }
}

fn baselineTarget(b: *std.Build, query: *const std.Target.Query) std.Build.ResolvedTarget {
    var q = query.*;
    q.cpu_model = .baseline;
    return b.resolveTargetQuery(q);
}
