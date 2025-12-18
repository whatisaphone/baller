const std = @import("std");

const version = "0.6.3";

pub fn build(b: *std.Build) void {
    const valgrind = b.option(bool, "valgrind", "Add valgrind support") orelse false;
    const test_filters = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match any filter",
    ) orelse &.{};

    const optimize = b.standardOptimizeOption(.{});

    var target_args: std.Build.StandardTargetOptionsArgs = .{};
    if (valgrind)
        target_args.default_target.cpu_model = .baseline;
    const target = b.standardTargetOptions(target_args);

    /////////
    // exe

    const exe = b.addExecutable(.{
        .name = "baller",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (valgrind)
        exe.linkLibC();

    const exe_options = b.addOptions();
    exe_options.addOption([]const u8, "version", "dev");
    exe.root_module.addOptions("build_options", exe_options);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    ///////////
    // tests

    const exe_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .filters = test_filters,
    });
    if (valgrind)
        exe_unit_tests.linkLibC();

    exe_unit_tests.root_module.addOptions("build_options", exe_options);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    run_exe_unit_tests.has_side_effects = true;

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const run_tests_valgrind = b.addSystemCommand(&.{
        "valgrind",
        "--leak-check=full",
        "--error-exitcode=1",
        "--exit-on-first-error=yes",
        "--track-origins=yes",
        "--",
    });
    run_tests_valgrind.addFileArg(exe_unit_tests.getEmittedBin());
    const test_valgrind_step = b.step("test:valgrind", "Run unit tests under valgrind");
    if (valgrind)
        test_valgrind_step.dependOn(&run_tests_valgrind.step)
    else
        test_valgrind_step.dependOn(&b.addFail("missing valgrind flag").step);

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
