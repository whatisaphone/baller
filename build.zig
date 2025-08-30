const std = @import("std");

const version = "0.6.1";

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const valgrind = b.option(bool, "valgrind", "Add valgrind support") orelse false;

    var target_args: std.Build.StandardTargetOptionsArgs = .{};
    if (valgrind)
        target_args.default_target.cpu_model = .baseline;
    const target = b.standardTargetOptions(target_args);

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

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

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (valgrind)
        exe_unit_tests.linkLibC();

    exe_unit_tests.root_module.addOptions("build_options", exe_options);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    run_exe_unit_tests.has_side_effects = true;

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const run_tests_valgrind = b.addSystemCommand(&.{
        "valgrind",
        "--leak-check=full",
        "--error-exitcode=1",
        "--exit-on-first-error=yes",
        "--",
    });
    run_tests_valgrind.addFileArg(exe_unit_tests.getEmittedBin());
    const test_valgrind_step = b.step("test:valgrind", "Run unit tests under valgrind");
    if (valgrind)
        test_valgrind_step.dependOn(&run_tests_valgrind.step)
    else
        test_valgrind_step.dependOn(&b.addFail("missing valgrind flag").step);

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
