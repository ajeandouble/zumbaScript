const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "zumba",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .error_tracing = true,
        }),
    });
    // const options = b.addOptions();
    // options.addOption(bool, "verbose", false);
    // exe.root_module.addImport("build_options", options.createModule());
    b.installArtifact(exe);
}
