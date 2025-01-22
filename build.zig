const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{ .name = "zumba", .root_source_file = b.path("src/main.zig"), .target = b.standardTargetOptions(.{}), .optimize = b.standardOptimizeOption(.{}), .error_tracing = true });
    // const options = b.addOptions();
    // options.addOption(bool, "verbose", false);
    // exe.root_module.addImport("build_options", options.createModule());
    b.installArtifact(exe);
}
