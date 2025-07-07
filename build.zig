const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("interface", .{
        .root_source_file = b.path("src/interface.zig"),
    });

    const lib_tests = b.addTest(.{ .root_source_file = b.path("src/tests.zig" )});
    const run_lib_tests = b.addRunArtifact(lib_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_tests.step);
}
