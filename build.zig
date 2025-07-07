const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("my_lib", .{
        .source_file = .{ .path = "src/interface.zig" },
    });
}
