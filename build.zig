const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("interface", .{
        .root_source_file = .{ 
            .path = "src/interface.zig" },
    });
}
