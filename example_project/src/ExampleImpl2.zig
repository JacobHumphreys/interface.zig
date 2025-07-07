const std = @import("std");
const ExampleImpl = @This();

property: []const u8,

pub fn print(self: *ExampleImpl) void {
    std.debug.print("prop: {s}\n", .{self.property});
}
