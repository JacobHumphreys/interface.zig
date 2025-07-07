const std = @import("std");
const ExampleImpl = @This();

property: []const u8,

pub fn print(self: *ExampleImpl, message: []const u8) void {
    std.debug.print("prop: {s}\nmessage: {s}\n", .{self.property, message});
}
