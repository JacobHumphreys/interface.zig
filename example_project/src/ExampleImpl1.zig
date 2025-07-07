const std = @import("std");
const ExampleImpl = @This();

property: u32,

pub fn print(self: *ExampleImpl, message: []const u8) void {
    std.debug.print("prop: {}\nmessage: {s}\n", .{self.property, message});
}
