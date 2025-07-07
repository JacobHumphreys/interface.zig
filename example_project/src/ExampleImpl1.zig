const std = @import("std");
const ExampleImpl = @This();

property: u32,

pub fn print(self: *ExampleImpl) void {
    std.debug.print("prop: {}\n", .{self.property});
}
