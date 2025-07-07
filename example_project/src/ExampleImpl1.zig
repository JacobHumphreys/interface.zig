const std = @import("std");

property: u32,

pub fn print(self: @This(), message: []const u8) void {
    std.debug.print("prop: {}\nmessage: {s}\n", .{self.property, message});
}
