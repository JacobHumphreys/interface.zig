const std = @import("std");

property: []const u8,

pub fn print(self: *@This(), message: []const u8) void {
    std.debug.print("prop: {s}\nmessage: {s}\n", .{self.property, message});
}
