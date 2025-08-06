const std = @import("std");

property: u32,

/// An implementation of the print function, notice the signature's members
/// match the one found in the definition
pub fn print(self: @This(), message: []const u8) void {
    std.debug.print("prop: {}\nmessage: {s}\n", .{ self.property, message });
}
