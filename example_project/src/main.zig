const std = @import("std");

const ArrayList = std.ArrayListUnmanaged;

const ExampleImpl1 = @import("ExampleImpl1.zig");
const ExampleImpl2 = @import("ExampleImpl2.zig");
const ExampleInterface = @import("ExampleInterface.zig");

pub fn main() !void {
    var debug_allocator = std.heap.DebugAllocator(.{}).init;
    defer _ = debug_allocator.deinit();
    const alloc = debug_allocator.allocator();

    var ex1 = ExampleImpl1{
        .property = 1,
    };
    var ex2 = ExampleImpl2{
        .property = "Interfaces are easy!",
    };

    var interface_list = ArrayList(ExampleInterface).empty;
    defer interface_list.deinit(alloc);

    try interface_list.append(alloc, ExampleInterface.Impl(&ex1));
    try interface_list.append(alloc, ExampleInterface.Impl(&ex2));

    printPrinters(interface_list);
}

fn printPrinters(printers: ArrayList(ExampleInterface)) void {
    for (printers.items) |printer| {
        printer.print();
    }
}
