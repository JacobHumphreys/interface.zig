# Zig Interfaces
Easy solution for all your zig dynamic dispatch needs!

This project is a fork of interface.zig which aims to port the project to zig 0.14.1
as a fetchable library. It changes certain behavior and naming conventions see Example 
for details.

## Features
- Fully decoupled interfaces and implementations
- Control over the storage/ownership of interface objects
- Comptime support (including comptime-only interfaces)
- Optional function support
- Support for manually written vtables

## Install
Run the following command to install into a zig project

```bash 
    > zig fetch --save git+https://github.com/JacobHumphreys/interface.zig.git
```

Then Add the following to build.zig

```zig
    const interface_lib_dep = b.dependency("interface", .{});
    const interface_lib = interface_lib_dep.module("interface");

    const exe = b.addExecutable(.{
        //Find this setting
    });

    //paste this after
    exe.root_module.addImport("interface", interface_lib);
```

## Example

```zig
const std = @import("std");
const interface = @import("interface");
const SelfType = interface.SelfType;

const ExampleInterface = struct{
    const Definition = interface.Define(struct {
        print: *const fn (*SelfType) void,
    }, interface.Storage.NonOwning);

    impl: Definition,

    //Inlining is optional
    pub inline fn Impl(impl_ptr: anytype) ExampleInterface {
        return ExampleInterface{
            .impl = Definition.init(impl_ptr),
        };
    }

    //Inlining is optional
    pub inline fn print(self: ExampleInterface) void {
        self.impl.call("print", .{});
    }
};

const ExampleImpl = struct{
    property: u32,

    pub fn print(self: *ExampleImpl) void {
        std.debug.print("prop: {}\n", .{self.property});
    }
};

pub fn main() !void {
    var exampleStruct = ExampleImpl{
        .property = 1,
    };

    const impl_from_struct = ExampleInterface.Impl(&ex1));

    callPrintOnExample(impl_from_struct);
}

fn callPrintOnExample(printer: ExampleInterface) void {
    printer.print();
}
```
See example-project directory for full implementation details.

See testing.zig for more examples.
