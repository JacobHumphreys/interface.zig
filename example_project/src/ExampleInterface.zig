const interface = @import("interface");
const SelfType = interface.SelfType;

const ExampleInterface = @This();

const Definition = interface.Define(struct {
    print: *const fn (*SelfType, []const u8) void,
}, interface.Storage.NonOwning);

impl: Definition,

pub inline fn Impl(impl_ptr: anytype) ExampleInterface {
    return ExampleInterface{
        .impl = Definition.init(impl_ptr),
    };
}

pub inline fn print(self: ExampleInterface, message: []const u8) void {
    self.impl.call("print", .{message});
}
