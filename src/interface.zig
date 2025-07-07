const std = @import("std");
const mem = std.mem;
const Type = std.builtin.Type;

const assert = std.debug.assert;

///Seems to be the Type of the ptr to the concrete type
pub const SelfType = opaque {};

/// Returns true if PtrType points to single item ptr
fn isSingleItemPtr(PtrType: type) bool {
    const ptr_info = @typeInfo(PtrType);
    if (ptr_info != Type.pointer) {
        return false;
    }
    if (ptr_info.pointer.size != .one) {
        return false;
    }
    return true;
}

/// Returns true if type is Struct enum, union, or opaque
fn isContainer(T: type) bool {
    const type_info = @typeInfo(T);
    return switch (type_info) {
        .@"struct", .@"enum", .@"union", .@"opaque" => true,
        else => false,
    };
}

/// Casts provided pointer to a pointer to opaque SelfType
///
/// [ptr] must be a single item pointer
fn makeSelfPtr(ptr: anytype) *SelfType {
    if (comptime !isSingleItemPtr(@TypeOf(ptr))) {
        @compileError("SelfType pointer initialization expects single item pointer parameter.");
    }

    const T = std.meta.Child(@TypeOf(ptr));

    if (@sizeOf(T) > 0) {
        return @ptrCast(ptr);
    } else {
        return undefined;
    }
}

/// Casts *SelfType to *T
///
/// Size of T must be > 0
fn selfPtrAs(self: *SelfType, comptime T: type) *T {
    if (@sizeOf(T) > 0) {
        return @ptrCast(@alignCast(self));
    } else {
        return undefined;
    }
}

/// Casts *const SelfType to *const T
///
/// Size of T must be > 0
fn constSelfPtrAs(self: *const SelfType, comptime T: type) *const T {
    if (@sizeOf(T) > 0) {
        return @ptrCast(@alignCast(self));
    } else {
        return undefined;
    }
}

/// Returns type of child of pointer or type of self if not a single item pointer
fn PtrChildOrSelf(comptime T: type) type {
    if (comptime isSingleItemPtr(T)) {
        return std.meta.Child(T);
    }

    return T;
}

const GenCallType = enum {
    BothAsync,
    BothBlocking,
    AsyncCallsBlocking,
    BlockingCallsAsync,
};

fn makeCall(
    comptime name: []const u8,
    comptime CurrSelfType: type,
    comptime Return: type,
    comptime ImplType: type,
    comptime call_type: GenCallType,
    self_ptr: CurrSelfType,
    args: anytype,
) Return {
    const is_const = CurrSelfType == *const SelfType;
    const self = if (is_const) constSelfPtrAs(self_ptr, ImplType) else selfPtrAs(self_ptr, ImplType);
    const fptr = @field(ImplType, name);

    const first_arg_ptr = comptime @typeInfo((@typeInfo(@TypeOf(fptr)).@"fn".params[0].type.?)) == .pointer;

    const self_arg = if (first_arg_ptr) .{self} else .{self.*};

    return switch (call_type) {
        .BothBlocking => @call(.always_inline, fptr, self_arg ++ args),
        .AsyncCallsBlocking, .BothAsync => await @call(.async_kw, fptr, self_arg ++ args),
        .BlockingCallsAsync => @compileError("Trying to implement blocking virtual function " ++ name ++ " with async implementation."),
    };
}

///param - name: name of function
///param - FnType: type of function extracted from fn pointer
///param - ImplType: Type of concrete interface implementation
fn getFunctionFromImpl(comptime name: []const u8, comptime FnType: type, comptime ImplType: type) ?FnType {
    const our_cc = @typeInfo(FnType).@"fn".calling_convention;

    // Find the candidate in the implementation type.
    for (std.meta.declarations(ImplType)) |decl| {
        if (!std.mem.eql(u8, name, decl.name)) {
            continue;
        }

        const data = @field(ImplType, decl.name);
        if (@typeInfo(@TypeOf(data)) != .@"fn") {
            return null;
        }

        const fn_type = @typeInfo(@TypeOf(data)).@"fn";

        const args = fn_type.params;

        if (args.len == 0) {
            return @field(ImplType, name);
        }

        const arg0_type = args[0].type.?;
        const is_method = arg0_type == ImplType or arg0_type == *ImplType or arg0_type == *const ImplType;

        const candidate_cc = fn_type.calling_convention;
        switch (candidate_cc) {
            .@"async", .auto => {},
            else => return null,
        }

        const Return = @typeInfo(FnType).@"fn".return_type orelse noreturn;
        const CurrSelfType = @typeInfo(FnType).@"fn".params[0].type.?;

        const call_type: GenCallType = switch (our_cc) {
            .@"async" => if (candidate_cc == .@"async") .BothAsync else .AsyncCallsBlocking,
            .auto => if (candidate_cc == .auto) .BothBlocking else .BlockingCallsAsync,
            else => unreachable,
        };

        if (!is_method) {
            return @field(ImplType, name);
        }

        // TODO: Make this less hacky somehow?
        // We need some new feature to do so unfortunately.

        return switch (args.len) {
            1 => struct {
                fn impl(self_ptr: CurrSelfType) callconv(our_cc) Return {
                    return @call(.always_inline, makeCall, .{ name, CurrSelfType, Return, ImplType, call_type, self_ptr, .{} });
                }
            }.impl,
            2 => struct {
                fn impl(self_ptr: CurrSelfType, arg: args[1].type.?) callconv(our_cc) Return {
                    return @call(.always_inline, makeCall, .{ name, CurrSelfType, Return, ImplType, call_type, self_ptr, .{arg} });
                }
            }.impl,
            3 => struct {
                fn impl(self_ptr: CurrSelfType, arg1: args[1].type.?, arg2: args[2].type.?) callconv(our_cc) Return {
                    return @call(.always_inline, makeCall, .{ name, CurrSelfType, Return, ImplType, call_type, self_ptr, .{ arg1, arg2 } });
                }
            }.impl,
            4 => struct {
                fn impl(self_ptr: CurrSelfType, arg1: args[1].type.?, arg2: args[2].type.?, arg3: args[3].type.?) callconv(our_cc) Return {
                    return @call(.always_inline, makeCall, .{ name, CurrSelfType, Return, ImplType, call_type, self_ptr, .{ arg1, arg2, arg3 } });
                }
            }.impl,
            5 => struct {
                fn impl(self_ptr: CurrSelfType, arg1: args[1].type.?, arg2: args[2].type.?, arg3: args[3].type.?, arg4: args[4].type.?) callconv(our_cc) Return {
                    return @call(.always_inline, makeCall, .{ name, CurrSelfType, Return, ImplType, call_type, self_ptr, .{ arg1, arg2, arg3, arg4 } });
                }
            }.impl,
            6 => struct {
                fn impl(self_ptr: CurrSelfType, arg1: args[1].type.?, arg2: args[2].type.?, arg3: args[3].type.?, arg4: args[4].type.?, arg5: args[5].type.?) callconv(our_cc) Return {
                    return @call(.always_inline, makeCall, .{ name, CurrSelfType, Return, ImplType, call_type, self_ptr, .{ arg1, arg2, arg3, arg4, arg5 } });
                }
            }.impl,
            else => @compileError("Unsupported number of arguments, please provide a manually written vtable."),
        };
    }

    return null;
}

fn makeVTable(comptime VTableType: type, comptime ImplType: type) VTableType {
    if (comptime !isContainer(ImplType)) {
        @compileError("Type '" ++ @typeName(ImplType) ++ "' must be a container to implement interface.");
    }
    var vtable: VTableType = undefined;

    for (std.meta.fields(VTableType)) |field| {
        var fn_type = field.type;
        const is_optional = @typeInfo(fn_type) == Type.optional;
        if (is_optional) {
            fn_type = std.meta.Child(fn_type);
        }

        // Erase the function pointer type
        fn_type = std.meta.Child(fn_type);

        const candidate = comptime getFunctionFromImpl(field.name, fn_type, ImplType);
        if (candidate == null and !is_optional) {
            @compileError("Type '" ++ @typeName(ImplType) ++ "' does not implement non optional function '" ++ field.name ++ "'.");
        } else if (!is_optional) {
            @field(vtable, field.name) = candidate.?;
        } else {
            if (candidate) |c| {
                @field(vtable, field.name) = c;
            } else {
                @field(vtable, field.name) = null;
            }
        }
    }

    return vtable;
}

/// Asserts that [VTableType] is struct and that it defines no methods.
///
/// Asserts that all fields of [VTableType] are non-generic functions with
/// either an .auto or .async calling convention
fn checkVtableType(comptime VTableType: type) void {
    //Asserts VTableType is struct
    if (comptime @typeInfo(VTableType) != Type.@"struct") {
        @compileError("VTable type " ++ @typeName(VTableType) ++ " must be a struct.");
    }

    //Asserts no concrete methods
    for (@typeInfo(VTableType).@"struct".decls) |decl| {
        @compileError("VTable type defines method '" ++ decl.name ++ "'.");
    }

    for (std.meta.fields(VTableType)) |field| {
        var field_type = field.type;

        if (Type.optional == @typeInfo(field_type)) {
            field_type = std.meta.Child(field_type);
        }

        if (Type.pointer == @typeInfo(field_type)) {
            field_type = std.meta.Child(field_type);
        }

        //Asserts that vtable field is function
        if (Type.@"fn" != @typeInfo(field_type)) {
            @compileError("VTable type defines non function field '" ++ field.name ++ "'.");
        }

        const fn_field: Type.Fn = @typeInfo(field_type).@"fn";

        if (fn_field.is_generic) {
            @compileError("Virtual function '" ++ field.name ++ "' cannot be generic.");
        }

        switch (fn_field.calling_convention) {
            .auto, .@"async" => {},
            else => @compileError("Virtual function's  '" ++ field.name ++ "' calling convention is not auto or async."),
        }
    }
}

/// Checks for the [name] of the method on the vtable then sets is_async and is_method based on functions properties
fn vtableHasMethod(comptime VTableType: type, comptime name: []const u8, is_optional: *bool, is_async: *bool, is_method: *bool) bool {
    for (std.meta.fields(VTableType)) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            is_optional.* = Type.optional == @typeInfo(field.type);
            const fn_typeinfo = @typeInfo(std.meta.Child(if (is_optional.*) std.meta.Child(field.type) else field.type)).@"fn";
            is_async.* = fn_typeinfo.calling_convention == .@"async";
            //Checks if first method is pointer to SelfType eg: fn(self: *SelfType) void
            is_method.* = fn_typeinfo.params.len > 0 and blk: {
                const first_arg_type = fn_typeinfo.params[0].type.?;
                break :blk first_arg_type == *SelfType or first_arg_type == *const SelfType;
            };
            return true;
        }
    }

    return false;
}

/// Searches for method in VTable and attempts to extract return type
fn VTableReturnType(comptime VTableType: type, comptime name: []const u8) type {
    for (std.meta.fields(VTableType)) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            const is_optional = @typeInfo(field.type) == .optional;
            //            const is_optional = trait.is(.optional)(field.type);

            const fn_ret_type = (if (is_optional)
                @typeInfo(std.meta.Child(std.meta.Child(field.type))).@"fn".return_type
            else
                @typeInfo(std.meta.Child(field.type)).@"fn".return_type) orelse noreturn;

            if (is_optional) {
                return ?fn_ret_type;
            }

            return fn_ret_type;
        }
    }

    @compileError("VTable type '" ++ @typeName(VTableType) ++ "' has no virtual function '" ++ name ++ "'.");
}

/// Defines an interface type and data storage type. Holds a vtable and StorageType
pub fn Define(comptime VTableType: type, storage: StorageType) type {
    comptime checkVtableType(VTableType);

    const stack_size: usize = if (@hasDecl(VTableType, "async_call_stack_size"))
        VTableType.async_call_stack_size
    else
        1 * 1024 * 1024;

    return struct {
        vtable_ptr: *const VTableType,
        storage: storage.GetType(),

        const Self = @This();
        const VTable = VTableType;
        const Storage = storage.GetType();

        /// StorageType's dynamic init function
        pub const init = Storage.MakeInit(Self).init;

        /// Initializes Interface with *const VTable
        pub fn initWithVTable(vtable_ptr: *const VTableType, args: anytype) Self {
            return .{
                .vtable_ptr = vtable_ptr,
                .storage = init(args),
            };
        }

        /// Calls the given function with args and appends concrete pointer to interface if function is method
        pub fn call(self: @This(), comptime name: []const u8, args: anytype) VTableReturnType(VTableType, name) {
            comptime var is_optional = true;
            comptime var is_async = true;
            comptime var is_method = true;
            comptime assert(vtableHasMethod(VTableType, name, &is_optional, &is_async, &is_method));

            // Potentially null pointer to name field. Should never be null because of assert above
            const fn_ptr = if (is_optional) blk: {
                const fn_val = @field(self.vtable_ptr, name);
                if (fn_val) |v| break :blk v;
                return null;
            } else @field(self.vtable_ptr, name);

            if (is_method) {
                const self_ptr = self.storage.getSelfPtr();
                const new_args = .{self_ptr};

                if (is_async) {
                    var stack_frame: [stack_size]u8 align(std.Target.stack_align) = undefined;
                    return await @asyncCall(&stack_frame, {}, fn_ptr, new_args ++ args);
                }

                return @call(.auto, fn_ptr, new_args ++ args);
            }

            if (is_async) {
                var stack_frame: [stack_size]u8 align(std.Target.stack_align) = undefined;
                return await @asyncCall(&stack_frame, {}, fn_ptr, args);
            }

            return @call(.auto, fn_ptr, args);
        }

        pub fn deinit(self: Self) void {
            self.storage.deinit();
        }
    };
}

/// Defines the way the interface will handle memory references
pub const StorageType = enum {

    /// Interface can only be used at compile time but can contain concrete type information
    compiled,

    /// Interface contains reference to externally defined memory
    non_owned,

    /// Allocates struct to heap via copy. Interface owns memory and must call deinit() to prevent
    /// leaks.
    owned,

    fn GetType(comptime self: StorageType) type {
        return switch (self) {
            .compiled => StorageContainer.Compiled,
            .non_owned => StorageContainer.NonOwned,
            .owned => StorageContainer.Owned,
        };
    }
};

// Struct used to contain ptr storage types.
//
// All types are expected to implement the following methods:
//
//     fn MakeInit(comptime TInterface: type) type
//     pub fn getSelfPtr(comptime self: *Comptime) *SelfType
//     pub fn deinit(comptime self: Comptime) void
const StorageContainer = struct {
    pub const Compiled = struct {
        erased_ptr: *SelfType,
        ImplType: type,

        fn MakeInit(comptime TInterface: type) type {
            return struct {
                fn init(comptime obj: anytype) TInterface {
                    const ImplType = PtrChildOrSelf(@TypeOf(obj));

                    comptime var obj_holder = obj;

                    return TInterface{
                        .vtable_ptr = &comptime makeVTable(TInterface.VTable, ImplType),
                        .storage = Compiled{
                            .erased_ptr = makeSelfPtr(&obj_holder),
                            .ImplType = @TypeOf(obj),
                        },
                    };
                }
            };
        }

        pub fn getSelfPtr(comptime self: *const Compiled) *SelfType {
            return self.erased_ptr;
        }

        pub fn deinit(comptime self: Compiled) void {
            _ = self;
        }
    };

    pub const NonOwned = struct {
        erased_ptr: *SelfType,

        fn MakeInit(comptime TInterface: type) type {
            return struct {
                fn init(ptr: anytype) TInterface {
                    return TInterface{
                        .vtable_ptr = &comptime makeVTable(TInterface.VTable, PtrChildOrSelf(@TypeOf(ptr))),
                        .storage = NonOwned{
                            .erased_ptr = makeSelfPtr(ptr),
                        },
                    };
                }
            };
        }

        pub fn getSelfPtr(self: NonOwned) *SelfType {
            return self.erased_ptr;
        }

        pub fn deinit(self: NonOwned) void {
            _ = self;
        }
    };

    /// Allocates struct to heap via copy. Interface owns memory
    pub const Owned = struct {
        allocator: mem.Allocator,
        mem: []u8,
        alignment: mem.Alignment,

        fn MakeInit(comptime TInterface: type) type {
            return struct {
                fn init(obj: anytype, allocator: std.mem.Allocator) TInterface {
                    const AllocType = @TypeOf(obj);

                    const t_align = @alignOf(AllocType);
                    //                    const t_align_log2 = std.math.log2(t_align);
                    const t_alignment = mem.Alignment.fromByteUnits(t_align);

                    const base = allocator.rawAlloc(@sizeOf(AllocType), t_alignment, @returnAddress()) orelse unreachable;

                    const slice = base[0..@sizeOf(AllocType)];
                    errdefer allocator.rawFree(slice, t_alignment, @returnAddress());

                    const ptr: *AllocType = @ptrCast(@alignCast(base));
                    ptr.* = obj;

                    return TInterface{
                        .vtable_ptr = &comptime makeVTable(TInterface.VTable, PtrChildOrSelf(AllocType)),
                        .storage = Owned{
                            .allocator = allocator,
                            .mem = slice,
                            .alignment = t_alignment,
                        },
                    };
                }
            };
        }

        pub fn getSelfPtr(self: Owned) *SelfType {
            return makeSelfPtr(&self.mem[0]);
        }

        pub fn deinit(self: Owned) void {
            self.allocator.rawFree(self.mem, self.alignment, @returnAddress());
        }
    };

    pub fn Inlined(comptime size: usize) type {
        return struct {
            const Self = @This();

            mem: [size]u8,

            fn MakeInit(comptime TInterface: type) type {
                return struct {
                    fn init(value: anytype) TInterface {
                        const ImplSize = @sizeOf(@TypeOf(value));

                        if (ImplSize > size) {
                            @compileError("Type does not fit in inline storage.");
                        }

                        var self = Self{
                            .mem = undefined,
                        };
                        if (ImplSize > 0) {
                            std.mem.copy(u8, self.mem[0..], @as([*]const u8, @ptrCast(&value))[0..ImplSize]);
                        }

                        return TInterface{
                            .vtable_ptr = &comptime makeVTable(TInterface.VTable, PtrChildOrSelf(@TypeOf(value))),
                            .storage = self,
                        };
                    }
                };
            }

            pub fn getSelfPtr(self: *Self) *SelfType {
                return makeSelfPtr(&self.mem[0]);
            }

            pub fn deinit(self: Self) void {
                _ = self;
            }
        };
    }

    pub fn InlineOrOwning(comptime size: usize) type {
        return struct {
            const Self = @This();

            data: union(enum) {
                Inlined: Inlined(size),
                Owned: Owned,
            },

            pub fn init(args: anytype) Self {
                if (args.len != 2) {
                    @compileError("InlineOrOwning storage expected a 2-tuple in initialization.");
                }

                const ImplSize = @sizeOf(@TypeOf(args[0]));

                if (ImplSize > size) {
                    return Self{
                        .data = .{
                            .Owned = Owned.init(args),
                        },
                    };
                } else {
                    return Self{
                        .data = .{
                            .Inlined = Inlined(size).init(.{args[0]}),
                        },
                    };
                }
            }

            pub fn getSelfPtr(self: *Self) *SelfType {
                return switch (self.data) {
                    .Inlined => |*i| i.getSelfPtr(),
                    .Owned => |*o| o.getSelfPtr(),
                };
            }

            pub fn deinit(self: Self) void {
                switch (self.data) {
                    .Inlined => |i| i.deinit(),
                    .Owned => |o| o.deinit(),
                }
            }
        };
    }
};
