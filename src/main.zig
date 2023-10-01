//! this is a zig wrapper over wasm3 that aims to be as thin as possible. no
//! extra allocations or big computations, just make the c interface look as
//! ziggy as possible with the constraints

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const in_debug = builtin.mode == .Debug;

/// keep public for hackability
pub const c = @import("m3.zig");

fn rawCStrToSlice(raw_cstr: [*c]const u8) [:0]const u8 {
    const cstr = @as([*:0]const u8, @ptrCast(raw_cstr));
    const len = std.mem.indexOfSentinel(u8, 0, cstr);
    return cstr[0..len :0];
}

// errors ======================================================================

const ErrorTableEntry = struct {
    const Self = @This();

    err: anyerror,
    name: []const u8,

    fn init(err: anyerror, name: []const u8) Self {
        return .{
            .err = err,
            .name = name,
        };
    }
};

const error_table: []const ErrorTableEntry = table: {
    const e = ErrorTableEntry.init;
    break :table &.{
        e(error.MallocFailed, "m3Err_mallocFailed"),
        e(error.IncompatibleWasmVersion, "m3Err_incompatibleWasmVersion"),
        e(error.WasmMalformed, "m3Err_wasmMalformed"),
        e(error.MisorderedWasmSection, "m3Err_misorderedWasmSection"),
        e(error.WasmUnderrun, "m3Err_wasmUnderrun"),
        e(error.WasmOverrun, "m3Err_wasmOverrun"),
        e(error.WasmMissingInitExpr, "m3Err_wasmMissingInitExpr"),
        e(error.LebOverflow, "m3Err_lebOverflow"),
        e(error.MissingUTF8, "m3Err_missingUTF8"),
        e(error.WasmSectionUnderrun, "m3Err_wasmSectionUnderrun"),
        e(error.WasmSectionOverrun, "m3Err_wasmSectionOverrun"),
        e(error.InvalidTypeId, "m3Err_invalidTypeId"),
        e(error.TooManyMemorySections, "m3Err_tooManyMemorySections"),
        e(error.TooManyArgsRets, "m3Err_tooManyArgsRets"),
        e(error.ModuleNotLinked, "m3Err_moduleNotLinked"),
        e(error.ModuleAlreadyLinked, "m3Err_moduleAlreadyLinked"),
        e(error.FunctionLookupFailed, "m3Err_functionLookupFailed"),
        e(error.FunctionImportMissing, "m3Err_functionImportMissing"),
        e(error.MalformedFunctionSignature, "m3Err_malformedFunctionSignature"),
        e(error.NoCompiler, "m3Err_noCompiler"),
        e(error.UnknownOpcode, "m3Err_unknownOpcode"),
        e(error.RestrictedOpcode, "m3Err_restrictedOpcode"),
        e(error.FunctionStackOverflow, "m3Err_functionStackOverflow"),
        e(error.FunctionStackUnderrun, "m3Err_functionStackUnderrun"),
        e(error.MallocFailedCodePage, "m3Err_mallocFailedCodePage"),
        e(error.SettingImmutableGlobal, "m3Err_settingImmutableGlobal"),
        e(error.TypeMismatch, "m3Err_typeMismatch"),
        e(error.TypeCountMismatch, "m3Err_typeCountMismatch"),
        e(error.MissingCompiledCode, "m3Err_missingCompiledCode"),
        e(error.WasmMemoryOverflow, "m3Err_wasmMemoryOverflow"),
        e(error.GlobalMemoryNotAllocated, "m3Err_globalMemoryNotAllocated"),
        e(error.GlobaIndexOutOfBounds, "m3Err_globaIndexOutOfBounds"),
        e(error.ArgumentCountMismatch, "m3Err_argumentCountMismatch"),
        e(error.ArgumentTypeMismatch, "m3Err_argumentTypeMismatch"),
        e(error.GlobalLookupFailed, "m3Err_globalLookupFailed"),
        e(error.GlobalTypeMismatch, "m3Err_globalTypeMismatch"),
        e(error.GlobalNotMutable, "m3Err_globalNotMutable"),
        e(error.TrapOutOfBoundsMemoryAccess, "m3Err_trapOutOfBoundsMemoryAccess"),
        e(error.TrapDivisionByZero, "m3Err_trapDivisionByZero"),
        e(error.TrapIntegerOverflow, "m3Err_trapIntegerOverflow"),
        e(error.TrapIntegerConversion, "m3Err_trapIntegerConversion"),
        e(error.TrapIndirectCallTypeMismatch, "m3Err_trapIndirectCallTypeMismatch"),
        e(error.TrapTableIndexOutOfRange, "m3Err_trapTableIndexOutOfRange"),
        e(error.TrapTableElementIsNull, "m3Err_trapTableElementIsNull"),
        e(error.TrapExit, "m3Err_trapExit"),
        e(error.TrapAbort, "m3Err_trapAbort"),
        e(error.TrapUnreachable, "m3Err_trapUnreachable"),
        e(error.TrapStackOverflow, "m3Err_trapStackOverflow"),
    };
};

pub const Error = e: {
    const Type = std.builtin.Type;

    var errors: [error_table.len]Type.Error = undefined;
    for (error_table, &errors) |entry, *slot| {
        slot.* = .{ .name = @errorName(entry.err) };
    }

    break :e @Type(.{ .ErrorSet = &errors });
};

/// useful wrapper for wasm3 functions
fn check(res: c.M3Result) Error!void {
    if (res) |msg_ptr| {
        inline for (error_table) |entry| {
            if (msg_ptr == @field(c, entry.name)) {
                return @as(Error, @errSetCast(entry.err));
            }
        }
    }
}

// main interface ==============================================================

pub const ValueType = enum {
    const Self = @This();

    i32,
    i64,
    f32,
    f64,

    pub fn zigType(comptime self: Self) type {
        inline for (@typeInfo(Value).Union.fields) |field| {
            if (std.mem.eql(u8, @tagName(self), field.name)) {
                return field.type;
            }
        }

        unreachable;
    }

    fn fromC(raw: c.M3Value.Type) ?Self {
        return switch (raw) {
            inline .i32,
            .i64,
            .f32,
            .f64,
            => |tag| std.enums.nameCast(Self, tag),

            else => null,
        };
    }

    fn intoC(self: Self) c.M3Value.Type {
        return switch (self) {
            inline else => |tag| std.enums.nameCast(c.M3Value.Type, tag),
        };
    }
};

pub const Value = union(ValueType) {
    const Self = @This();

    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,

    pub fn eql(a: Self, b: Self) bool {
        return std.meta.eql(a, b);
    }

    fn fromC(cval: c.M3Value) Self {
        return switch (ValueType.fromC(cval.type).?) {
            inline else => |data, tag| @unionInit(Self, @tagName(tag), data),
        };
    }

    fn intoC(self: Self) c.M3TaggedValue {
        return switch (self) {
            inline else => |data, tag| .{
                .type = tag.intoC(),
                .value = @unionInit(c.M3Value.Data, @tagName(tag), data),
            },
        };
    }

    /// get a pointer to the data (this is useful for some c interfacing
    /// operations)
    fn ptrMut(self: *Self) *anyopaque {
        return switch (self.*) {
            inline else => |_, tag| &@field(self, @tagName(tag)),
        };
    }

    /// get a pointer to the data (this is useful for some c interfacing
    /// operations)
    fn ptr(self: *const Self) *const anyopaque {
        return switch (self.*) {
            inline else => |_, tag| &@field(self, @tagName(tag)),
        };
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self) {
            inline else => |data, tag| {
                try writer.print("({s}){d}", .{ @tagName(tag), data });
            },
        }
    }
};

/// global execution environment, which can host multiple runtimes
pub const Environment = struct {
    const Self = @This();

    ptr: *c.M3Environment,

    pub fn init() Self {
        return .{ .ptr = c.m3_NewEnvironment() };
    }

    pub fn deinit(self: Self) void {
        c.m3_FreeEnvironment(self.ptr);
    }
};

/// the context for executing wasm modules
pub const Runtime = struct {
    const Self = @This();

    ptr: *c.M3Runtime,

    pub const Config = struct {
        stack_bytes: u32 = 64 * 1024,
        user_data: ?*anyopaque = null,
    };

    pub fn init(env: Environment, cfg: Config) Self {
        return .{
            .ptr = c.m3_NewRuntime(env.ptr, cfg.stack_bytes, cfg.user_data),
        };
    }

    pub fn deinit(self: Self) void {
        c.m3_FreeRuntime(self.ptr);
    }

    pub fn printInfo(self: Self) void {
        if (in_debug) {
            c.m3_PrintRuntimeInfo(self.ptr);
        }
    }

    /// get the memory region associated with the runtime
    pub fn getMemory(self: Self) []u8 {
        var nbytes: u32 = undefined;
        const ptr = c.m3_GetMemory(self.ptr, &nbytes, 0).?;
        return ptr[0..nbytes];
    }

    /// get the userdata passed in from config
    pub fn getUserData(self: Self) ?*anyopaque {
        return c.m3_GetUserData(self.ptr);
    }

    /// load a module to the runtime (transfers ownership)
    pub fn load(self: Self, module: *Module) Error!void {
        std.debug.assert(!module.loaded);
        try check(c.m3_LoadModule(self.ptr, module.ptr));
        module.loaded = true;
    }

    pub fn findFunction(self: Self, name: [:0]const u8) Error!Function {
        var fun_ptr: *c.M3Function = undefined;
        try check(c.m3_FindFunction(&fun_ptr, self.ptr, name.ptr));
        return .{ .ptr = fun_ptr };
    }
};

pub const Module = struct {
    const Self = @This();

    ptr: *c.M3Module,
    loaded: bool = false,

    pub fn parse(env: Environment, bytecode: []const u8) Error!Self {
        var ptr: *c.M3Module = undefined;
        try check(c.m3_ParseModule(
            env.ptr,
            &ptr,
            @ptrCast(bytecode.ptr),
            @intCast(bytecode.len),
        ));

        return .{ .ptr = ptr };
    }

    pub fn deinit(self: *Self) void {
        if (!self.loaded) c.m3_FreeModule(self.ptr);
    }

    pub const linkRawFunction = linkRawZigFunction;

    pub fn getRuntime(self: Self) ?Runtime {
        if (c.m3_GetModuleRuntime(self.ptr)) |runtime_ptr| {
            return .{ .ptr = runtime_ptr };
        }

        return null;
    }

    /// (*optional*) compile all functions in the module
    pub fn compile(self: Self) Error!void {
        try check(c.m3_CompileModule(self.ptr));
    }

    /// (*optional*)
    pub fn runStart(self: Self) Error!void {
        try check(c.m3_RunStart(self.ptr));
    }

    // TODO figure out how to add raw functions to my wrapper?

    pub fn setName(self: Self, name: [:0]const u8) void {
        c.m3_SetModuleName(self.ptr, name.ptr);
    }

    pub fn getName(self: Self) [:0]const u8 {
        const raw_cstr = c.m3_GetModuleName(self.ptr);
        return rawCStrToSlice(raw_cstr);
    }
};

pub const Function = struct {
    const Self = @This();

    ptr: *c.M3Function,

    pub fn getName(self: Self) [:0]const u8 {
        const raw_cstr = c.m3_GetFunctionName(self.ptr);
        return rawCStrToSlice(raw_cstr);
    }

    pub fn getArgCount(self: Self) usize {
        return c.m3_GetArgCount(self.ptr);
    }

    pub fn getRetCount(self: Self) usize {
        return c.m3_GetRetCount(self.ptr);
    }

    /// assumes that your index is inbounds
    pub fn getArgType(self: Self, index: usize) ValueType {
        const cval = c.m3_GetArgType(self.ptr, @intCast(index));
        return ValueType.fromC(cval).?;
    }

    /// assumes that your index is inbounds
    pub fn getRetType(self: Self, index: usize) ValueType {
        const cval = c.m3_GetRetType(self.ptr, @intCast(index));
        return ValueType.fromC(cval).?;
    }

    pub const CallAllocError = Allocator.Error || Error;

    /// make a function call using allocator for args & return values
    pub fn callAlloc(
        self: Self,
        ally: Allocator,
        args: []const Value,
    ) CallAllocError![]const Value {
        std.debug.assert(args.len == self.getArgCount());

        for (args, 0..) |arg, i| {
            std.debug.assert(self.getArgType(i) == @as(ValueType, arg));
        }

        // call
        const arg_ptrs = try ally.alloc(*const anyopaque, args.len);
        for (arg_ptrs, args) |*slot, *arg| {
            slot.* = arg.ptr();
        }

        try check(c.m3_Call(
            self.ptr,
            @intCast(arg_ptrs.len),
            @ptrCast(arg_ptrs.ptr),
        ));

        ally.free(arg_ptrs);

        // get results
        const retc = self.getRetCount();
        const results = try ally.alloc(Value, retc);

        for (results, 0..) |*value, i| {
            switch (self.getRetType(i)) {
                inline else => |tag| {
                    value.* = @unionInit(Value, @tagName(tag), undefined);
                },
            }
        }

        const ret_ptrs = try ally.alloc(*anyopaque, retc);
        defer ally.free(ret_ptrs);

        for (ret_ptrs, results) |*slot, *value| {
            slot.* = value.ptrMut();
        }

        try check(c.m3_GetResults(
            self.ptr,
            @intCast(ret_ptrs.len),
            @ptrCast(ret_ptrs.ptr),
        ));

        return results;
    }

    pub const CallBufError = error{BufferOverflow} || Error;

    /// make a function call using a buffer for storing args & return values
    pub fn callBuf(
        self: Self,
        buf: []u8,
        args: []const Value,
    ) CallBufError![]const Value {
        var fba = std.heap.FixedBufferAllocator.init(buf);
        return self.callAlloc(fba.allocator(), args) catch |e| {
            return switch (e) {
                Allocator.Error.OutOfMemory => CallBufError.BufferOverflow,
                else => @as(CallBufError, @errSetCast(e)),
            };
        };
    }
};

pub const RawFunction = fn (
    Runtime,
    params: []const Value,
    out_results: []Value,
) void;

/// calls
fn rawZigFunctionWrapper(
    runtime: *c.M3Runtime,
    import_ctx: *c.M3ImportContext,
    stack: [*]u64,
    mem: ?*anyopaque,
) callconv(.C) ?*const anyopaque {
    _ = mem;

    const function = Function{ .ptr = import_ctx.function };
    const raw_function: *const RawFunction = @ptrCast(import_ctx.userdata.?);

    // set up arg buf
    const argc = function.getArgCount();
    var arg_buf: [256]Value = undefined;
    const args = arg_buf[0..argc];

    for (args, 0..) |*slot, i| {
        switch (function.getArgType(i)) {
            inline else => |arg_type| {
                const FieldType = std.meta.FieldType(Value, arg_type);

                slot.* = @unionInit(
                    Value,
                    @tagName(arg_type),
                    @as(*const FieldType, @ptrCast(&stack[i])).*,
                );
            },
        }
    }

    // set up ret buf
    const retc = function.getRetCount();
    var ret_buf: [256]Value = undefined;
    const rets = ret_buf[0..retc];

    raw_function(.{ .ptr = runtime }, args, rets);

    // put return values on stack
    for (rets, 0..) |val, i| {
        switch (val) {
            inline else => |data| {
                @as(*@TypeOf(data), @ptrCast(&stack[i])).* = data;
            },
        }
    }

    return null;
}

fn wasm3SigChar(t: ValueType) u8 {
    return switch (t) {
        .i32 => 'i',
        .i64 => 'I',
        .f32 => 'f',
        .f64 => 'F',
    };
}

fn linkRawZigFunction(
    module: Module,
    from_module: [:0]const u8,
    name: [:0]const u8,
    params: []const ValueType,
    results: []const ValueType,
    func_ptr: *const RawFunction,
) Error!void {
    var sig = std.BoundedArray(u8, 512){};

    for (results) |t| sig.appendAssumeCapacity(wasm3SigChar(t));
    sig.appendAssumeCapacity('(');
    for (params) |t| sig.appendAssumeCapacity(wasm3SigChar(t));
    sig.appendAssumeCapacity(')');
    sig.appendAssumeCapacity(0);

    try check(c.m3_LinkRawFunctionEx(
        module.ptr,
        from_module.ptr,
        name.ptr,
        @ptrCast(&sig.buffer),
        rawZigFunctionWrapper,
        @ptrCast(func_ptr),
    ));
}

// debugging functions =========================================================

fn wrapDebugFn(comptime f: fn () callconv(.C) void) fn () void {
    return struct {
        fn wrapped() void {
            if (in_debug) f();
        }
    }.wrapped;
}

pub const printM3Info = wrapDebugFn(c.m3_PrintM3Info);
pub const printProfilerInfo = wrapDebugFn(c.m3_PrintProfilerInfo);
