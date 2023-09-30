//! this is a zig wrapper over wasm3 that aims to be as thin as possible. no
//! extra allocations or big computations, just make the c interface look as
//! ziggy as possible with the constraints

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const in_debug = builtin.mode == .Debug;

/// keep public for hackability
pub const c = @cImport({
    @cInclude("wasm3.h");
});

fn rawCStrToSlice(raw_cstr: [*c]const u8) [:0]const u8 {
    const cstr = @as([*:0]const u8, @ptrCast(raw_cstr));
    const len = std.mem.indexOfSentinel(u8, 0, cstr);
    return cstr[0..len :0];
}

// errors ======================================================================

const ErrorTableEntry = struct {
    const Self = @This();

    err: anyerror,
    msg_ptr: [*:0]const u8,

    fn init(err: anyerror, msg_ptr: [*:0]const u8) Self {
        return .{
            .err = err,
            .msg_ptr = msg_ptr,
        };
    }
};

const error_table: []const ErrorTableEntry = table: {
    const e = ErrorTableEntry.init;
    break :table &.{
        e(error.MallocFailed, "mallocFailed"),
        e(error.IncompatibleWasmVersion, "incompatibleWasmVersion"),
        e(error.WasmMalformed, "wasmMalformed"),
        e(error.MisorderedWasmSection, "misorderedWasmSection"),
        e(error.WasmUnderrun, "wasmUnderrun"),
        e(error.WasmOverrun, "wasmOverrun"),
        e(error.WasmMissingInitExpr, "wasmMissingInitExpr"),
        e(error.LebOverflow, "lebOverflow"),
        e(error.MissingUTF8, "missingUTF8"),
        e(error.WasmSectionUnderrun, "wasmSectionUnderrun"),
        e(error.WasmSectionOverrun, "wasmSectionOverrun"),
        e(error.InvalidTypeId, "invalidTypeId"),
        e(error.TooManyMemorySections, "tooManyMemorySections"),
        e(error.TooManyArgsRets, "tooManyArgsRets"),
        e(error.ModuleNotLinked, "moduleNotLinked"),
        e(error.ModuleAlreadyLinked, "moduleAlreadyLinked"),
        e(error.FunctionLookupFailed, "functionLookupFailed"),
        e(error.FunctionImportMissing, "functionImportMissing"),
        e(error.MalformedFunctionSignature, "malformedFunctionSignature"),
        e(error.NoCompiler, "noCompiler"),
        e(error.UnknownOpcode, "unknownOpcode"),
        e(error.RestrictedOpcode, "restrictedOpcode"),
        e(error.FunctionStackOverflow, "functionStackOverflow"),
        e(error.FunctionStackUnderrun, "functionStackUnderrun"),
        e(error.MallocFailedCodePage, "mallocFailedCodePage"),
        e(error.SettingImmutableGlobal, "settingImmutableGlobal"),
        e(error.TypeMismatch, "typeMismatch"),
        e(error.TypeCountMismatch, "typeCountMismatch"),
        e(error.MissingCompiledCode, "missingCompiledCode"),
        e(error.WasmMemoryOverflow, "wasmMemoryOverflow"),
        e(error.GlobalMemoryNotAllocated, "globalMemoryNotAllocated"),
        e(error.GlobaIndexOutOfBounds, "globaIndexOutOfBounds"),
        e(error.ArgumentCountMismatch, "argumentCountMismatch"),
        e(error.ArgumentTypeMismatch, "argumentTypeMismatch"),
        e(error.GlobalLookupFailed, "globalLookupFailed"),
        e(error.GlobalTypeMismatch, "globalTypeMismatch"),
        e(error.GlobalNotMutable, "globalNotMutable"),
        e(error.TrapOutOfBoundsMemoryAccess, "trapOutOfBoundsMemoryAccess"),
        e(error.TrapDivisionByZero, "trapDivisionByZero"),
        e(error.TrapIntegerOverflow, "trapIntegerOverflow"),
        e(error.TrapIntegerConversion, "trapIntegerConversion"),
        e(error.TrapIndirectCallTypeMismatch, "trapIndirectCallTypeMismatch"),
        e(error.TrapTableIndexOutOfRange, "trapTableIndexOutOfRange"),
        e(error.TrapTableElementIsNull, "trapTableElementIsNull"),
        e(error.TrapExit, "trapExit"),
        e(error.TrapAbort, "trapAbort"),
        e(error.TrapUnreachable, "trapUnreachable"),
        e(error.TrapStackOverflow, "trapStackOverflow"),
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
            if (msg_ptr == @field(c, "m3Err_" ++ entry.msg_ptr)) {
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

    const TableEntry = struct {
        val: Self,
        cval: c_int,
    };

    const table = [_]TableEntry{
        .{ .val = .i32, .cval = c.c_m3Type_i32 },
        .{ .val = .i64, .cval = c.c_m3Type_i64 },
        .{ .val = .f32, .cval = c.c_m3Type_f32 },
        .{ .val = .f64, .cval = c.c_m3Type_f64 },
    };

    fn fromC(n: c_uint) ?Self {
        inline for (table) |entry| {
            if (n == entry.cval) {
                return entry.val;
            }
        }

        return null;
    }

    fn intoC(self: Self) c_uint {
        inline for (table) |entry| {
            if (entry.val == self) {
                return entry.cval;
            }
        }

        unreachable;
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

    fn fromC(cval: c.M3TaggedValue) Self {
        return switch (ValueType.fromC(cval.type).?) {
            inline else => |data, tag| @unionInit(Self, @tagName(tag), data),
        };
    }

    fn intoC(self: Self) c.M3TaggedValue {
        return switch (self) {
            inline else => |data, tag| .{
                .type = tag.intoC(),
                .value = @unionInit(c.union_M3ValueUnion, @tagName(tag), data),
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

    ptr: c.IM3Environment,

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

    ptr: c.IM3Runtime,

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
        var fun_ptr: c.IM3Function = undefined;
        try check(c.m3_FindFunction(&fun_ptr, self.ptr, name.ptr));
        return .{ .ptr = fun_ptr };
    }
};

pub const Module = struct {
    const Self = @This();

    ptr: c.IM3Module,
    loaded: bool = false,

    pub fn parse(env: Environment, bytecode: []const u8) Error!Self {
        var ptr: c.IM3Module = undefined;
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

    ptr: c.IM3Function,

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

pub const RawFunction = fn(
    Runtime,
    params: []const Value,
    out_results: []Value,
) void;

/// calls
fn rawZigFunctionWrapper(
    runtime: c.IM3Runtime,
    import_ctx: c.IM3ImportContext,
    stack: [*c]u64,
    mem: ?*anyopaque,
) callconv(.C) ?*const anyopaque {
    _ = mem;

    const function = Function{ .ptr = import_ctx[0].function };
    const raw_function: *const RawFunction = @ptrCast(import_ctx[0].userdata);

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
            }
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
