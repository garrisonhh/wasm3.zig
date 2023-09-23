const std = @import("std");
const builtin = @import("builtin");
const in_debug = builtin.mode == .Debug;

/// keep public for hackability
pub const c = @cImport({
    @cInclude("wasm3.h");
});

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
    name: ?[:0]const u8 = null,
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

    pub fn getName(self: Self) ?[:0]const u8 {
        return @ptrCast(self.name);
    }
};

pub const Function = struct {
    const Self = @This();

    ptr: c.IM3Function,

    pub fn call(self: Self) Error!void {
        _ = self;
        @panic("TODO");
    }
};

// debugging functions =========================================================

fn wrapDebugFn(comptime f: fn () callconv(.C) void) fn () void {
    return struct {
        fn wrapped() void {
            if (in_debug) f();
        }
    }.wrapped;
}

pub const printRuntimeInfo = wrapDebugFn(c.m3_PrintRuntimeInfo);
pub const printM3Info = wrapDebugFn(c.m3_PrintM3Info);
pub const printProfilerInfo = wrapDebugFn(c.m3_PrintProfilerInfo);
