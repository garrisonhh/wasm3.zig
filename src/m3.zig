//! the wasm3.h header, translated and edited

pub const M3Value = extern struct {
    pub const Type = enum(c_uint) {
        none = 0,
        i32 = 1,
        i64 = 2,
        f32 = 3,
        f64 = 4,
        unknown = 5,
    };

    pub const Data = extern union {
        i32: u32,
        i64: u64,
        f32: f32,
        f64: f64,
    };

    type: Type,
    value: Data,
};

// environment =================================================================

pub const M3Environment = opaque {};

pub extern fn m3_NewEnvironment() *M3Environment;
pub extern fn m3_FreeEnvironment(i_environment: *M3Environment) void;

pub const M3SectionHandler = *const fn (
    i_module: *M3Module,
    name: [*:0]const u8,
    start: [*]const u8,
    end: [*]const u8,
) callconv(.C) M3Result;

pub extern fn m3_SetCustomSectionHandler(
    i_environment: *M3Environment,
    i_handler: M3SectionHandler,
) void;

// runtime =====================================================================

pub const M3Runtime = opaque {};

pub extern fn m3_NewRuntime(
    io_environment: *M3Environment,
    i_stackSizeInBytes: u32,
    i_userdata: ?*anyopaque,
) *M3Runtime;
pub extern fn m3_FreeRuntime(i_runtime: *M3Runtime) void;

pub extern fn m3_GetMemory(
    i_runtime: *M3Runtime,
    o_memorySizeInBytes: *u32,
    i_memoryIndex: u32,
) [*]u8;
pub extern fn m3_GetMemorySize(i_runtime: *M3Runtime) u32;
pub extern fn m3_GetUserData(i_runtime: *M3Runtime) ?*anyopaque;

pub extern fn m3_LoadModule(
    io_runtime: *M3Runtime,
    io_module: *M3Module,
) M3Result;

// modules =====================================================================

pub const M3Module = opaque {};

pub extern fn m3_ParseModule(
    i_environment: *M3Environment,
    o_module: **M3Module,
    i_wasmBytes: [*]const u8,
    i_numWasmBytes: u32,
) M3Result;
pub extern fn m3_FreeModule(i_module: *M3Module) void;
pub extern fn m3_CompileModule(io_module: *M3Module) M3Result;
pub extern fn m3_RunStart(i_module: *M3Module) M3Result;

pub extern fn m3_GetModuleName(i_module: *M3Module) [*:0]const u8;
pub extern fn m3_SetModuleName(i_module: *M3Module, name: [*:0]const u8) void;
pub extern fn m3_GetModuleRuntime(i_module: *M3Module) *M3Runtime;

// globals =====================================================================

pub const M3Global = opaque {};

pub extern fn m3_FindGlobal(
    io_module: *M3Module,
    i_globalName: [*:0]const u8,
) *M3Global;
pub extern fn m3_GetGlobal(
    i_global: *M3Global,
    o_value: *M3Value,
) M3Result;
pub extern fn m3_SetGlobal(
    i_global: *M3Global,
    i_value: *M3Value,
) M3Result;
pub extern fn m3_GetGlobalType(i_global: *M3Global) M3Value.Type;

// raw function linking ========================================================

pub const M3ImportContext = extern struct {
    userdata: ?*anyopaque,
    function: *M3Function,
};

pub const M3RawCall = *const fn (
    *M3Runtime,
    *M3ImportContext,
    [*]u64,
    ?*anyopaque,
) callconv(.C) ?*const anyopaque;

pub extern fn m3_LinkRawFunction(
    io_module: *M3Module,
    i_moduleName: [*:0]const u8,
    i_functionName: [*:0]const u8,
    i_signature: [*:0]const u8,
    i_function: M3RawCall,
) M3Result;
pub extern fn m3_LinkRawFunctionEx(
    io_module: *M3Module,
    i_moduleName: [*:0]const u8,
    i_functionName: [*:0]const u8,
    i_signature: [*:0]const u8,
    i_function: M3RawCall,
    i_userdata: ?*const anyopaque,
) M3Result;

// functions ===================================================================

pub const M3Function = opaque {};

pub extern fn m3_FindFunction(
    o_function: **M3Function,
    i_runtime: *M3Runtime,
    i_functionName: [*:0]const u8,
) M3Result;
pub extern fn m3_GetArgCount(i_function: *M3Function) u32;
pub extern fn m3_GetRetCount(i_function: *M3Function) u32;
pub extern fn m3_GetArgType(i_function: *M3Function, i_index: u32) M3Value.Type;
pub extern fn m3_GetRetType(i_function: *M3Function, i_index: u32) M3Value.Type;

pub extern fn m3_Call(
    i_function: *M3Function,
    i_argc: u32,
    i_argptrs: [*]*const anyopaque,
) M3Result;
pub extern fn m3_GetResults(
    i_function: *M3Function,
    i_retc: u32,
    o_retptrs: [*]*const anyopaque,
) M3Result;

pub extern fn m3_GetFunctionName(i_function: *M3Function) [*:0]const u8;
pub extern fn m3_GetFunctionModule(i_function: *M3Function) *M3Module;

pub extern fn m3_Yield() M3Result;

// errors ======================================================================

pub const M3ErrorInfo = extern struct {
    result: M3Result,
    runtime: *M3Runtime,
    module: *M3Module,
    function: *M3Function,
    file: [*:0]const u8,
    line: u32,
    message: [*:0]const u8,
};

pub extern fn m3_GetErrorInfo(i_runtime: *M3Runtime, o_info: *M3ErrorInfo) void;
pub extern fn m3_ResetErrorInfo(i_runtime: *M3Runtime) void;

// backtraces ==================================================================

pub const M3BacktraceFrame = extern struct {
    moduleOffset: u32,
    function: *M3Function,
    next: ?*const M3BacktraceFrame,
};

pub const M3BacktraceInfo = extern struct {
    frames: *const M3BacktraceFrame,
    lastFrame: *const M3BacktraceFrame,
};

pub extern fn m3_GetBacktrace(i_runtime: *M3Runtime) *const M3BacktraceInfo;

// debugging ===================================================================

pub extern fn m3_PrintRuntimeInfo(i_runtime: *M3Runtime) void;
pub extern fn m3_PrintM3Info() void;
pub extern fn m3_PrintProfilerInfo() void;

// errors ======================================================================

pub const M3Result = ?[*:0]const u8;

pub extern const m3Err_none: M3Result;
pub extern const m3Err_mallocFailed: M3Result;
pub extern const m3Err_incompatibleWasmVersion: M3Result;
pub extern const m3Err_wasmMalformed: M3Result;
pub extern const m3Err_misorderedWasmSection: M3Result;
pub extern const m3Err_wasmUnderrun: M3Result;
pub extern const m3Err_wasmOverrun: M3Result;
pub extern const m3Err_wasmMissingInitExpr: M3Result;
pub extern const m3Err_lebOverflow: M3Result;
pub extern const m3Err_missingUTF8: M3Result;
pub extern const m3Err_wasmSectionUnderrun: M3Result;
pub extern const m3Err_wasmSectionOverrun: M3Result;
pub extern const m3Err_invalidTypeId: M3Result;
pub extern const m3Err_tooManyMemorySections: M3Result;
pub extern const m3Err_tooManyArgsRets: M3Result;
pub extern const m3Err_moduleNotLinked: M3Result;
pub extern const m3Err_moduleAlreadyLinked: M3Result;
pub extern const m3Err_functionLookupFailed: M3Result;
pub extern const m3Err_functionImportMissing: M3Result;
pub extern const m3Err_malformedFunctionSignature: M3Result;
pub extern const m3Err_noCompiler: M3Result;
pub extern const m3Err_unknownOpcode: M3Result;
pub extern const m3Err_restrictedOpcode: M3Result;
pub extern const m3Err_functionStackOverflow: M3Result;
pub extern const m3Err_functionStackUnderrun: M3Result;
pub extern const m3Err_mallocFailedCodePage: M3Result;
pub extern const m3Err_settingImmutableGlobal: M3Result;
pub extern const m3Err_typeMismatch: M3Result;
pub extern const m3Err_typeCountMismatch: M3Result;
pub extern const m3Err_missingCompiledCode: M3Result;
pub extern const m3Err_wasmMemoryOverflow: M3Result;
pub extern const m3Err_globalMemoryNotAllocated: M3Result;
pub extern const m3Err_globaIndexOutOfBounds: M3Result;
pub extern const m3Err_argumentCountMismatch: M3Result;
pub extern const m3Err_argumentTypeMismatch: M3Result;
pub extern const m3Err_globalLookupFailed: M3Result;
pub extern const m3Err_globalTypeMismatch: M3Result;
pub extern const m3Err_globalNotMutable: M3Result;
pub extern const m3Err_trapOutOfBoundsMemoryAccess: M3Result;
pub extern const m3Err_trapDivisionByZero: M3Result;
pub extern const m3Err_trapIntegerOverflow: M3Result;
pub extern const m3Err_trapIntegerConversion: M3Result;
pub extern const m3Err_trapIndirectCallTypeMismatch: M3Result;
pub extern const m3Err_trapTableIndexOutOfRange: M3Result;
pub extern const m3Err_trapTableElementIsNull: M3Result;
pub extern const m3Err_trapExit: M3Result;
pub extern const m3Err_trapAbort: M3Result;
pub extern const m3Err_trapUnreachable: M3Result;
pub extern const m3Err_trapStackOverflow: M3Result;
