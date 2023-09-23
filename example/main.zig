const std = @import("std");
const w3 = @import("wasm3");

// zig fmt: off
// (module
//     (func (result i32 f32)
//         i32.const 1234
//         f32.const 5678.9
//     )
//     (export "main" (func 0))
// )
const bytecode = [_]u8{
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x06, 0x01, 0x60,
    0x00, 0x02, 0x7f, 0x7d, 0x03, 0x02, 0x01, 0x00, 0x07, 0x08, 0x01, 0x04,
    0x6d, 0x61, 0x69, 0x6e, 0x00, 0x00, 0x0a, 0x0c, 0x01, 0x0a, 0x00, 0x41,
    0xd2, 0x09, 0x43, 0x33, 0x77, 0xb1, 0x45, 0x0b
};
// zig fmt: on

pub fn main() !void {
    const env = w3.Environment.init();
    defer env.deinit();

    const runtime = w3.Runtime.init(env, .{});
    defer runtime.deinit();

    var module = try w3.Module.parse(env, &bytecode);
    defer module.deinit();

    try runtime.load(&module);

    const main_func = try runtime.findFunction("main");

    // TODO zig interface
    _ = w3.c.m3_CallV(main_func.ptr);
    var ret0: i32 = undefined;
    var ret1: f32 = undefined;
    _ = w3.c.m3_GetResultsV(main_func.ptr, &ret0, &ret1);

    std.debug.print("got results: {d} {d}\n", .{ ret0, ret1 });
}
