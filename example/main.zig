const std = @import("std");
const w3 = @import("wasm3");

const bytecode = @embedFile("example.wasm");

fn printValue(
    runtime: w3.Runtime,
    args: []const w3.Value,
    results: []w3.Value,
) void {
    _ = runtime;
    std.debug.assert(args.len == 1);
    std.debug.assert(results.len == 0);

    std.debug.print("received wasm value: {}\n", .{args[0]});
}

pub fn main() !void {
    const env = w3.Environment.init();
    defer env.deinit();

    const runtime = w3.Runtime.init(env, .{});
    defer runtime.deinit();

    var module = try w3.Module.parse(env, bytecode);
    defer module.deinit();

    try runtime.load(&module);

    try module.linkRawFunction(
        "zig",
        "printValue",
        &.{.i32},
        &.{},
        &printValue,
    );

    const main_func = try runtime.findFunction("example");

    var buf: [256]u8 = undefined;
    const results = try main_func.callBuf(&buf, &.{});
    _ = results;
}
