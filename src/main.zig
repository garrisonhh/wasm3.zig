const std = @import("std");

pub const c = @cImport({
    @cInclude("wasm3.h");
});