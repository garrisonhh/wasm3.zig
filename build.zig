const std = @import("std");
const fs = std.fs;
const Build = std.Build;
const Allocator = std.mem.Allocator;

const wasm3_source_dir = "wasm3";
const wasm3_cflags: []const []const u8 = &.{};

/// walks the wasm3_source_dir and adds all of the `.c` files to a compile step
fn addCSources(b: *Build, com: *Build.Step.Compile) !void {
    const ally = b.allocator;

    var wasm3_dir = try fs.cwd().openIterableDir(wasm3_source_dir, .{});
    defer wasm3_dir.close();

    var walker = try wasm3_dir.walk(ally);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;

        const ext = fs.path.extension(entry.path);
        if (!std.mem.eql(u8, ext, ".c")) continue;

        const path = try fs.path.join(ally, &.{wasm3_source_dir, entry.path});
        defer ally.free(path);

        com.addCSourceFile(.{
            .file = .{ .path = path },
            .flags = wasm3_cflags,
        });
    }
}

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "wasm3",
        .target = target,
        .optimize = optimize,
    });

    lib.linkLibC();
    lib.addIncludePath(.{ .path = wasm3_source_dir });
    addCSources(b, lib) catch |e| {
        std.debug.panic("unhandled error: {s}", .{@errorName(e)});
    };

    b.installArtifact(lib);

    const module = b.addModule("wasm3", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    const example = b.addExecutable(.{
        .name = "example",
        .root_source_file = .{ .path = "example/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    example.step.dependOn(&lib.step);
    example.addModule("wasm3", module);

    const run_example = b.addRunArtifact(example);
    const run_example_step = b.step("run", "run example");
    run_example_step.dependOn(&run_example.step);
}
