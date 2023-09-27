const std = @import("std");
const fs = std.fs;
const Build = std.Build;
const Allocator = std.mem.Allocator;

const wasm3_source_dir = "wasm3";
const wasm3_cflags: []const []const u8 = &.{};

/// walks the wasm3_source_dir and adds all of the `.c` files to a compile step
fn addCSources(b: *Build, com: *Build.Step.Compile) !void {
    const ally = b.allocator;

    const build_dir = b.build_root.handle;
    var wasm3_dir = try build_dir.openIterableDir(wasm3_source_dir, .{});
    defer wasm3_dir.close();

    var walker = try wasm3_dir.walk(ally);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;

        const ext = fs.path.extension(entry.path);
        if (!std.mem.eql(u8, ext, ".c")) continue;

        const path = try fs.path.join(ally, &.{ wasm3_source_dir, entry.path });
        defer ally.free(path);

        const lazy = Build.LazyPath{ .path = path };
        const abspath = lazy.getPath(b);

        com.addCSourceFile(.{
            .file = .{ .cwd_relative = abspath },
            .flags = wasm3_cflags,
        });
    }
}

/// builds the  example
pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("wasm3", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    const example = b.addExecutable(.{
        .name = "example",
        .root_source_file = .{ .path = "example/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(example);

    example.linkLibC();
    example.addIncludePath(.{ .path = wasm3_source_dir });
    try addCSources(b, example);

    if (optimize == .Debug) {
        example.defineCMacro("DEBUG", null);
    } else {
        example.defineCMacro("NDEBUG", null);
    }

    example.addModule("wasm3", module);

    const run_example = b.addRunArtifact(example);
    const run_example_step = b.step("run", "run example");
    run_example_step.dependOn(&run_example.step);
}
