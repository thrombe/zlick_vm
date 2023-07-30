const std = @import("std");

const zlick_mod = @import("zlick.zig");
const Zlick = zlick_mod.Zlick;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    var zgpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = zgpa.deinit();
    // var zgpa = std.heap.FixedBufferAllocator.init(try alloc.alloc(u8, 10_000_000));
    // defer alloc.free(zgpa.buffer);
    const zalloc = zgpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const stdout = std.io.getStdOut().writer();

    var zlick = try Zlick.new(alloc, zalloc);
    defer zlick.deinit();

    if (args.len > 2) {
        try stdout.print("zlick [script]\n", .{});
    } else if (args.len == 2) {
        try zlick.run_file(args[1]);
    } else {
        try zlick.repl();
    }
}
