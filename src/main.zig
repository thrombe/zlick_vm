const std = @import("std");

const zlick_mod = @import("zlick.zig");
const Zlick = zlick_mod.Zlick;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const stdout = std.io.getStdOut().writer();

    var zlick = try Zlick.new(alloc);
    defer zlick.deinit();

    if (args.len > 2) {
        try stdout.print("lokx [script]\n", .{});
    } else if (args.len == 2) {
        try zlick.run_file(args[1]);
    } else {
        try zlick.repl();
    }
}
