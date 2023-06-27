const std = @import("std");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;

const vm_mod = @import("vm.zig");
const Vm = vm_mod.Vm;

const lexer_mod = @import("lexer.zig");
const Lexer = lexer_mod.Lexer;
const Token = lexer_mod.Token;

const compiler_mod = @import("compiler.zig");
const Compiler = compiler_mod.Compiler;

const parser_mod = @import("parser.zig");
const Parser = parser_mod.Parser;

pub const Zlick = struct {
    const Self = @This();

    alloc: std.mem.Allocator,
    chunk: *Chunk,
    vm: Vm,

    had_err: bool = false,

    pub fn new(alloc: std.mem.Allocator) !Self {
        var chunk = try alloc.create(Chunk);
        chunk.* = Chunk.new(alloc);
        const vm = try Vm.new(chunk);
        return .{ .alloc = alloc, .chunk = chunk, .vm = vm };
    }

    pub fn deinit(self: *Self) void {
        self.vm.deinit();
        self.chunk.deinit();
        self.alloc.destroy(self.chunk);
    }

    pub fn repl(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();
        const stdin = std.io.getStdIn().reader();

        // assume users are not gonna type in strings too long
        var buff: [1024]u8 = undefined;
        while (true) {
            try stdout.print("> ", .{});

            if (stdin.readUntilDelimiterOrEof(&buff, '\n') catch null) |line| {
                try self.run(line);
            } else {
                try stdout.print("\n", .{});
                break;
            }
        }
    }

    pub fn run_file(self: *Self, fp: []const u8) !void {
        var f = try std.fs.cwd().openFile(fp, .{});
        defer f.close();

        var str = try f.readToEndAlloc(self.alloc, 10_000_000);
        defer self.alloc.free(str);

        try self.run(str);
    }

    fn run(self: *Self, code: []const u8) !void {
        var lexer = try Lexer.new(code, self.alloc);
        defer lexer.deinit();

        var compiler = try Compiler.new(self.chunk);
        defer compiler.deinit();

        var tokens = std.ArrayList(Token).init(self.alloc);
        while (try lexer.next()) |token| {
            try tokens.append(token.tok);
            // std.debug.print("{any}\n", .{token});
        }

        var parser = Parser.new(tokens.toOwnedSlice(), self.alloc);
        defer parser.deinit();

        while (try parser.next_stmt()) |s| {
            defer s.free(self.alloc);

            // std.debug.print("{any}\n", .{s});

            if (self.had_err) {
                continue;
            }

            var r = try compiler.compile_stmt(s);
            _ = r;

            // if (r) |res| {
            //     switch (res) {
            //         .Void => {},
            //         .Continue => return error.BadContinue,
            //         .Break => return error.BadBreak,
            //         .Return => return error.BadReturn,
            //     }
            // } else |err| {
            //     std.debug.print("{}\n", .{err});
            //     self.had_err = true;
            // }
        }

        var vm = try Vm.new(self.chunk);
        defer vm.deinit();

        _ = try vm.run();
    }
};
