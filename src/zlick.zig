const std = @import("std");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Disassembler = code_mod.Disassembler;

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
    vm: Vm,

    had_err: bool = false,

    pub fn new(alloc: std.mem.Allocator) !Self {
        const vm = try Vm.new(alloc);
        return .{ .alloc = alloc, .vm = vm };
    }

    pub fn deinit(self: *Self) void {
        self.vm.deinit();
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

        var func = try self.alloc.create(code_mod.Function);
        func.* = code_mod.Function.new(.{
            .arity = 0,
            .name = "<script>",
            .chunk = code_mod.Chunk.new(),
        });
        var closure = try self.alloc.create(code_mod.Closure);
        closure.* = code_mod.Closure.new(.{
            .func = func,
            .upvalues = try self.alloc.alloc(*code_mod.Upvalue, 0),
        });
        var compiler = try Compiler.new(&closure.inner.func.inner.chunk, self.alloc);
        defer compiler.deinit();

        var tokens = std.ArrayList(Token).init(self.alloc);
        while (try lexer.next()) |token| {
            try tokens.append(token.tok);
            // std.debug.print("{any}\n", .{token});
        }

        var parser = Parser.new(tokens.toOwnedSlice(), self.alloc);
        defer parser.deinit();

        while (true) {
            const s = parser.next_stmt() catch |err| {
                self.had_err = true;
                std.debug.print("{}\n", .{err});
                continue;
            } orelse {
                break;
            };
            defer s.free(self.alloc);

            // std.debug.print("{any}\n", .{s});

            if (self.had_err) {
                continue;
            }

            var r = compiler.compile_stmt(s) catch |err| {
                self.had_err = true;
                std.debug.print("{}\n", .{err});
            };
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

        if (self.had_err) {
            return;
        }

        try compiler.end_script();

        var dis = Disassembler.new(&closure.inner.func.inner.chunk);
        try dis.disassemble_chunk(closure.inner.func.inner.name);

        _ = try self.vm.start_script(closure);
    }
};
