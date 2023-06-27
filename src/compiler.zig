const std = @import("std");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Instruction = code_mod.Instruction;

const lexer_mod = @import("lexer.zig");
const Lexer = lexer_mod.Lexer;
const TokenInfo = lexer_mod.TokenInfo;
const Token = lexer_mod.Token;

const parser_mod = @import("parser.zig");
const Stmt = parser_mod.Stmt;
const Expr = parser_mod.Expr;

pub const Compiler = struct {
    const Self = @This();
    const Error = error{
        UnexpectedEof,
        ExpectedEof,
    };

    chunk: *Chunk,
    lexer: Lexer,

    had_error: bool = false,
    panic_mode: bool = false,

    prev: TokenInfo = undefined,
    curr: TokenInfo = undefined,

    pub fn new(chunk: *Chunk, alloc: std.mem.Allocator) !Self {
        return .{ .chunk = chunk, .lexer = try Lexer.new("", alloc) };
    }

    pub fn deinit(self: *Self) void {
        self.lexer.deinit();
    }

    fn write_instruction(self: *Self, inst: Instruction) !void {
        try self.chunk.write_instruction(inst, self.lexer.line);
    }

    pub fn compile_stmt(self: *Self, stmt: *Stmt) !void {
        _ = self;
        _ = stmt;
    }

    pub fn compile_expr(self: *Self, expr: *Expr) !void {
        _ = self;
        _ = expr;
    }
};
