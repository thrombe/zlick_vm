const std = @import("std");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Instruction = code_mod.Instruction;

const lexer_mod = @import("lexer.zig");
const TokenInfo = lexer_mod.TokenInfo;
const Token = lexer_mod.Token;

const parser_mod = @import("parser.zig");
const Stmt = parser_mod.Stmt;
const Expr = parser_mod.Expr;

pub const Compiler = struct {
    const Self = @This();
    const Error = error{
        Unimplemented,
        BadUnaryOperator,
        BadBinaryOperator,
    };

    chunk: *Chunk,

    had_error: bool = false,
    panic_mode: bool = false,

    prev: TokenInfo = undefined,
    curr: TokenInfo = undefined,

    pub fn new(chunk: *Chunk) !Self {
        return .{ .chunk = chunk };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    fn write_instruction(self: *Self, inst: Instruction) !void {
        try self.chunk.write_instruction(inst, 0);
    }

    pub fn compile_stmt(self: *Self, stmt: *Stmt) !void {
        switch (stmt.*) {
            .Expr => |expr| {
                try self.compile_expr(expr);

                // TEMP:
                try self.write_instruction(.Return);
            },
            else => return error.Unimplemented,
        }
    }

    pub fn compile_expr(self: *Self, expr: *Expr) !void {
        switch (expr.*) {
            .Literal => |val| {
                switch (val) {
                    .Number => |str| {
                        const index = try self.chunk.write_constant(try std.fmt.parseFloat(f64, str));
                        try self.write_instruction(.{ .Constant = index });
                    },
                    else => return error.Unimplemented,
                }
            },
            .Unary => |val| {
                try self.compile_expr(val.oparand);
                switch (val.operator) {
                    .Dash => {
                        try self.write_instruction(.Negate);
                    },
                    else => return error.BadUnaryOperator,
                }
            },
            .Binary => |val| {
                try self.compile_expr(val.left);
                try self.compile_expr(val.right);
                switch (val.operator) {
                    .Plus => {
                        try self.write_instruction(.Add);
                    },
                    .Dash => {
                        try self.write_instruction(.Subtract);
                    },
                    .Star => {
                        try self.write_instruction(.Multiply);
                    },
                    .Slash => {
                        try self.write_instruction(.Divide);
                    },
                    else => return error.BadBinaryOperator,
                }
            },
            .Group => |val| {
                try self.compile_expr(val);
            },
            else => return error.Unimplemented,
        }
    }
};
