const std = @import("std");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Instruction = code_mod.Instruction;
const Opcode = code_mod.Opcode;

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
        UndefinedVariable,
    };

    chunk: *Chunk,
    locals: LocalMan,

    const LocalMan = struct {
        const LocalsList = std.ArrayList(Local);
        locals: LocalsList,
        curr_scope: u32 = 0,

        fn new(alloc: std.mem.Allocator) LocalMan {
            return .{ .locals = LocalsList.init(alloc) };
        }

        fn deinit(self: *LocalMan) void {
            self.locals.deinit();
        }

        fn define(self: *LocalMan, name: []const u8) !void {
            try self.locals.append(.{ .name = name, .depth = self.curr_scope });
        }

        fn resolve(self: *LocalMan, name: []const u8) ?u8 {
            for (self.locals.items) |_, i| {
                const j = self.locals.items.len - i - 1;
                const val = self.locals.items[j];
                if (std.mem.eql(u8, val.name, name)) {
                    return @intCast(u8, j);
                }
            }

            return null;
        }

        fn begin_scope(self: *LocalMan) void {
            self.curr_scope += 1;
        }

        fn end_scope(self: *LocalMan) u8 {
            self.curr_scope -= 1;

            var count: u8 = 0;
            for (self.locals.items) |_| {
                const j = self.locals.items.len - 1;
                const val = self.locals.items[j];
                if (val.depth == self.curr_scope) {
                    break;
                }
                _ = self.locals.pop();
                count += 1;
            }
            // while (self.locals.popOrNull()) |v| {
            //     if (v.depth == self.curr_scope) {
            //         try self.locals.append(v);
            //         break;
            //     }
            //     count += 1;
            // }

            return count;
        }
    };

    const Local = struct {
        name: []const u8,
        depth: u32,
    };

    pub fn new(chunk: *Chunk) !Self {
        return .{ .chunk = chunk, .locals = LocalMan.new(chunk.alloc) };
    }

    pub fn deinit(self: *Self) void {
        self.locals.deinit();
    }

    fn write_instruction(self: *Self, inst: Instruction) !void {
        try self.chunk.write_instruction(inst, 0);
    }

    fn edit_instruction(self: *Self, inst: Instruction, pos: usize) !void {
        try self.chunk.edit_instruction(inst, pos);
    }

    fn patch_jmp(self: *Self, comptime opcode: Opcode, pos: usize) !void {
        const target = self.chunk.code.items.len;

        const offset = target - pos - Instruction.size(opcode);
        try self.edit_instruction(
            @unionInit(
                Instruction,
                @tagName(opcode),
                @intCast(std.meta.TagPayload(Instruction, opcode), offset),
            ),
            pos,
        );
    }

    fn loop_to(self: *Self, pos: usize) !void {
        const inst: Opcode = .Loop;
        const offset = self.chunk.code.items.len - pos + Instruction.size(inst);
        try self.write_instruction(@unionInit(
            Instruction,
            @tagName(inst),
            @intCast(std.meta.TagPayload(Instruction, inst), offset),
        ));
    }

    pub fn compile_stmt(self: *Self, stmt: *Stmt) !void {
        switch (stmt.*) {
            .Expr => |expr| {
                try self.compile_expr(expr);
                try self.write_instruction(.Pop);
            },
            .Print => |expr| {
                try self.compile_expr(expr);
                try self.write_instruction(.Print);
            },
            .Let => |val| {
                if (val.init_expr) |expr| {
                    try self.compile_expr(expr);
                } else {
                    try self.write_instruction(.ConstNone);
                }

                if (self.locals.curr_scope == 0) {
                    var obj = try self.chunk.alloc.create(code_mod.String);
                    obj.* = code_mod.String.new(.{ .str = val.name });
                    const s = try self.chunk.write_constant(.{ .Object = &obj.tag });

                    try self.write_instruction(.{ .DefineGlobal = s });
                } else {
                    try self.locals.define(val.name);
                    // variable is implicitly 'set'
                }
            },
            .Assign => |val| {
                try self.compile_expr(val.expr);

                if (self.locals.curr_scope == 0) {
                    var obj = try self.chunk.alloc.create(code_mod.String);
                    obj.* = code_mod.String.new(.{ .str = val.name });
                    const s = try self.chunk.write_constant(.{ .Object = &obj.tag });

                    try self.write_instruction(.{ .SetGlobal = s });
                } else {
                    if (self.locals.resolve(val.name)) |i| {
                        try self.write_instruction(.{ .SetLocal = i });
                    } else {
                        return error.UndefinedVariable;
                    }
                }
            },
            .Block => |val| {
                self.locals.begin_scope();
                defer {
                    const num = self.locals.end_scope();
                    self.write_instruction(.{ .PopN = num }) catch unreachable;
                }

                for (val) |s| {
                    try self.compile_stmt(s);
                }
            },
            .If => |val| {
                try self.compile_expr(val.condition);

                const if_jmp = self.chunk.code.items.len;
                try self.write_instruction(.{ .JmpIfFalse = 0 });

                try self.write_instruction(.Pop);
                try self.compile_stmt(val.if_block);

                const else_jmp = self.chunk.code.items.len;
                try self.write_instruction(.{ .Jmp = 0 });

                try self.patch_jmp(.JmpIfFalse, if_jmp);

                try self.write_instruction(.Pop);
                if (val.else_block) |els| {
                    try self.compile_stmt(els);
                }

                try self.patch_jmp(.Jmp, else_jmp);
            },
            .While => |val| {
                const loop_start = self.chunk.code.items.len;
                try self.compile_expr(val.condition);

                const skip = self.chunk.code.items.len;
                try self.write_instruction(.{ .JmpIfFalse = 0 });

                try self.write_instruction(.Pop);
                try self.compile_stmt(val.block);

                try self.loop_to(loop_start);

                try self.patch_jmp(.JmpIfFalse, skip);
                try self.write_instruction(.Pop);
            },
            .For => |val| {
                self.locals.begin_scope();
                defer {
                    const num = self.locals.end_scope();
                    self.write_instruction(.{ .PopN = num }) catch unreachable;
                }

                if (val.start) |start| {
                    try self.compile_stmt(start);
                }
                const loop_start = self.chunk.code.items.len;
                var end_jmp: ?usize = null;

                if (val.mid) |mid| {
                    try self.compile_expr(mid);
                    end_jmp = self.chunk.code.items.len;
                    try self.write_instruction(.{ .JmpIfFalse = 0 });
                    try self.write_instruction(.Pop);
                }

                try self.compile_stmt(val.block);
                if (val.end) |end| {
                    try self.compile_stmt(end);
                }

                try self.loop_to(loop_start);

                if (end_jmp) |end| {
                    try self.patch_jmp(.JmpIfFalse, end);
                    try self.write_instruction(.Pop);
                }
            },
            else => return error.Unimplemented,
        }
    }

    pub fn compile_expr(self: *Self, expr: *Expr) !void {
        switch (expr.*) {
            .Literal => |val| {
                switch (val) {
                    .Number => |str| {
                        const num = try std.fmt.parseFloat(f64, str);
                        const index = try self.chunk.write_constant(.{ .Number = num });
                        try self.write_instruction(.{ .Constant = index });
                    },
                    .None => try self.write_instruction(.ConstNone),
                    .True => try self.write_instruction(.ConstTrue),
                    .False => try self.write_instruction(.ConstFalse),
                    .String => |str| {
                        var obj = try self.chunk.alloc.create(code_mod.String);

                        obj.* = code_mod.String.new(.{ .str = str });
                        const s = try self.chunk.write_constant(.{ .Object = &obj.tag });

                        try self.write_instruction(.{ .Constant = s });
                    },
                }
            },
            .Unary => |val| {
                try self.compile_expr(val.oparand);
                switch (val.operator) {
                    .Dash => try self.write_instruction(.Negate),
                    .Bang => try self.write_instruction(.LogicalNot),
                    else => return error.BadUnaryOperator,
                }
            },
            .Binary => |val| {
                switch (val.operator) {
                    .And => {
                        try self.compile_expr(val.left);

                        const skip_jmp = self.chunk.code.items.len;
                        try self.write_instruction(.{ .JmpIfFalse = 0 });
                        try self.write_instruction(.Pop);

                        try self.compile_expr(val.right);

                        try self.patch_jmp(.JmpIfFalse, skip_jmp);

                        return;
                    },
                    .Or => {
                        try self.compile_expr(val.left);

                        const skip_jmp = self.chunk.code.items.len;
                        try self.write_instruction(.{ .JmpIfTrue = 0 });
                        try self.write_instruction(.Pop);

                        try self.compile_expr(val.right);

                        try self.patch_jmp(.JmpIfTrue, skip_jmp);

                        return;
                    },
                    else => {},
                }
                try self.compile_expr(val.left);
                try self.compile_expr(val.right);
                switch (val.operator) {
                    .Plus => try self.write_instruction(.Add),
                    .Dash => try self.write_instruction(.Subtract),
                    .Star => try self.write_instruction(.Multiply),
                    .Slash => try self.write_instruction(.Divide),
                    .Gt => try self.write_instruction(.GreaterThan),
                    .Lt => try self.write_instruction(.LessThan),
                    .DoubleEqual => try self.write_instruction(.Equal),
                    .BangEqual => {
                        try self.write_instruction(.Equal);
                        try self.write_instruction(.LogicalNot);
                    },
                    .Gte => {
                        try self.write_instruction(.LessThan);
                        try self.write_instruction(.LogicalNot);
                    },
                    .Lte => {
                        try self.write_instruction(.GreaterThan);
                        try self.write_instruction(.LogicalNot);
                    },
                    else => return error.BadBinaryOperator,
                }
            },
            .Group => |val| {
                try self.compile_expr(val);
            },
            .Variable => |val| {
                var obj = try self.chunk.alloc.create(code_mod.String);

                if (self.locals.curr_scope == 0) {
                    obj.* = code_mod.String.new(.{ .str = val });
                    const s = try self.chunk.write_constant(.{ .Object = &obj.tag });

                    try self.write_instruction(.{ .GetGlobal = s });
                } else {
                    try self.write_instruction(.{ .GetLocal = self.locals.resolve(val) orelse return error.UndefinedVariable });
                }
            },
            else => return error.Unimplemented,
        }
    }
};
