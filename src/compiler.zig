const std = @import("std");

const build_options = @import("build_options");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Instruction = code_mod.Instruction;
const Opcode = code_mod.Opcode;
const Function = code_mod.Function;

const lexer_mod = @import("lexer.zig");
const TokenInfo = lexer_mod.TokenInfo;
const Token = lexer_mod.Token;

const parser_mod = @import("parser.zig");
const Stmt = parser_mod.Stmt;
const Expr = parser_mod.Expr;

const vm_mod = @import("vm.zig");
const Allocator = vm_mod.Allocator;

pub const Compiler = struct {
    const Self = @This();
    const Error = error{
        Unimplemented,
        BadUnaryOperator,
        BadBinaryOperator,
        UndefinedVariable,
        NotInsideLoop,
        CannotInheritFromSelf,
    };

    const Upvalues = std.ArrayList(UpvalueRef);
    // this can be replaced with a tagged union LocalIndex: u8, UpvalueIndex: u8
    pub const UpvalueRef = struct {
        index: u8,
        is_local: bool,
    };

    pub const FunctionType = enum {
        Function,
        Method,
        Script,
        Initializer,
    };

    chunk: *Chunk,
    locals: LocalMan,
    upvalues: Upvalues,
    enclosing: ?*Self,
    alloc: std.mem.Allocator,
    zalloc: *Allocator,

    func_type: FunctionType,

    const LocalMan = struct {
        const LocalsList = std.ArrayList(Local);
        const Local = struct {
            name: []const u8,
            depth: u32,
            is_captured: bool,
        };

        locals: LocalsList,
        curr_scope: u32 = 0,

        fn new(alloc: std.mem.Allocator) LocalMan {
            return .{ .locals = LocalsList.init(alloc) };
        }

        fn deinit(self: *LocalMan) void {
            self.locals.deinit();
        }

        fn define(self: *LocalMan, name: []const u8) !void {
            try self.locals.append(.{ .name = name, .depth = self.curr_scope, .is_captured = false });
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

        fn end_scope(self: *LocalMan) []Local {
            self.curr_scope -= 1;
            const locals = self.locals.items;

            var count: usize = 0;
            for (self.locals.items) |_| {
                const j = self.locals.items.len - 1;
                const val = self.locals.items[j];
                if (val.depth <= self.curr_scope) {
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

            const len = self.locals.items.len;
            // OOF: ArrayList.pop does not deallocate as of now, so this should be fine :P
            // this slice is of memory outside of self.locals.items
            // reallocations in locals array invalidate the slice
            return locals[len .. len + count];
        }
    };

    fn new(chunk: *Chunk, function_type: FunctionType, alloc: std.mem.Allocator, zalloc: *Allocator) !Self {
        var locals = LocalMan.new(alloc);

        switch (function_type) {
            .Script, .Function => {
                try locals.define("");
            },
            .Method, .Initializer => {
                try locals.define("self");
            },
        }

        return .{
            .locals = locals,
            .chunk = chunk,
            .alloc = alloc,
            .zalloc = zalloc,
            .enclosing = null,
            .upvalues = Upvalues.init(alloc),
            .func_type = function_type,
        };
    }

    pub fn enclosed(self: *Self, chunk: *Chunk, function_type: FunctionType) !Self {
        var comp = try Self.new(chunk, function_type, self.alloc, self.zalloc);
        comp.enclosing = self;
        return comp;
    }

    pub fn deinit(self: *Self) void {
        self.locals.deinit();
        self.upvalues.deinit();
    }

    fn write_constant(self: *Self, constant: code_mod.Value) !u8 {
        return self.chunk.write_constant(constant, self.alloc);
    }

    fn write_instruction(self: *Self, inst: Instruction) !void {
        try self.chunk.write_instruction(inst, 0, self.alloc);
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

    fn resolve_upvalue(self: *Self, name: []const u8) !?u8 {
        if (self.enclosing) |enc| {
            if (enc.locals.resolve(name)) |c| {
                enc.locals.locals.items[c].is_captured = true;
                return try self.add_upvalue(c, true);
            }

            if (try enc.resolve_upvalue(name)) |c| {
                return try self.add_upvalue(c, false);
            }
        }
        return null;
    }

    fn add_upvalue(self: *Self, index: u8, is_local: bool) !u8 {
        for (self.upvalues.items) |uv, i| {
            if (uv.index == index and uv.is_local == is_local) {
                return @intCast(u8, i);
            }
        }
        try self.upvalues.append(.{ .is_local = is_local, .index = index });
        const ind = self.upvalues.items.len - 1;
        return @intCast(u8, ind);
    }

    pub fn new_script(alloc: std.mem.Allocator, zalloc: *Allocator) !Self {
        var chunk = try alloc.create(Chunk);
        chunk.* = Chunk.new(alloc);
        return Self.new(chunk, .Script, alloc, zalloc);
    }

    pub fn end_script(self: *Self) !*code_mod.Closure {
        try self.write_instruction(.ConstNone);
        try self.write_instruction(.Return);

        if (comptime build_options.print_bytecode) {
            var dis = code_mod.Disassembler.new(self.chunk);
            try dis.disassemble_chunk("<script>");
            std.debug.print("----- <code end> -----\n\n", .{});
        }

        var chunk = self.chunk.*;
        self.alloc.destroy(self.chunk);

        var func = try self.zalloc.create(code_mod.Function);
        func.* = code_mod.Function.new(.{
            .arity = 0,
            .name = try self.new_string("<script>"),
            .chunk = chunk,
        });
        try self.zalloc.add_val(func.as_val());

        var closure = try self.zalloc.create(code_mod.Closure);
        closure.* = code_mod.Closure.new(.{
            .func = func,
            .upvalues = try self.zalloc.alloc(*code_mod.Upvalue, 0),
        });
        try self.zalloc.add_val(closure.as_val());
        return closure;
    }

    fn define_function(self: *Self, val: Stmt.Function, function_type: FunctionType) anyerror!void {
        var func = try self.zalloc.create(Function);
        func.* = Function.new(.{
            .arity = @intCast(u32, val.params.len),
            .name = try self.new_string(val.name),
            .chunk = code_mod.Chunk.new(self.alloc),
        });
        try self.zalloc.add_val(func.as_val());

        var comp = try self.enclosed(&func.inner.chunk, function_type);
        defer comp.deinit();
        for (val.params) |name| {
            try comp.locals.define(name);
        }
        try comp.compile_stmt(val.body, null);
        try comp.emit_return(null);

        if (comptime build_options.print_bytecode) {
            var dis = code_mod.Disassembler.new(&func.inner.chunk);
            try dis.disassemble_chunk(val.name);
        }

        try self.write_instruction(.{ .Closure = .{
            .func = try self.write_constant(func.as_val()),
            .upvalues = comp.upvalues.items,
        } });
    }

    fn emit_return(self: *Self, val: ?*Expr) !void {
        // if it is an initializer, then ignore the return value and enforce it to be self
        if (val) |ret| {
            try self.compile_expr(ret);
            if (self.func_type == .Initializer) {
                try self.write_instruction(.Pop);
                try self.write_instruction(.{ .GetLocal = self.locals.resolve("self").? });
            }
        } else if (self.func_type == .Initializer) {
            try self.write_instruction(.{ .GetLocal = self.locals.resolve("self").? });
        } else {
            try self.write_instruction(.ConstNone);
        }
        try self.write_instruction(.Return);
    }

    fn begin_scope(self: *Self) void {
        self.locals.begin_scope();
    }

    fn end_scope(self: *Self) !void {
        const locals = self.locals.end_scope();
        for (locals) |_, i| {
            const j = locals.len - i - 1;
            if (locals[j].is_captured) {
                try self.write_instruction(.CloseUpvalue);
            } else {
                try self.write_instruction(.Pop);
            }
        }
        // self.write_instruction(.{ .PopN = num }) catch unreachable;
    }

    const LoopInfo = struct {
        loop_start: usize,
        skip: usize,
    };

    pub fn compile_stmt(self: *Self, stmt: *Stmt, loup: ?LoopInfo) !void {
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
                    try self.write_instruction(.{ .DefineGlobal = try self.new_str_const(val.name) });
                } else {
                    try self.locals.define(val.name);
                    // variable is implicitly 'set'
                }
            },
            .Assign => |val| {
                try self.compile_expr(val.expr);

                if (self.locals.resolve(val.name)) |s| {
                    try self.write_instruction(.{ .SetLocal = s });
                } else if (try self.resolve_upvalue(val.name)) |s| {
                    try self.write_instruction(.{ .SetUpvalue = s });
                } else {
                    try self.write_instruction(.{ .SetGlobal = try self.new_str_const(val.name) });
                }
            },
            .Block => |val| {
                self.begin_scope();

                for (val) |s| {
                    try self.compile_stmt(s, loup);
                }

                try self.end_scope();
            },
            .If => |val| {
                try self.compile_expr(val.condition);

                const if_jmp = self.chunk.code.items.len;
                try self.write_instruction(.{ .JmpIfFalse = 0 });

                try self.write_instruction(.Pop);
                try self.compile_stmt(val.if_block, loup);

                const else_jmp = self.chunk.code.items.len;
                try self.write_instruction(.{ .Jmp = 0 });

                try self.patch_jmp(.JmpIfFalse, if_jmp);

                try self.write_instruction(.Pop);
                if (val.else_block) |els| {
                    try self.compile_stmt(els, loup);
                }

                try self.patch_jmp(.Jmp, else_jmp);
            },
            .While => |val| {
                const loop_start = self.chunk.code.items.len;
                try self.compile_expr(val.condition);

                const skip = self.chunk.code.items.len;
                try self.write_instruction(.{ .JmpIfFalse = 0 });

                try self.write_instruction(.Pop);
                try self.compile_stmt(val.block, .{ .loop_start = loop_start, .skip = skip });

                try self.loop_to(loop_start);

                try self.patch_jmp(.JmpIfFalse, skip);
                try self.write_instruction(.Pop);
            },
            .For => |val| {
                self.begin_scope();

                if (val.start) |start| {
                    try self.compile_stmt(start, null);
                }
                const loop_start = self.chunk.code.items.len;

                if (val.mid) |mid| {
                    try self.compile_expr(mid);
                } else {
                    try self.write_instruction(.ConstTrue);
                }
                var end_jmp = self.chunk.code.items.len;
                try self.write_instruction(.{ .JmpIfFalse = 0 });
                try self.write_instruction(.Pop);

                try self.compile_stmt(val.block, .{ .loop_start = loop_start, .skip = end_jmp });
                if (val.end) |end| {
                    try self.compile_stmt(end, null);
                }

                try self.loop_to(loop_start);

                try self.patch_jmp(.JmpIfFalse, end_jmp);
                try self.write_instruction(.Pop);

                try self.end_scope();
            },
            .Function => |val| {
                try self.define_function(val, .Function);

                if (self.locals.curr_scope == 0) {
                    try self.write_instruction(.{ .DefineGlobal = try self.new_str_const(val.name) });
                } else {
                    try self.locals.define(val.name);
                }
            },
            .Return => |val| {
                try self.emit_return(val.val);
            },
            // DONE: break and continue statements
            // -[.] loops already have a condition which can end the loop by jumps
            //  - pass the address of this jump and simply jump to this instruction on breaks.
            //  - similar strategy for continue statements. just jump to the instruction where the condition stuff starts.
            //  - will need to pass these 2 addresses in this function as arguments.
            // -[x] maybe this function can return some value that encodes where the break / continue instruction was written
            //   and we can check that value in the immediately enclosing loop and act on it accordingly (like editing the
            //   instruction to jump out of the loop)
            //   - will need a list or something to keep track of all the continue and breaks. which is fine as it is only
            //     needed during compile time.
            .Continue => {
                var loop = loup orelse return error.NotInsideLoop;
                try self.loop_to(loop.loop_start);
            },
            .Break => {
                var loop = loup orelse return error.NotInsideLoop;
                try self.write_instruction(.ConstFalse);
                try self.loop_to(loop.skip);
            },
            .Class => |val| {
                const class = code_mod.Class.new(.{ .name = try self.new_string(val.name) });

                const c = try self.write_constant(try class.to_val(self.zalloc));
                try self.write_instruction(.{ .Constant = c });

                if (self.locals.curr_scope == 0) {
                    try self.write_instruction(.{ .DefineGlobal = try self.new_str_const(val.name) });
                } else {
                    try self.locals.define(val.name);
                }

                if (val.super) |super| {
                    if (std.mem.eql(u8, super, val.name)) {
                        return error.CannotInheritFromSelf;
                    } else if (self.locals.resolve(super)) |s| {
                        try self.write_instruction(.{ .GetLocal = s });
                    } else if (try self.resolve_upvalue(super)) |s| {
                        try self.write_instruction(.{ .GetUpvalue = s });
                    } else {
                        try self.write_instruction(.{ .GetGlobal = try self.new_str_const(super) });
                    }
                    try self.write_instruction(.{ .Constant = c });
                    try self.write_instruction(.Inherit);

                    self.begin_scope();
                    try self.locals.define("super");
                }

                try self.write_instruction(.{ .Constant = c });
                for (val.methods) |method| {
                    if (std.mem.eql(u8, method.name, "init")) {
                        try self.define_function(method, .Initializer);
                    } else {
                        try self.define_function(method, .Method);
                    }
                    try self.write_instruction(.{ .DefineMethod = try self.new_str_const(method.name) });
                }
                try self.write_instruction(.Pop);

                if (val.super) |_| {
                    try self.end_scope();
                }
            },
            .Set => |val| {
                try self.compile_expr(val.object);
                try self.compile_expr(val.value);
                try self.write_instruction(.{ .SetProperty = try self.new_str_const(val.name) });
            },
        }
    }

    pub fn compile_expr(self: *Self, expr: *Expr) !void {
        switch (expr.*) {
            .Literal => |val| {
                switch (val) {
                    .Number => |str| {
                        const num = try std.fmt.parseFloat(f64, str);
                        const index = try self.write_constant(.{ .Number = num });
                        try self.write_instruction(.{ .Constant = index });
                    },
                    .None => try self.write_instruction(.ConstNone),
                    .True => try self.write_instruction(.ConstTrue),
                    .False => try self.write_instruction(.ConstFalse),
                    .String => |str| {
                        try self.write_instruction(.{ .Constant = try self.new_str_const(str) });
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
            .Variable => |name| {
                if (self.locals.resolve(name)) |s| {
                    try self.write_instruction(.{ .GetLocal = s });
                } else if (try self.resolve_upvalue(name)) |s| {
                    try self.write_instruction(.{ .GetUpvalue = s });
                } else {
                    try self.write_instruction(.{ .GetGlobal = try self.new_str_const(name) });
                }
            },
            .Call => |val| {
                try self.compile_expr(val.callee);

                for (val.args) |arg| {
                    try self.compile_expr(arg);
                }

                try self.write_instruction(.{ .Call = @intCast(u8, val.args.len) });
            },
            .Get => |val| {
                try self.compile_expr(val.object);

                try self.write_instruction(.{ .GetProperty = try self.new_str_const(val.name) });
            },
            .Self => {
                if (self.locals.resolve("self")) |s| {
                    try self.write_instruction(.{ .GetLocal = s });
                } else if (try self.resolve_upvalue("self")) |s| {
                    try self.write_instruction(.{ .GetUpvalue = s });
                } else {
                    return error.UndefinedVariable;
                }
            },
            .Super => |val| {
                if (self.locals.resolve("self")) |s| {
                    try self.write_instruction(.{ .GetLocal = s });
                } else if (try self.resolve_upvalue("self")) |s| {
                    try self.write_instruction(.{ .GetUpvalue = s });
                } else {
                    return error.UndefinedVariable;
                }

                if (try self.resolve_upvalue("super")) |s| {
                    try self.write_instruction(.{ .GetUpvalue = s });
                } else {
                    return error.UndefinedVariable;
                }
                try self.write_instruction(.{ .GetSuper = try self.new_str_const(val.method) });
            },
        }
    }

    fn new_string(self: *Self, str: []const u8) ![]const u8 {
        var s = try self.zalloc.alloc(u8, str.len);
        std.mem.copy(u8, s, str);
        return s;
    }

    fn new_str_const(self: *Self, str: []const u8) !Instruction.ConstantRef {
        var s = try self.new_string(str);
        var o = code_mod.String.new(.{ .str = s });
        return self.write_constant(try o.to_val(self.zalloc));
    }
};
