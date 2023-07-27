const std = @import("std");

const build_options = @import("build_options");
const trace_enabled = build_options.trace_enable;

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Disassembler = code_mod.Disassembler;
const Value = code_mod.Value;
const Function = code_mod.Function;
const ChunkReader = code_mod.ChunkReader;

pub const Vm = struct {
    const Self = @This();
    pub const Error = error{
        StackOverflow,
        StackUnderflow,
        UndefinedVariable,
        PeekErr,
        NotCallable,
        IncorrectNumArgs,
    };
    pub const Result = enum {
        Ok,
    };

    const GlobalValues = std.StringHashMap(Value);
    const CallFrame = struct {
        reader: ChunkReader,
        stack_top: usize,
    };

    frames: []CallFrame,
    frame_top: usize = 0,
    stack: []Value,
    stack_top: usize = 0,

    // TODO: very inefficient to do a hashmap lookup for every variable.
    // can do some kind of intermediate refrence thing at compile time maybe
    // at compile time - have a monotonic increasing counter that increases each time
    // a new string or an identifier is encountered.
    // create a new type of Value that just stores this value (effectively refrencing this string)
    // this would also Intern the string as only one copy of it would be required.
    //
    // this table can be turned into (u32 -> Value) if the above is implemented
    // (the vounter value is for each unique string and not each unique variable)
    globals: GlobalValues,

    alloc: std.mem.Allocator,

    // TODO: “direct threaded code”, “jump table”, and “computed goto”

    pub fn new(alloc: std.mem.Allocator) !Self {
        var stack = try alloc.alloc(Value, 256 * 64);
        var globals = GlobalValues.init(alloc);
        var frames = try alloc.alloc(CallFrame, 64);
        return .{ .stack = stack, .globals = globals, .frames = frames, .alloc = alloc };
    }

    pub fn deinit(self: *Self) void {
        for (self.stack) |*val| {
            val.deinit(self.alloc);
        }
        self.alloc.free(self.stack);
        self.globals.deinit();
    }

    fn push_callframe(self: *Self, func: *Function, args: u8) !*CallFrame {
        if (self.frame_top >= self.frames.len) {
            return error.StackOverflow;
        }

        var callf = .{
            .reader = func.inner.chunk.reader(),
            .stack_top = self.stack_top - args,
        };
        self.frames[self.frame_top] = callf;
        self.frame_top += 1;

        return &self.frames[self.frame_top - 1];
    }

    fn pop_callframe(self: *Self) !?*CallFrame {
        if (self.frame_top < 1) {
            return error.StackUnderflow;
        }
        self.frame_top -= 1;
        self.stack_top = self.frames[self.frame_top].stack_top - 1;
        if (self.frame_top == 0) {
            return null;
        }
        return &self.frames[self.frame_top - 1];
    }

    fn stacktrace(self: *Self) void {
        for (self.frames) |_| {
            // std.debug.print("fn {s}\n", .{frame.reader.})
        }
    }

    pub fn start_script(self: *Self, func: *Function) !Result {
        try self.push_value(null);
        return self.run(func);
    }

    pub fn run(self: *Self, func: *Function) !Result {
        var frame = try self.push_callframe(func, 0);
        errdefer self.stacktrace();

        while (frame.reader.has_next()) {
            const start = frame.reader.curr;
            const inst = try frame.reader.next_instruction();

            if (trace_enabled) {
                std.debug.print("{any}\n", .{self.stack[0..self.stack_top]});
                var dis = Disassembler.new(frame.reader.chunk);
                try dis.disassemble_instruction(inst, start);
            }

            switch (inst) {
                .Return => {
                    var result = try self.pop_value();
                    frame = try self.pop_callframe() orelse return .Ok;

                    try self.push_value(result);
                },
                .Call => |args| {
                    var callee = self.stack[self.stack_top - 1 - args];
                    var cal = callee.as_obj(.Function) catch return error.NotCallable;
                    if (cal.inner.arity != args) {
                        return error.IncorrectNumArgs;
                    }
                    frame = try self.push_callframe(cal, args);
                },
                .Pop => {
                    _ = try self.pop_value();
                },
                .PopN => |count| {
                    var i = count;
                    while (i != 0) : (i -= 1) {
                        _ = try self.pop_value();
                    }
                },
                .DefineGlobal => |c| {
                    var val = frame.reader.chunk.consts.items[c];
                    var str = try val.as_obj(.String);
                    try self.globals.put(str.inner.str, try self.pop_value());
                },
                .GetGlobal => |c| {
                    var name_val = frame.reader.chunk.consts.items[c];
                    var name = try name_val.as_obj(.String);
                    var val = self.globals.get(name.inner.str) orelse return error.UndefinedVariable;
                    try self.push_value(val);
                },
                .SetGlobal => |c| {
                    var name_val = frame.reader.chunk.consts.items[c];
                    var name = try name_val.as_obj(.String);
                    var val = self.globals.getPtr(name.inner.str) orelse return error.UndefinedVariable;
                    val.* = try self.pop_value();
                },
                .GetLocal => |i| try self.push_value(self.stack[frame.stack_top + i - 1]),
                .SetLocal => |i| self.stack[frame.stack_top + i - 1] = try self.pop_value(),
                .Print => {
                    var val = try self.pop_value();
                    const print = std.debug.print;
                    try val.print();
                    print("\n", .{});
                },
                .Constant => |index| {
                    var val = frame.reader.chunk.consts.items[index];
                    try self.push_value(val);
                },
                .ConstNone => try self.push_value(null),
                .ConstTrue => try self.push_value(true),
                .ConstFalse => try self.push_value(false),
                .Negate => {
                    var val = try self.pop_value();
                    const num = try val.as(.Number);
                    try self.push_value(-num);
                },
                .LogicalNot => {
                    var val = try self.pop_value();
                    const b = try val.as(.Bool);
                    try self.push_value(!b);
                },
                .Equal => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(try v2.eq(&v1));
                },
                .GreaterThan => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(try v2.gt(&v1));
                },
                .LessThan => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(try v2.lt(&v1));
                },
                .Add => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    const num = try v2.as(.Number) + try v1.as(.Number);
                    try self.push_value(num);
                },
                .Subtract => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    const num = try v2.as(.Number) - try v1.as(.Number);
                    try self.push_value(num);
                },
                .Multiply => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    const num = try v2.as(.Number) * try v1.as(.Number);
                    try self.push_value(num);
                },
                .Divide => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    const num = try v2.as(.Number) / try v1.as(.Number);
                    try self.push_value(num);
                },
                .JmpIfFalse => |offset| {
                    var condition = try self.peek_value();
                    if (!try condition.as(.Bool)) {
                        frame.reader.curr += offset;
                    }
                },
                .JmpIfTrue => |offset| {
                    var condition = try self.peek_value();
                    if (try condition.as(.Bool)) {
                        frame.reader.curr += offset;
                    }
                },
                .Jmp => |offset| {
                    frame.reader.curr += offset;
                },
                .Loop => |offset| {
                    frame.reader.curr -= offset;
                },
            }
        }

        return .Ok;
    }

    fn push_value(self: *Self, val: anytype) !void {
        if (self.stack.len > self.stack_top) {
            self.stack[self.stack_top] = Value.new(val);
            self.stack_top += 1;
        } else {
            return error.StackOverflow;
        }
    }

    fn pop_value(self: *Self) !Value {
        if (self.stack_top > 0) {
            self.stack_top -= 1;
            return self.stack[self.stack_top];
        } else {
            return error.StackUnderflow;
        }
    }

    fn peek_value(self: *Self) !Value {
        if (self.stack_top == 0) {
            return error.PeekErr;
        } else {
            return self.stack[self.stack_top - 1];
        }
    }
};
