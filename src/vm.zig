const std = @import("std");

const build_options = @import("build_options");
const trace_enabled = build_options.trace_enable;

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Disassembler = code_mod.Disassembler;
const Value = code_mod.Value;

pub const Vm = struct {
    const Self = @This();
    pub const Error = error{
        StackOverflow,
        StackUnderflow,
        UndefinedVariable,
        PeekErr,
    };
    pub const Result = enum {
        Ok,
    };

    const GlobalValues = std.StringHashMap(Value);

    chunk: *Chunk,
    dis: Disassembler,
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

    // TODO: (each byte in Chunk is accessed using indexing instead of simple pointer derefs and pointer arithmatics)
    // it might be faster to deref a pointer than indexing an array.
    // but it is easier to modify the chunk curr position.
    // maybe check the speed difference and implement it using pointers instead

    // TODO: “direct threaded code”, “jump table”, and “computed goto”

    pub fn new(chunk: *Chunk) !Self {
        var stack = try chunk.alloc.alloc(Value, 256);
        var globals = GlobalValues.init(chunk.alloc);
        var dis = Disassembler.new(chunk);
        return .{ .chunk = chunk, .stack = stack, .dis = dis, .globals = globals };
    }

    pub fn deinit(self: *Self) void {
        self.chunk.alloc.free(self.stack);
        self.globals.deinit();
    }

    pub fn run(self: *Self) !Result {
        var reader = self.chunk.reader();

        while (reader.has_next()) {
            const start = reader.curr;
            const inst = try reader.next_instruction();

            if (trace_enabled) {
                std.debug.print("{any}\n", .{self.stack[0..self.stack_top]});
                try self.dis.disassemble_instruction(inst, start);
            }

            switch (inst) {
                .Return, .Call => unreachable,
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
                    var val = self.chunk.consts.items[c];
                    var str = try val.as_obj(.String);
                    try self.globals.put(str.str, try self.pop_value());
                },
                .GetGlobal => |c| {
                    var name_val = self.chunk.consts.items[c];
                    var name = try name_val.as_obj(.String);
                    var val = self.globals.get(name.str) orelse return error.UndefinedVariable;
                    try self.push_value(val);
                },
                .SetGlobal => |c| {
                    var name_val = self.chunk.consts.items[c];
                    var name = try name_val.as_obj(.String);
                    var val = self.globals.getPtr(name.str) orelse return error.UndefinedVariable;
                    val.* = try self.pop_value();
                },
                .GetLocal => |i| try self.push_value(self.stack[i]),
                .SetLocal => |i| self.stack[i] = try self.pop_value(),
                .Print => {
                    var val = try self.pop_value();
                    const print = std.debug.print;
                    val.print();
                    print("\n", .{});
                },
                .Constant => |index| {
                    var val = self.chunk.consts.items[index];
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
                        reader.curr += offset;
                    }
                },
                .JmpIfTrue => |offset| {
                    var condition = try self.peek_value();
                    if (try condition.as(.Bool)) {
                        reader.curr += offset;
                    }
                },
                .Jmp => |offset| {
                    reader.curr += offset;
                },
                .Loop => |offset| {
                    reader.curr -= offset;
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
