const std = @import("std");

const build_options = @import("build_options");
const trace_enabled = build_options.trace_enable;

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Disassembler = code_mod.Disassembler;

pub const Vm = struct {
    const Self = @This();
    pub const Error = error{
        StackOverflow,
        StackUnderflow,
    };
    pub const Result = enum {
        Ok,
    };

    chunk: *Chunk,
    dis: Disassembler,
    stack: []f64,
    stack_top: usize,

    // TODO: it might be faster to deref a pointer than indexing an array.
    // but it is easier to modify the chunk curr position.
    // maybe check the speed difference and implement it using pointers instead

    // TODO: “direct threaded code”, “jump table”, and “computed goto”

    pub fn new(chunk: *Chunk) !Self {
        var stack = try chunk.alloc.alloc(f64, 256);
        return .{ .chunk = chunk, .stack = stack, .stack_top = 0, .dis = Disassembler.new(chunk) };
    }

    pub fn deinit(self: *Self) void {
        self.chunk.alloc.free(self.stack);
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
                .Return => {
                    std.debug.print("{}\n", .{try self.pop_value()});
                    return .Ok;
                },
                .Constant => |index| try self.push_value(self.chunk.consts.items[index]),
                .Negate => try self.push_value(-try self.pop_value()),
                .Add => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(v2 + v1);
                },
                .Subtract => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(v2 - v1);
                },
                .Multiply => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(v2 * v1);
                },
                .Divide => {
                    var v1 = try self.pop_value();
                    var v2 = try self.pop_value();
                    try self.push_value(v2 / v1);
                },
                .Call => {},
            }
        }

        return .Ok;
    }

    fn push_value(self: *Self, val: f64) !void {
        if (self.stack.len > self.stack_top) {
            self.stack[self.stack_top] = val;
            self.stack_top += 1;
        } else {
            return error.StackOverflow;
        }
    }

    fn pop_value(self: *Self) !f64 {
        if (self.stack_top > 0) {
            self.stack_top -= 1;
            return self.stack[self.stack_top];
        } else {
            return error.StackUnderflow;
        }
    }
};
