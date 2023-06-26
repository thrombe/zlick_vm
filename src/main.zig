const std = @import("std");

const build_options = @import("build_options");
const trace_enabled = build_options.trace_enable;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();
    var chunk = Chunk.new(alloc);
    defer chunk.deinit();

    // try chunk.write_instruction(.{ .Call = 255 }, 1);
    const cnst = try chunk.write_constant(0.1212);
    try chunk.write_instruction(.{ .Constant = cnst }, 1);
    try chunk.write_instruction(.Negate, 0);
    try chunk.write_instruction(.Return, 0);

    std.debug.print("{any}\n", .{chunk.code.items});

    var vm = try Vm.new(&chunk);
    defer vm.deinit();

    _ = try vm.run();
}

const Vm = struct {
    const Self = @This();
    const Result = enum {
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

    fn new(chunk: *Chunk) !Self {
        var stack = try chunk.alloc.alloc(f64, 256);
        return .{ .chunk = chunk, .stack = stack, .stack_top = 0, .dis = Disassembler.new(chunk) };
    }

    fn deinit(self: *Self) void {
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

const Disassembler = struct {
    const Self = @This();

    chunk: *Chunk,

    pub fn new(chunk: *Chunk) Self {
        return .{
            .chunk = chunk,
        };
    }

    pub fn disassemble_chunk(self: *Self, name: []const u8) !void {
        var reader = self.chunk.reader();

        const print = std.debug.print;
        print("----- {s} -----\n", .{name});
        print("line no. | byte no. | opcode | args \n\n", .{});

        var inst: Instruction = undefined;
        while (reader.has_next()) {
            var start = reader.curr;

            inst = reader.next_instruction() catch |err| {
                print("Bad Opcode: {X:2}\n", .{try reader.next_byte()});
                return err;
            };

            try self.disassemble_instruction(inst, start);
        }
    }

    pub fn disassemble_instruction(self: *Self, inst: Instruction, start: usize) !void {
        const print = std.debug.print;

        print("{:4} ", .{try self.chunk.get_line_number(start)});
        print("{:5} ", .{start});

        inline for (std.meta.tags(Opcode)) |tag, i| {
            if (tag == inst) {
                const name = comptime std.meta.fieldNames(Opcode)[i];
                const payload = comptime @field(inst, name);
                const payload_size = comptime @sizeOf(@TypeOf(payload));

                print("{s:<10} ", .{name});

                inline for ([_]u8{0} ** payload_size) |_, j| {
                    const bytes = std.mem.asBytes(&payload);
                    const byte = bytes[j];
                    print("{X:0>2} ", .{byte});
                }

                if (payload_size > 0) {
                    print("({}) ", .{payload});

                    if (.Constant == tag) {
                        const val = self.chunk.consts.items[payload];
                        print("[{}]", .{val});
                    }
                }
            }
        }

        print("\n", .{});
    }
};

const Error = error{
    NoBytes,
    BadOpcode,
    BadByteOffset,
    StackOverflow,
    StackUnderflow,
};

const Chunk = struct {
    const Self = @This();
    const ByteList = std.ArrayListUnmanaged(u8);
    const ConstantList = std.ArrayListUnmanaged(f64);
    const LineInfo = struct {
        line: usize,
        bytes: usize,
    };
    const LineNoList = std.ArrayListUnmanaged(LineInfo);

    alloc: std.mem.Allocator,
    code: ByteList,
    consts: ConstantList,

    line_nos: LineNoList,

    fn new(alloc: std.mem.Allocator) Self {
        return .{ .alloc = alloc, .code = ByteList{}, .consts = ConstantList{}, .line_nos = LineNoList{} };
    }

    fn deinit(self: *Self) void {
        self.code.deinit(self.alloc);
        self.consts.deinit(self.alloc);
        self.line_nos.deinit(self.alloc);
    }

    fn reader(self: *Self) ChunkReader {
        return .{
            .code = self.code.items,
            .curr = 0,
        };
    }

    fn write_constant(self: *Self, constant: f64) !u8 {
        try self.consts.append(self.alloc, constant);
        return @intCast(u8, self.consts.items.len - 1);
    }

    fn write_instruction(self: *Self, inst: Instruction, line: usize) !void {
        const opcode = @enumToInt(inst);
        try self.code.append(self.alloc, opcode);

        if (self.line_nos.items.len == 0) {
            try self.line_nos.append(self.alloc, .{ .line = line, .bytes = 0 });
        }

        var info: *LineInfo = &self.line_nos.items[self.line_nos.items.len - 1];
        if (info.line != line) {
            try self.line_nos.append(self.alloc, .{ .line = line, .bytes = 0 });
            info = &self.line_nos.items[self.line_nos.items.len - 1];
        }

        inline for (std.meta.tags(Opcode)) |tag, i| {
            if (tag == inst) {
                const name = comptime std.meta.fieldNames(Opcode)[i];
                const payload = comptime @field(inst, name);

                const payload_size = comptime @sizeOf(@TypeOf(payload));
                info.bytes += payload_size + 1;

                try self.code.appendSlice(self.alloc, std.mem.asBytes(&payload));
            }
        }
    }

    fn get_line_number(self: *Self, byte_offset: usize) !usize {
        var num = byte_offset;
        for (self.line_nos.items) |info| {
            if (num < info.bytes) {
                return info.line;
            } else {
                num -= info.bytes;
            }
        }

        return error.BadByteOffset;
    }
};

const ChunkReader = struct {
    const Self = @This();

    code: []u8,
    curr: usize,

    fn has_next(self: *Self) bool {
        return self.code.len > self.curr;
    }

    fn next_byte(self: *Self) !u8 {
        if (self.has_next()) {
            defer self.curr += 1;
            return self.code[self.curr];
        } else {
            return error.NoBytes;
        }
    }

    fn next_opcode(self: *Self) !Opcode {
        const byte = try self.next_byte();

        // don't consume if bad opcode
        errdefer self.curr -= 1;

        return std.meta.intToEnum(Opcode, byte) catch error.BadOpcode;
    }

    fn next_instruction(self: *Self) !Instruction {
        const opcode = try self.next_opcode();

        var inst: Instruction = undefined;

        inline for (std.meta.tags(Opcode)) |tag, i| {
            if (tag == opcode) {
                const name = comptime std.meta.fieldNames(Opcode)[i];
                const field_index = comptime std.meta.fieldIndex(Instruction, name).?;
                const field = comptime std.meta.fields(Instruction)[field_index];
                const payload_type = comptime field.field_type;
                const payload_size = comptime @sizeOf(payload_type);

                if (payload_size > 0) {
                    const start = self.code[self.curr..];

                    self.curr += payload_size;
                    if (self.code.len < self.curr) {
                        return error.NoBytes;
                    }

                    const val = std.mem.bytesToValue(payload_type, start[0..payload_size]);
                    inst = @unionInit(Instruction, name, val);
                } else {
                    inst = @unionInit(Instruction, name, {});
                }
            }
        }

        return inst;
    }
};

const Instruction = union(enum) {
    const Self = @This();

    Return,
    Call: u8,
    Constant: u8,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
};

const Opcode = std.meta.Tag(Instruction);

test "size test" {
    std.debug.assert(@sizeOf(Instruction) == 2);
}

test "random tests" {
    std.debug.assert(std.meta.intToEnum(Opcode, 2323) catch .Return == .Return);
    std.debug.assert(std.meta.intToEnum(Opcode, @enumToInt(Opcode.Call)) catch .Return == .Call);
}

test "packed unions" {
    const U = packed union {
        boo: bool,
        in: u32,
    };

    // oof. not what i thought. ofc
    std.debug.print("{}\n", .{@sizeOf(U)});
}
