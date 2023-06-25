const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();
    var chunk = Chunk.new(alloc);
    defer chunk.deinit();

    try chunk.write(.Return);
    try chunk.write(.{ .Call = 255 });
    std.debug.print("{any}\n", .{chunk.code.items});

    var dis = Disassembler.new(chunk.reader(), "first test code");
    try dis.disassemble();
}

const Disassembler = struct {
    const Self = @This();

    chunk: ChunkReader,
    name: []const u8,

    pub fn new(chunk: ChunkReader, name: []const u8) Self {
        return .{
            .chunk = chunk,
            .name = name,
        };
    }

    pub fn disassemble(self: *Self) !void {
        const print = std.debug.print;
        print("----- {s} -----\n", .{self.name});

        var inst: Instruction = undefined;
        while (self.chunk.has_next()) {
            print("{X:2} ", .{self.chunk.curr});

            inst = self.chunk.next_instruction() catch |err| {
                print("Bad Opcode: {X:2}\n", .{try self.chunk.next_byte()});
                return err;
            };

            inline for (std.meta.tags(Opcode)) |tag, i| {
                if (tag == inst) {
                    const name = comptime std.meta.fieldNames(Opcode)[i];
                    const payload = comptime @field(inst, name);
                    const payload_size = comptime @sizeOf(@TypeOf(payload));

                    print(name ++ " ", .{});

                    inline for ([_]u8{0} ** payload_size) |_, j| {
                        const bytes = std.mem.asBytes(&payload);
                        const byte = bytes[j];
                        print("{X:2}", .{byte});
                    }
                }
            }

            // switch (inst) {
            //     .Return => print("RETURN", .{}),
            //     .Call => |b| print("CALL {X:2}", .{b}),
            //     else => unreachable,
            // }

            print("\n", .{});
        }
    }
};

const Error = error{
    NoBytes,
    BadOpcode,
};

const Chunk = struct {
    const Self = @This();
    const ByteList = std.ArrayListUnmanaged(u8);

    alloc: std.mem.Allocator,
    code: ByteList,

    fn new(alloc: std.mem.Allocator) Self {
        return .{ .alloc = alloc, .code = ByteList{} };
    }

    fn deinit(self: *Self) void {
        self.code.deinit(self.alloc);
    }

    fn reader(self: *Self) ChunkReader {
        return ChunkReader.new(self.code.items);
    }

    fn write(self: *Self, inst: Instruction) !void {
        const opcode = @enumToInt(inst);
        try self.code.append(self.alloc, opcode);

        inline for (std.meta.tags(Opcode)) |tag, i| {
            if (tag == inst) {
                const name = comptime std.meta.fieldNames(Opcode)[i];
                const payload = comptime @field(inst, name);
                const payload_size = comptime @sizeOf(@TypeOf(payload));
                // std.meta.activeTag()
                inline for ([_]u8{0} ** payload_size) |_, j| {
                    const bytes = std.mem.asBytes(&payload);
                    const byte = bytes[j];
                    try self.code.append(self.alloc, byte);
                }
            }
        }
    }
};

const ChunkReader = struct {
    const Self = @This();

    code: []u8,
    curr: usize,

    fn new(code: []u8) Self {
        return .{
            .code = code,
            .curr = 0,
        };
    }

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
    Constant: f64,
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
