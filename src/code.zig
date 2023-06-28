const std = @import("std");

pub const Instruction = union(enum) {
    const Self = @This();

    Return,
    Call: u8,
    Constant: u8,
    ConstNone,
    ConstTrue,
    ConstFalse,
    Negate,
    LogicalNot,
    Add,
    Subtract,
    Multiply,
    Divide,
};

pub const Opcode = std.meta.Tag(Instruction);

pub const Value = union(enum) {
    const Self = @This();
    pub const Error = error{
        NotNumber,
        NotBoolean,
        NotNone,
    };
    pub const Type = std.meta.Tag(Self);

    Number: f64,
    Bool: bool,
    None,

    pub fn new(val: anytype) Self {
        const t = comptime @TypeOf(val);
        return switch (t) {
            f64 => .{ .Number = val },
            bool => .{ .Bool = val },
            @TypeOf(null) => .None,
            else => {
                @compileLog("type passed => ", t);
                @compileError("cannot represent as a Value!");
            },
        };
    }

    pub fn is(self: *Self, typ: Self.Type) bool {
        return typ == self.*;
    }

    pub fn as(self: *Self, comptime typ: Self.Type) !switch (typ) {
        .Number => f64,
        .Bool => bool,
        .None => void,
    } {
        if (self.is(typ)) {
            return switch (typ) {
                .Number => self.Number,
                .Bool => self.Bool,
                .None => {},
            };
        } else {
            return switch (typ) {
                .Number => error.NotNumber,
                .Bool => error.NotBoolean,
                .None => error.NotNone,
            };
        }
    }
};

pub const Chunk = struct {
    const Self = @This();
    pub const Error = error{
        BadByteOffset,
    };

    const ByteList = std.ArrayListUnmanaged(u8);
    const ConstantList = std.ArrayListUnmanaged(Value);
    const LineInfo = struct {
        line: usize,
        bytes: usize,
    };
    const LineNoList = std.ArrayListUnmanaged(LineInfo);

    alloc: std.mem.Allocator,
    code: ByteList,
    consts: ConstantList,

    line_nos: LineNoList,

    pub fn new(alloc: std.mem.Allocator) Self {
        return .{ .alloc = alloc, .code = ByteList{}, .consts = ConstantList{}, .line_nos = LineNoList{} };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit(self.alloc);
        self.consts.deinit(self.alloc);
        self.line_nos.deinit(self.alloc);
    }

    pub fn reader(self: *Self) ChunkReader {
        return .{
            .code = self.code.items,
            .curr = 0,
        };
    }

    pub fn write_constant(self: *Self, constant: Value) !u8 {
        try self.consts.append(self.alloc, constant);
        return @intCast(u8, self.consts.items.len - 1);
    }

    pub fn write_instruction(self: *Self, inst: Instruction, line: usize) !void {
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

    pub fn get_line_number(self: *Self, byte_offset: usize) !usize {
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

pub const ChunkReader = struct {
    const Self = @This();
    pub const Error = error{
        NoBytes,
        BadOpcode,
    };

    code: []u8,
    curr: usize,

    pub fn has_next(self: *Self) bool {
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

    pub fn next_instruction(self: *Self) !Instruction {
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

pub const Disassembler = struct {
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
