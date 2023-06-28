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
    Equal,
    GreaterThan,
    LessThan,
};

pub const Opcode = std.meta.Tag(Instruction);

// all this stuff allows all objects to have their own size in heap
// an union consumes the max of it's possible varients always. but in this case - it does not need to
// for example:
// assume a type S which takes 50 bytes and another type T which takes 8 bytes
// each of these types can be allocated on the heap with their size.
// then - the pointers to Object can be stored anywhere in the program (which is a field of both S and T)
// and we can cast *Object to *S or *T as required
pub const Object = struct {
    const Self = @This();
    pub const Error = error{
        BadObjectCast,
    };
    pub const Tag = enum(u8) {
        String,
    };

    tag: Tag,

    fn tag_to_type(comptime tag: Tag) type {
        return switch (tag) {
            .String => String,
        };
    }

    pub fn as(self: *Self, comptime tag: Tag) !*tag_to_type(tag) {
        if (self.tag != tag) {
            return error.BadObjectCast;
        } else {
            return @fieldParentPtr(tag_to_type(tag), "tag", self);
        }
    }

    pub fn is(self: *Self, tag: Tag) bool {
        return self.tag == tag;
    }

    pub fn eq(self: *Self, other: *Self) !bool {
        if (other.is(self.tag)) {
            inline for (.{.String}) |t| {
                if (self.is(t)) {
                    const a = self.as(t) catch unreachable;
                    const b = other.as(t) catch unreachable;
                    return a.inner.eq(&b.inner);
                }
            }
            unreachable;
        } else {
            return error.CannotCompareValues;
        }
    }

    pub fn gt(self: *Self, other: *Self) !bool {
        if (other.is(self.tag)) {
            inline for (.{.String}) |t| {
                if (self.is(t)) {
                    const a = self.as(t) catch unreachable;
                    const b = other.as(t) catch unreachable;
                    return a.inner.gt(&b.inner);
                }
            }
            unreachable;
        } else {
            return error.CannotCompareValues;
        }
    }

    pub fn lt(self: *Self, other: *Self) !bool {
        if (other.is(self.tag)) {
            inline for (.{.String}) |t| {
                if (self.is(t)) {
                    const a = self.as(t) catch unreachable;
                    const b = other.as(t) catch unreachable;
                    return a.inner.lt(&b.inner);
                }
            }
            unreachable;
        } else {
            return error.CannotCompareValues;
        }
    }
};

// this allows the new object declarations to always comply to the assumptions made in methods in Object
// and always initialise with the correct stuff
// this also allows to define methods on all custom objects at once (don't know how useful that will be :P)
pub fn new_object(comptime tag: Object.Tag, comptime val: type) type {
    return struct {
        const Self = @This();
        pub const Inner = val;

        tag: Object = .{ .tag = tag },
        inner: Inner,

        pub fn new(inner: Inner) Self {
            return .{ .inner = inner };
        }
    };
}

pub const String = new_object(.String, struct {
    const Self = @This();

    str: []const u8,

    fn eq(self: *Self, other: *Self) bool {
        return std.mem.eql(u8, self.str, other.str);
    }

    fn gt(self: *Self, other: *Self) bool {
        const len = std.math.min(self.str.len, other.str.len);

        for (self.str[0..len]) |c, i| {
            if (c > other.str[i]) {
                return true;
            } else if (c < other.str[i]) {
                return false;
            }
        }

        return self.str.len > other.str.len;
    }

    fn lt(self: *Self, other: *Self) bool {
        const len = std.math.min(self.str.len, other.str.len);

        for (self.str[0..len]) |c, i| {
            if (c > other.str[i]) {
                return false;
            } else if (c < other.str[i]) {
                return true;
            }
        }

        return self.str.len < other.str.len;
    }
});

test "object tests" {
    const dbg = std.debug.print;
    const alloc = std.testing.allocator_instance.allocator();
    const p = alloc;
    _ = p;

    var obj: Object = .{ .tag = .String };
    var lol = try obj.as(.String);
    lol = try alloc.create(String);
    defer alloc.destroy(lol);
    lol.* = String.new(.{ .str = "adsas" });

    var o = String.new(.{ .str = "adsas" });
    var o2 = &o.tag;
    var o1 = &lol.tag;

    dbg("{any}\n", .{@sizeOf(String)});
    dbg("{any}\n", .{Object.Tag});
    dbg("{*}\n", .{try obj.as(.String)});
    dbg("{any}\n", .{lol});
    dbg("{any}\n", .{o1.eq(o2)});
}

pub const Value = union(enum) {
    const Self = @This();
    pub const Error = error{
        NotObject,
        NotNumber,
        NotBoolean,
        NotNone,
        CannotCompareValues,
    };
    pub const Tag = std.meta.Tag(Self);

    Object: *Object,
    Number: f64,
    Bool: bool,
    None,

    pub fn new(val: anytype) Self {
        const t = comptime @TypeOf(val);
        return switch (t) {
            *Object => .{ .Object = val },
            f64 => .{ .Number = val },
            bool => .{ .Bool = val },
            @TypeOf(null) => .None,
            Self => return val,
            else => {
                @compileLog("type passed => ", t);
                @compileError("cannot represent as a Value!");
            },
        };
    }

    pub fn is(self: *const Self, typ: Self.Tag) bool {
        return typ == self.*;
    }

    pub fn as(self: *const Self, comptime typ: Self.Tag) !switch (typ) {
        .Object => *Object,
        .Number => f64,
        .Bool => bool,
        .None => void,
    } {
        if (self.is(typ)) {
            return switch (typ) {
                .Object => self.Object,
                .Number => self.Number,
                .Bool => self.Bool,
                .None => {},
            };
        } else {
            return switch (typ) {
                .Object => error.NotObject,
                .Number => error.NotNumber,
                .Bool => error.NotBoolean,
                .None => error.NotNone,
            };
        }
    }

    pub fn tag(self: *const Self) Self.Tag {
        return std.meta.activeTag(self.*);
    }

    pub fn eq(self: *const Self, other: Self) !bool {
        if (self.tag() != other) {
            return false;
        }

        switch (self.*) {
            .Object => |val| return try val.eq(try other.as(.Object)),
            .Number => |val| return val == try other.as(.Number),
            .Bool => |val| return val == try other.as(.Bool),
            .None => return true,
        }
    }

    pub fn gt(self: *const Self, other: Self) !bool {
        if (self.tag() != other) {
            return error.CannotCompareValues;
        }

        switch (self.*) {
            .Object => |val| return try val.gt(try other.as(.Object)),
            .Number => |val| return val > try other.as(.Number),
            .Bool => return error.CannotCompareValues,
            .None => return error.CannotCompareValues,
        }
    }

    pub fn lt(self: *const Self, other: Self) !bool {
        if (self.tag() != other) {
            return error.CannotCompareValues;
        }

        switch (self.*) {
            .Object => |val| return try val.lt(try other.as(.Object)),
            .Number => |val| return val < try other.as(.Number),
            .Bool => return error.CannotCompareValues,
            .None => return error.CannotCompareValues,
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
