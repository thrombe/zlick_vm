const std = @import("std");

const compiler_mod = @import("compiler.zig");

const vm_mod = @import("vm.zig");
const Allocator = vm_mod.Allocator;

const code_mod = @This();

pub const Instruction = union(enum) {
    const Self = @This();
    pub const ConstantRef = u8;
    pub const ConstantCount = u8;
    pub const JmpOffset = u16;
    pub const UpvalueRef = compiler_mod.Compiler.UpvalueRef;
    pub const Closure = struct {
        upvalues: []UpvalueRef,
        func: ConstantRef,
    };

    Return,
    Call: ConstantCount,

    Constant: ConstantRef,
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

    Pop,
    PopN: ConstantCount,

    DefineGlobal: ConstantRef,
    GetGlobal: ConstantRef,
    SetGlobal: ConstantRef,
    GetLocal: ConstantRef,
    SetLocal: ConstantRef,

    Closure: Self.Closure,
    GetUpvalue: ConstantRef,
    SetUpvalue: ConstantRef,
    CloseUpvalue,

    SetProperty: ConstantRef,
    GetProperty: ConstantRef,

    JmpIfFalse: JmpOffset,
    JmpIfTrue: JmpOffset,
    Jmp: JmpOffset,
    Loop: JmpOffset,

    Print,

    pub fn size(opcode: Opcode) usize {
        @setEvalBranchQuota(2000);
        switch (opcode) {
            inline else => |tag| {
                const payload_type = comptime std.meta.TagPayload(Self, tag);
                const payload_size = comptime @sizeOf(payload_type);
                return payload_size + 1;
            },
        }
    }
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
        Function,
        Closure,
        Upvalue,
        NativeFunction,
        Class,
        Instance,
    };

    tag: Tag,

    gc_mark: bool = false,

    fn tag_to_type(comptime tag: Tag) type {
        return switch (tag) {
            inline else => |t| @field(code_mod, @tagName(t)),
        };
    }

    pub fn as(self: *Self, comptime tag: Tag) !*tag_to_type(tag) {
        if (self.tag != tag) {
            return error.BadObjectCast;
        } else {
            return @fieldParentPtr(tag_to_type(tag), "tag", self);
        }
    }

    pub fn try_as(self: *Self, comptime tag: Tag) ?*tag_to_type(tag) {
        if (self.tag != tag) {
            return null;
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
        }
        return error.CannotCompareValues;
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
        }
        return error.CannotCompareValues;
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
        }
        return error.CannotCompareValues;
    }

    pub fn print(self: *Self) !void {
        switch (self.tag) {
            inline else => |itag| self.try_as(itag).?.inner.print(),
        }
    }

    pub fn deinit(self: *Self, zalloc: *Allocator) void {
        switch (self.tag) {
            inline else => |itag| self.try_as(itag).?.inner.deinit(zalloc),
        }
    }
};

// this allows the new object declarations to always comply to the assumptions made in methods in Object
// and always initialise with the correct stuff
// this also allows to define methods on all custom objects at once (don't know how useful that will be :P)
// reasons: https://craftinginterpreters.com/strings.html#struct-inheritance
//  - allocations of different kinds of objects can take a different amount of memory
//    - a tagged union always takes the max of all types within it
//  - while a pointer to the tag field can be saved whereever which acts as a type erased object
//  - a type erased pointer with a tag can do the same thing, but then there is another level of indirection.
// OOF: here, it might also be feasable to use a tagged union of pointers
//  - the Value type already has a pointer and a tag, and the pointer can be replaced with a tagged union that points to objects
//  - it might use the same amount of memory as it might use the bits from the other tag in Value type to
//    represent the tag of Object.
//    - not too sure for Zig, but rust does this kind of optimisation for Option
//      (not sure if it does this for other types as well :P)
pub fn new_object(comptime tag: Object.Tag, comptime val: type) type {
    return struct {
        const Self = @This();
        pub const Inner = val;

        tag: Object = .{ .tag = tag },
        inner: Inner,

        pub fn new(inner: Inner) Self {
            return .{ .inner = inner };
        }

        pub fn as_val(self: *Self) Value {
            return .{ .Object = &self.tag };
        }

        pub fn to_val(self: Self, zalloc: *Allocator) !Value {
            var v = try zalloc.create(Self);
            v.* = self;
            var value = v.as_val();
            try zalloc.add_val(value);
            return value;
        }
    };
}

pub const String = new_object(.String, struct {
    const Self = @This();

    str: []const u8,

    fn deinit(self: *Self, zalloc: *Allocator) void {
        zalloc.free(self.str);
        zalloc.destroy(self);
    }

    fn eq(self: *const Self, other: *const Self) bool {
        return std.mem.eql(u8, self.str, other.str);
    }

    fn gt(self: *const Self, other: *const Self) bool {
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

    fn lt(self: *const Self, other: *const Self) bool {
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

    fn print(self: *const Self) void {
        std.debug.print("{s}", .{self.str});
    }
});

pub const Function = new_object(.Function, struct {
    const Self = @This();

    arity: u32,
    chunk: Chunk,
    name: []const u8,

    fn deinit(self: *Self, zalloc: *Allocator) void {
        self.chunk.deinit();
        zalloc.free(self.name);
        zalloc.destroy(self);
    }

    fn print(self: *const Self) void {
        std.debug.print("<fn {s}>", .{self.name});
    }
});

// TODO: "closure conversion" "lambda lifting"
pub const Closure = new_object(.Closure, struct {
    const Self = @This();

    func: *Function,
    upvalues: []*Upvalue,

    fn deinit(self: *Self, zalloc: *Allocator) void {
        // self.func.inner.deinit(alloc);
        zalloc.free(self.upvalues);
        zalloc.destroy(self);
    }

    fn print(self: *const Self) void {
        self.func.inner.print();
    }
});

pub const Upvalue = new_object(.Upvalue, struct {
    const Self = @This();

    val: *Value,
    // TODO: this can be a tagged union maybe. as val points to closed when the upvalue if closed, else
    // val points to some other value, and closed is useless
    closed: Value,
    next: ?*Upvalue,

    fn deinit(self: *Self, zalloc: *Allocator) void {
        // alloc.destroy(self.val);
        zalloc.destroy(self);
    }

    fn print(_: *const Self) void {
        std.debug.print("<upvalue>", .{});
    }
});

pub const NativeFunction = new_object(.NativeFunction, struct {
    const Self = @This();
    pub const TypeFunc = *const fn (args: []Value) anyerror!Value;

    arity: u32,
    name: []const u8,
    func: TypeFunc,

    fn deinit(self: *Self, zalloc: *Allocator) void {
        zalloc.destroy(self);
    }

    fn print(self: *const Self) void {
        std.debug.print("<native fn {s}>", .{self.name});
    }

    fn call(self: *Self, args: []Value) anyerror!Value {
        return try self.func(args);
    }
});

pub const Class = new_object(.Class, struct {
    const Self = @This();

    name: []const u8,

    fn deinit(self: *Self, zalloc: *Allocator) void {
        zalloc.free(self.name);
        zalloc.destroy(self);
    }

    fn print(self: *const Self) void {
        std.debug.print("<class {s}>", .{self.name});
    }
});

pub const Instance = new_object(.Instance, struct {
    const Self = @This();
    const Fields = std.StringHashMapUnmanaged(Value);

    class: *Class,
    // TODO: intern string identifiers at compile time and map them to ids. (hashmap with strings is slower)
    fields: Fields = .{},

    fn deinit(self: *Self, zalloc: *Allocator) void {
        var keys = self.fields.keyIterator();
        while (keys.next()) |k| {
            zalloc.free(k.*);
        }
        self.fields.deinit(zalloc.zalloc);
        zalloc.destroy(self);
    }

    fn print(self: *const Self) void {
        std.debug.print("<object of {s}>", .{self.class.inner.name});
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

test "defer or something" {
    const dbg = std.debug.print;

    var a: i64 = 0;
    // if (comptime true) {
    defer if (comptime true) {
        a = std.time.milliTimestamp();
        _ = std.fs.cwd();
        dbg("defer ran {}\n", .{a});
    };
    // }

    dbg("before defer {}\n", .{a});
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

    pub fn as(self: *Self, comptime typ: Self.Tag) !switch (typ) {
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

    pub fn as_obj(self: *Self, comptime t: Object.Tag) !*Object.tag_to_type(t) {
        var obj = try self.as(.Object);
        return try obj.as(t);
    }

    pub fn tag(self: *const Self) Self.Tag {
        return std.meta.activeTag(self.*);
    }

    pub fn eq(self: *Self, other: *Self) !bool {
        if (self.tag() != other.tag()) {
            return false;
        }

        switch (self.*) {
            .Object => |val| return try val.eq(try other.as(.Object)),
            .Number => |val| return val == try other.as(.Number),
            .Bool => |val| return val == try other.as(.Bool),
            .None => return true,
        }
    }

    pub fn gt(self: *Self, other: *Self) !bool {
        if (self.tag() != other.tag()) {
            return error.CannotCompareValues;
        }

        switch (self.*) {
            .Object => |val| return try val.gt(try other.as(.Object)),
            .Number => |val| return val > try other.as(.Number),
            .Bool => return error.CannotCompareValues,
            .None => return error.CannotCompareValues,
        }
    }

    pub fn lt(self: *Self, other: *Self) !bool {
        if (self.tag() != other.tag()) {
            return error.CannotCompareValues;
        }

        switch (self.*) {
            .Object => |val| return try val.lt(try other.as(.Object)),
            .Number => |val| return val < try other.as(.Number),
            .Bool => return error.CannotCompareValues,
            .None => return error.CannotCompareValues,
        }
    }

    pub fn print(self: *Self) !void {
        switch (self.*) {
            .Object => |obj| try obj.print(),
            .Number => |num| std.debug.print("{}", .{num}),
            .Bool => |b| std.debug.print("{}", .{b}),
            .None => std.debug.print("None", .{}),
        }
    }

    pub fn deinit(self: *Self, zalloc: *Allocator) void {
        switch (self.*) {
            .Object => |obj| obj.deinit(zalloc),
            else => {},
        }
    }
};

pub const Chunk = struct {
    const Self = @This();
    pub const Error = error{
        BadByteOffset,
        CodeOverflow,
    };

    const ByteList = std.ArrayListUnmanaged(u8);
    const ConstantList = std.ArrayListUnmanaged(Value);
    const LineInfo = struct {
        line: usize,
        bytes: usize,
    };
    const LineNoList = std.ArrayListUnmanaged(LineInfo);

    code: ByteList,
    consts: ConstantList,
    alloc: std.mem.Allocator,

    line_nos: LineNoList,

    pub fn new(alloc: std.mem.Allocator) Self {
        return .{
            .code = ByteList{},
            .consts = ConstantList{},
            .line_nos = LineNoList{},
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit(self.alloc);
        // for (self.consts.items) |*item| {
        //     item.deinit(alloc);
        // }
        self.consts.deinit(self.alloc);
        self.line_nos.deinit(self.alloc);
    }

    pub fn reader(self: *Self) ChunkReader {
        return .{
            .chunk = self,
            .curr = 0,
        };
    }

    pub fn write_constant(self: *Self, constant: Value, alloc: std.mem.Allocator) !u8 {
        try self.consts.append(alloc, constant);
        return @intCast(u8, self.consts.items.len - 1);
    }

    pub fn edit_instruction(self: *Self, inst: Instruction, pos: usize) !void {
        const opcode = @enumToInt(inst);

        if (pos + Instruction.size(std.meta.activeTag(inst)) > self.code.items.len - 1) {
            return error.CodeOverflow;
        }
        self.code.items[pos] = opcode;

        switch (inst) {
            inline else => |payload| {
                for (std.mem.asBytes(&payload)) |byte, j| {
                    self.code.items[pos + 1 + j] = byte;
                }
            },
        }
    }

    pub fn write_instruction(self: *Self, inst: Instruction, line: usize, alloc: std.mem.Allocator) !void {
        const opcode = @enumToInt(inst);
        try self.code.append(alloc, opcode);

        if (self.line_nos.items.len == 0) {
            try self.line_nos.append(alloc, .{ .line = line, .bytes = 0 });
        }

        var info: *LineInfo = &self.line_nos.items[self.line_nos.items.len - 1];
        if (info.line != line) {
            try self.line_nos.append(alloc, .{ .line = line, .bytes = 0 });
            info = &self.line_nos.items[self.line_nos.items.len - 1];
        }

        switch (inst) {
            inline else => |payload, tag| {
                const payload_size = comptime @sizeOf(@TypeOf(payload));
                info.bytes += payload_size + 1;

                switch (comptime tag) {
                    .Closure => {
                        try self.code.append(alloc, payload.func);
                        try self.code.append(alloc, @intCast(u8, payload.upvalues.len));
                        // for (payload.upvalues) |upvalue| {
                        //     try self.code.appendSlice(alloc, std.mem.asBytes(&upvalue));
                        // }
                        try self.code.appendSlice(alloc, std.mem.sliceAsBytes(payload.upvalues));
                        std.debug.assert(std.mem.sliceAsBytes(payload.upvalues).len ==
                            payload.upvalues.len * @sizeOf(Instruction.UpvalueRef));
                    },
                    else => {
                        // OOF: endianness might be a problem for getting cross compatible bytecode.
                        try self.code.appendSlice(alloc, std.mem.asBytes(&payload));
                    },
                }
            },
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

    // TODO:
    // each time next_byte is called - it has to go through many indirections. (first to self, then to chunk, then to items)
    chunk: *Chunk,
    // TODO: (each byte in Chunk is accessed using indexing instead of simple pointer derefs and pointer arithmatics)
    // it might be faster to deref a pointer than indexing an array.
    // but it is easier to modify the chunk curr position.
    // maybe check the speed difference and implement it using pointers instead
    curr: usize,

    pub fn has_next(self: *Self) bool {
        return self.chunk.code.items.len > self.curr;
    }

    fn next_byte(self: *Self) !u8 {
        if (self.has_next()) {
            defer self.curr += 1;
            return self.chunk.code.items[self.curr];
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

        @setEvalBranchQuota(2000);
        switch (opcode) {
            inline else => |tag| {
                const name = comptime std.meta.tagName(tag);
                const payload_type = comptime std.meta.TagPayload(Instruction, tag);
                const payload_size = comptime @sizeOf(payload_type);

                if (payload_size > 0) {
                    switch (comptime tag) {
                        .Closure => {
                            const func = try self.next_byte();
                            const len = try self.next_byte() * @sizeOf(Instruction.UpvalueRef);
                            const v = self.chunk.code.items[self.curr .. self.curr + len];
                            const val = std.mem.bytesAsSlice(Instruction.UpvalueRef, v);
                            inst = @unionInit(Instruction, name, .{ .func = func, .upvalues = val });
                            self.curr += len;
                        },
                        else => {
                            const start = self.chunk.code.items[self.curr..];

                            self.curr += payload_size;
                            if (self.chunk.code.items.len < self.curr) {
                                return error.NoBytes;
                            }

                            const val = std.mem.bytesToValue(payload_type, start[0..payload_size]);
                            inst = @unionInit(Instruction, name, val);
                        },
                    }
                } else {
                    inst = @unionInit(Instruction, name, {});
                }
            },
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
        print("line no. | byte no. | opcode | args \n", .{});

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

        switch (inst) {
            inline else => |payload, tag| {
                const name = comptime std.meta.tagName(tag);
                const payload_size = comptime @sizeOf(@TypeOf(payload));

                print("{s:<15} ", .{name});

                switch (comptime tag) {
                    .Closure => {
                        print("[fn({})] closure[local|index](", .{payload.func});
                        for (payload.upvalues) |uv| {
                            print("[{}|{}]", .{ uv.is_local, uv.index });
                        }
                        print(")", .{});
                    },
                    else => {
                        inline for ([_]u8{0} ** payload_size) |_, j| {
                            const bytes = std.mem.asBytes(&payload);
                            const byte = bytes[j];
                            print("{X:0>2} ", .{byte});
                        }
                    },
                }

                if (payload_size > 0) {
                    switch (comptime tag) {
                        .Closure => {},
                        else => {
                            print("({}) ", .{payload});
                        },
                    }

                    switch (comptime tag) {
                        .Constant => {
                            var val = self.chunk.consts.items[payload];
                            print("[", .{});
                            try val.print();
                            print("]", .{});
                        },
                        .DefineGlobal, .GetGlobal, .SetGlobal, .SetProperty, .GetProperty => {
                            var val = self.chunk.consts.items[payload];
                            var obj = try val.as(.Object);
                            var str = try obj.as(.String);
                            print("[{s}]", .{str.inner.str});
                        },
                        else => {},
                    }
                }
            },
        }

        print("\n", .{});
    }
};
