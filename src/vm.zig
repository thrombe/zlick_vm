const std = @import("std");

const build_options = @import("build_options");

const code_mod = @import("code.zig");
const Chunk = code_mod.Chunk;
const Disassembler = code_mod.Disassembler;
const Value = code_mod.Value;
const Function = code_mod.Function;
const Closure = code_mod.Closure;
const Upvalue = code_mod.Upvalue;
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
        UndfinedProperty,
    };
    pub const Result = enum {
        Ok,
    };

    const GlobalValues = std.StringHashMap(Value);
    const CallFrame = struct {
        closure: *Closure,
        reader: ChunkReader,
        stack_top: usize,
    };

    frames: []CallFrame,
    frame_top: usize = 0,
    stack: []Value,
    stack_top: usize = 0,

    open_upvalues: ?*Upvalue,

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

    // allocator for Zlick Vm, compiler, et. al.
    alloc: std.mem.Allocator,
    // allocator to be used by Zlick objects
    zalloc: *Allocator,

    // TODO: “direct threaded code”, “jump table”, and “computed goto”

    pub fn new(alloc: std.mem.Allocator, zalloc: *Allocator) !Self {
        var stack = try alloc.alloc(Value, 256 * 64);
        var frames = try alloc.alloc(CallFrame, 64);
        return .{
            .stack = stack,
            .globals = GlobalValues.init(alloc),
            .frames = frames,
            .open_upvalues = null,
            .alloc = alloc,
            .zalloc = zalloc,
        };
    }

    pub fn deinit(self: *Self) void {
        // for (self.stack[0..self.stack_top]) |*val| {
        //     val.deinit(self.alloc);
        // }
        self.alloc.free(self.stack);
        self.globals.deinit();
        self.alloc.free(self.frames);
    }

    fn push_callframe(self: *Self, closure: *Closure) !*CallFrame {
        if (self.frame_top >= self.frames.len) {
            return error.StackOverflow;
        }

        var callf = .{
            .closure = closure,
            .reader = closure.inner.func.inner.chunk.reader(),
            .stack_top = self.stack_top - closure.inner.func.inner.arity - 1,
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
        self.stack_top = self.frames[self.frame_top].stack_top;
        if (self.frame_top == 0) {
            return null;
        }
        return &self.frames[self.frame_top - 1];
    }

    fn stacktrace(self: *Self) void {
        std.debug.print("---- stack trace ----\n", .{});
        for (self.frames[0..self.frame_top]) |frame| {
            std.debug.print("fn {s}\n", .{frame.closure.inner.func.inner.name});
        }
        std.debug.print("---- stack trace end ----\n", .{});
    }

    fn define_native_fn(self: *Self, comptime fn_name: []const u8, comptime arity: u8, comptime T: type) !void {
        // /usr/lib/zig/std/builtin.zig
        var nfn = code_mod.NativeFunction.new(.{
            // .arity = std.meta.fieldNames(std.meta.ArgsTuple(@TypeOf(@field(@TypeOf(t), fn_name)))).len,
            .arity = arity,
            .name = fn_name,
            .func = @ptrCast(code_mod.NativeFunction.Inner.TypeFunc, &@field(T, fn_name)),
        });
        try self.globals.put(fn_name, try nfn.to_val(self.zalloc));
    }

    fn capture_upvalue(self: *Self, index: usize) !*Upvalue {
        var puv: ?*Upvalue = null;
        var cuv = self.open_upvalues;
        while (cuv != null and @ptrToInt(cuv.?.inner.val) > @ptrToInt(&self.stack[index])) {
            puv = cuv;
            cuv = cuv.?.inner.next;
        }

        if (cuv != null and @ptrToInt(cuv.?.inner.val) == @ptrToInt(&self.stack[index])) {
            return cuv.?;
        }

        var uv = try self.zalloc.create(Upvalue);
        uv.* = Upvalue.new(.{
            .val = &self.stack[index],
            .next = null,
            .closed = Value.new(null),
        });
        try self.zalloc.add_val(uv.as_val());

        uv.inner.next = cuv;
        if (puv) |ppuv| {
            ppuv.inner.next = uv;
        } else {
            self.open_upvalues = uv;
        }

        return uv;
    }

    fn close_upvalues(self: *Self, last: *Value) void {
        while (self.open_upvalues != null and @ptrToInt(self.open_upvalues.?.inner.val) >= @ptrToInt(last)) {
            var uv = self.open_upvalues.?;
            uv.inner.closed = uv.inner.val.*;
            uv.inner.val = &uv.inner.closed;
            self.open_upvalues = uv.inner.next;
        }
    }

    pub fn start_script(self: *Self, closure: *Closure) !Result {
        try self.push_value(closure.as_val());

        const Builtins = struct {
            fn clock(_: []Value) !Value {
                return .{ .Number = @intToFloat(f64, std.time.milliTimestamp()) };
            }
        };
        try self.define_native_fn("clock", 0, Builtins);

        self.zalloc.gc_toggle = true;
        // inside a defer block so that errors also free all the required stuff.
        defer {
            self.stack_top = 0;
            self.frame_top = 0;
            self.open_upvalues = null;
            self.globals.clearRetainingCapacity();

            self.zalloc.collect_garbage() catch unreachable;

            self.zalloc.gc_toggle = false;
        }

        errdefer self.stacktrace();

        return try self.run(closure);
    }

    pub fn run(self: *Self, closure: *Closure) !Result {
        var frame = try self.push_callframe(closure);

        while (frame.reader.has_next()) {
            const start = frame.reader.curr;
            const inst = try frame.reader.next_instruction();

            if (comptime build_options.trace_enable) {
                std.debug.print("stack >> ", .{});
                for (self.stack[0..self.stack_top]) |*e| {
                    std.debug.print("[", .{});
                    try e.print();
                    std.debug.print("]", .{});
                }
                std.debug.print("\n", .{});

                var dis = Disassembler.new(frame.reader.chunk);
                try dis.disassemble_instruction(inst, start);
            }

            switch (inst) {
                .Return => {
                    var result = try self.pop_value();
                    self.close_upvalues(&self.stack[frame.stack_top]);
                    frame = try self.pop_callframe() orelse return .Ok;

                    try self.push_value(result);
                },
                .Call => |args| {
                    var callee = self.stack[self.stack_top - 1 - args];
                    switch ((try callee.as(.Object)).tag) {
                        .Closure => {
                            var cal = callee.as_obj(.Closure) catch unreachable;
                            if (cal.inner.func.inner.arity != args) {
                                return error.IncorrectNumArgs;
                            }
                            frame = try self.push_callframe(cal);
                        },
                        .NativeFunction => {
                            var cal = callee.as_obj(.NativeFunction) catch unreachable;
                            if (cal.inner.arity != args) {
                                return error.IncorrectNumArgs;
                            }

                            var arguments = self.stack[frame.stack_top + 1 .. self.stack_top];
                            const res = try cal.inner.func(arguments);
                            _ = try self.pop_value();
                            try self.push_value(res);
                        },
                        .Class => {
                            var class = callee.as_obj(.Class) catch unreachable;
                            const instance = code_mod.Instance.new(.{ .class = class });
                            self.stack[self.stack_top - 1 - args] = try instance.to_val(self.zalloc);
                        },
                        else => return error.NotCallable,
                    }
                },
                .Closure => |val| {
                    var obj = frame.reader.chunk.consts.items[val.func];
                    var fun = try obj.as_obj(.Function);

                    var upvalues = try self.zalloc.alloc(*Upvalue, val.upvalues.len);
                    for (val.upvalues) |uvref, i| {
                        if (uvref.is_local) {
                            upvalues[i] = try self.capture_upvalue(frame.stack_top + uvref.index);
                        } else {
                            upvalues[i] = frame.closure.inner.upvalues[uvref.index];
                        }
                    }

                    var value = code_mod.Closure.new(.{ .func = fun, .upvalues = upvalues });
                    try self.push_value(try value.to_val(self.zalloc));
                },
                .SetUpvalue => |c| {
                    frame.closure.inner.upvalues[c].inner.val.* = try self.pop_value();
                },
                .GetUpvalue => |c| {
                    try self.push_value(frame.closure.inner.upvalues[c].inner.val.*);
                },
                .CloseUpvalue => {
                    const last = &self.stack[self.stack_top - 1];
                    self.close_upvalues(last);
                    _ = try self.pop_value();
                },
                .SetProperty => |n| {
                    var str = try frame.reader.chunk.consts.items[n].as_obj(.String);
                    var name = try self.zalloc.alloc(u8, str.inner.str.len);
                    std.mem.copy(u8, name, str.inner.str);
                    errdefer self.zalloc.free(name);

                    var val = try self.pop_value();
                    var obj = try self.pop_value();

                    var instance = try obj.as_obj(.Instance);

                    // even if this allocation is not gonna go through zalloc's alloc implementation - it should be fine ig
                    try instance.inner.fields.put(self.zalloc.zalloc, name, val);
                },
                .GetProperty => |n| {
                    var val = try self.pop_value();
                    var instance = try val.as_obj(.Instance);
                    var name = try frame.reader.chunk.consts.items[n].as_obj(.String);

                    var prop = instance.inner.fields.get(name.inner.str) orelse return error.UndfinedProperty;

                    try self.push_value(prop);
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
                .GetLocal => |i| try self.push_value(self.stack[frame.stack_top + i]),
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

const dbg = std.debug.print;
pub const Allocator = struct {
    const Self = @This();

    const ObjectList = std.ArrayList(*code_mod.Object);
    const growth_factor = 2;

    zalloc: std.mem.Allocator,
    aalloc: std.mem.Allocator,
    vm: *Vm,
    gc_toggle: bool = false,

    gray_stack: ObjectList,
    bytes_allocated: u64 = 0,
    gc_threshold: u64 = 1024 * 1024,

    objects: ObjectList,

    pub fn new(a: std.mem.Allocator, z: std.mem.Allocator, v: *Vm) Self {
        return .{
            .aalloc = a,
            .zalloc = z,
            .gray_stack = ObjectList.init(a),
            .objects = ObjectList.init(a),
            .vm = v,
        };
    }

    pub fn deinit(self: *Self) void {
        self.objects.deinit();
        self.gray_stack.deinit();
    }

    pub fn create(self: *Self, comptime T: type) !*T {
        self.bytes_allocated += @sizeOf(T);

        if (comptime build_options.gc_stresstest) try self.collect_garbage();

        if (self.bytes_allocated > self.gc_threshold) {
            try self.collect_garbage();
        }

        const ptr = try self.zalloc.create(T);

        if (comptime build_options.gc_log) dbg("allocate {*} type: {any}\n", .{ ptr, T });

        return ptr;
    }

    pub fn alloc(self: *Self, comptime T: type, size: usize) ![]T {
        self.bytes_allocated += size * @sizeOf(T);

        return try self.zalloc.alloc(T, size);
    }

    pub fn free(self: *Self, slice: anytype) void {
        self.bytes_allocated -= std.mem.sliceAsBytes(slice).len;

        self.zalloc.free(slice);
    }

    pub fn destroy(self: *Self, ptr: anytype) void {
        const T = @typeInfo(@TypeOf(ptr)).Pointer.child;

        if (comptime build_options.gc_log) dbg("destroy {*} type: {any}\n", .{ ptr, T });

        self.bytes_allocated -= @sizeOf(T);

        self.zalloc.destroy(ptr);
    }

    pub fn collect_garbage(self: *Self) !void {
        if (!self.gc_toggle) return;

        if (comptime build_options.gc_log) dbg("-- gc start\n", .{});

        var before = self.bytes_allocated;

        defer if (comptime build_options.gc_log) {
            dbg("-- gc end\n", .{});
            dbg("collected {} bytes from {} to {}. next at {}\n", .{
                before - self.bytes_allocated,
                before,
                self.bytes_allocated,
                self.gc_threshold,
            });
        };

        try self.mark_roots();
        if (comptime build_options.gc_log) dbg("tracing refrences: \n", .{});
        try self.trace_refrences();
        if (comptime build_options.gc_log) dbg("sweeping: \n", .{});
        self.sweep();

        self.gc_threshold = self.bytes_allocated * growth_factor;
    }

    pub fn add_val(self: *Self, val: Value) !void {
        var v = val;
        var o = v.as(.Object) catch return;
        try self.objects.append(o);
    }

    fn mark_roots(self: *Self) !void {
        if (comptime build_options.gc_log) dbg("stack values: \n", .{});
        for (self.vm.stack[0..self.vm.stack_top]) |val| {
            try self.mark_value(val);
        }

        if (comptime build_options.gc_log) dbg("global values: \n", .{});
        var globals = self.vm.globals.iterator();
        while (globals.next()) |e| {
            // e.key_ptr; // the objects for these strings are in chunk.consts which do not vary in runtime
            //  these are deallocated when the chunks are deallocated.
            try self.mark_value(e.value_ptr.*);
        }

        if (comptime build_options.gc_log) dbg("frames: \n", .{});
        for (self.vm.frames[0..self.vm.frame_top]) |frame| {
            try self.mark_value(frame.closure.as_val());
        }

        if (comptime build_options.gc_log) dbg("open upvalues: \n", .{});
        var uv = self.vm.open_upvalues;
        while (uv != null) : (uv = uv.?.inner.next) {
            try self.mark_value(uv.?.as_val());
        }
    }

    fn mark_value(self: *Self, val: Value) !void {
        switch (val) {
            .Bool, .Number, .None => {},
            .Object => |obj| {
                if (obj.gc_mark) return;
                obj.gc_mark = true;

                if (comptime build_options.gc_log) dbg("marking {*} obj: {any}\n", .{ obj, obj });

                try self.gray_stack.append(obj);
            },
        }
    }

    fn trace_refrences(self: *Self) !void {
        while (true) {
            var e = self.gray_stack.popOrNull() orelse break;

            if (comptime build_options.gc_log) dbg("blackening {*} obj: {any}\n", .{ e, e });

            switch (e.tag) {
                .Class => {},
                .Instance => {
                    var instance = e.try_as(.Instance).?;
                    try self.mark_value(instance.inner.class.as_val());
                    var vals = instance.inner.fields.valueIterator();
                    while (vals.next()) |v| {
                        try self.mark_value(v.*);
                    }
                },
                .String => {},
                .Function => {
                    var fun = e.try_as(.Function).?;
                    // fun.inner.name; does not need separate handling here :P
                    for (fun.inner.chunk.consts.items) |v| {
                        try self.mark_value(v);
                    }
                },
                .NativeFunction => {},
                .Closure => {
                    var closure = e.try_as(.Closure).?;
                    try self.mark_value(closure.inner.func.as_val());
                    for (closure.inner.upvalues) |v| {
                        try self.mark_value(v.as_val());
                    }
                },
                .Upvalue => {
                    var uv = e.try_as(.Upvalue).?;
                    try self.mark_value(uv.inner.closed);
                },
            }
        }
    }

    fn sweep(self: *Self) void {
        var curr: usize = 0;

        while (true) {
            if (curr == self.objects.items.len) break;

            var obj = self.objects.items[curr];
            if (obj.gc_mark) {
                obj.gc_mark = false;
                curr += 1;
            } else {
                if (self.objects.items.len - 1 == curr) {
                    _ = self.objects.pop();
                } else {
                    var last = self.objects.pop();
                    self.objects.items[curr] = last;
                }

                obj.deinit(self);
            }
        }
    }
};
