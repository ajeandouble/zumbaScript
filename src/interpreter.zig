const std = @import("std");
const dbg = @import("./debug.zig");
const AstNode = @import("./ast_nodes.zig");
const Node = @import("./ast_nodes.zig").Node;
const Program = @import("./ast_nodes.zig").Program;
const FunctionDecl = @import("./ast_nodes.zig").FunctionDecl;
const BinOp = @import("./ast_nodes.zig").BinOp;
const UnaryOp = @import("./ast_nodes.zig").UnaryOp;
const Num = @import("./ast_nodes.zig").Num;
const Array = @import("./ast_nodes.zig").Array;
const String = @import("./ast_nodes.zig").String;
const Variable = @import("./ast_nodes.zig").Variable;
const FunctionCall = @import("./ast_nodes.zig").FunctionCall;
const IfBlock = @import("./ast_nodes.zig").IfBlock;
const WhileBlock = @import("./ast_nodes.zig").WhileBlock;
const BreakStatement = @import("./ast_nodes.zig").BreakStatement;
const ContinueStatement = @import("./ast_nodes.zig").ContinueStatement;
const ReturnStatement = @import("./ast_nodes.zig").ReturnStatement;
const Subscript = @import("./ast_nodes.zig").Subscript;
const Slice = @import("./ast_nodes.zig").Slice;
const TokenType = @import("./tokens.zig").TokenType;
const Token = @import("./tokens.zig").Token;

const NotImplemented = error{NotImplemented}.NotImplemented;
const MAX_CALL_DEPTH = 1000;
const Error = error{ NotImplemented, InterpreterError, DuplicateFunctionDeclaration, FunctionIsNotDeclared, MissingMainFunctionDeclaration, WrongBinOpTypes, MismatchingBinOpTypes, InvalidGlobalStatement, VariableIsNotDeclared, MainShouldReturnInteger, InvalidIfBlockExpression, InvalidElseBlockExpression, InvalidWhileBlockExpression, InvalidContinueStatementExpression, InvalidConditionType, UnexpectedControlFlow, IndexOutOfBounds, CallStackOverflow };
const ControlFlow = enum { Continue, Break, Return };
const ValueType = enum { integer, float, string, array, void };
const Value = union(enum) { integer: i64, float: f64, string: *String, array: []Value, void: void };
const TraceEntry = struct { name: []const u8, line: usize };
const EvalResultErr = struct { type: Error, line: usize = 0, msg: []const u8 = "" };
const EvalResult = union(enum) {
    const Self = @This();
    value: Value,
    err: EvalResultErr,
    break_stmt,
    continue_stmt,
    return_val: Value,

    pub fn isValue(self: Self) bool {
        return self == .value;
    }

    pub fn getValue(self: Self) Error!Value {
        return switch (self) {
            .value => self.value,
            .err => |e| return e.type,
            else => return Error.UnexpectedControlFlow,
        };
    }

    pub fn ok(value: Value) Self {
        return Self{ .value = value };
    }

    pub fn isError(self: Self) bool {
        return self == .err;
    }

    pub fn failure(e: EvalResultErr) Self {
        return Self{ .err = e };
    }

    pub fn isControlFlow(stmt: Self) bool {
        return switch (stmt) {
            .break_stmt, .continue_stmt, .return_val => true,
            else => false,
        };
    }
};

pub const StackFrame = struct {
    const Self = @This();
    symbols: std.StringHashMap(EvalResult),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Self {
        const locals = std.StringHashMap(EvalResult).init(allocator);
        return Self{ .symbols = locals, .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        var it = self.symbols.iterator();
        while (it.next()) |item| {
            self.allocator.free(item.key_ptr.*);
        }
        self.symbols.deinit();
    }
};

pub const Interpreter = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    stack: std.ArrayList(StackFrame),
    global_funcs: std.StringHashMap(*const FunctionDecl),
    owned_strings: std.ArrayList(*String),
    owned_arrays: std.ArrayList([]Value),
    source_lines: []const []const u8,
    call_trace: std.ArrayList(TraceEntry),
    call_depth: usize = 0,
    ast: *const Program = undefined,

    pub fn init(ast: *Program, allocator: std.mem.Allocator, source_lines: []const []const u8) !Self {
        const stack = try std.ArrayList(StackFrame).initCapacity(allocator, 1024);
        const global_funcs = std.StringHashMap(*const FunctionDecl).init(allocator);
        const owned_strings = std.ArrayList(*String){};
        const owned_arrays = std.ArrayList([]Value){};
        const call_trace = std.ArrayList(TraceEntry){};
        return Self{
            .allocator = allocator,
            .ast = ast,
            .stack = stack,
            .global_funcs = global_funcs,
            .owned_strings = owned_strings,
            .owned_arrays = owned_arrays,
            .source_lines = source_lines,
            .call_trace = call_trace,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.stack.items) |*frame| {
            frame.deinit();
        }
        self.stack.deinit(self.allocator);
        var it_funcs = self.global_funcs.iterator();
        while (it_funcs.next()) |item| {
            self.allocator.free(item.key_ptr.*);
        }
        self.global_funcs.deinit();
        for (self.owned_strings.items) |s| {
            s.deinit();
            self.allocator.destroy(s);
        }
        self.owned_strings.deinit(self.allocator);
        for (self.owned_arrays.items) |arr| {
            self.allocator.free(arr);
        }
        self.owned_arrays.deinit(self.allocator);
        self.call_trace.deinit(self.allocator);
    }

    pub inline fn isTruethy(value: Value) !bool {
        return switch (value) {
            .integer => |i| i != 0,
            .float => |f| f != 0.0,
            .string => |s| s.value.len > 0,
            .array => |a| a.len > 0,
            .void => false,
        };
    }

    fn computeBinOp(self: *Self, op: TokenType, lhs: Value, rhs: Value) !Value {
        if (lhs == .void or rhs == .void) {
            return switch (op) {
                .eq => .{ .integer = 0 },
                .ne => .{ .integer = 1 },
                else => Error.MismatchingBinOpTypes,
            };
        }

        if (lhs == .string and rhs == .string) {
            if (op == .plus) {
                const l = lhs.string.value;
                const r = rhs.string.value;
                const buf = try self.allocator.alloc(u8, l.len + r.len);
                @memcpy(buf[0..l.len], l);
                @memcpy(buf[l.len..], r);
                const s = try self.allocator.create(String);
                s.* = String{ .token = lhs.string.token, .value = buf, .allocator = self.allocator };
                try self.owned_strings.append(self.allocator, s);
                return .{ .string = s };
            }
            const equal = std.mem.eql(u8, lhs.string.value, rhs.string.value);
            return switch (op) {
                .eq => .{ .integer = @intFromBool(equal) },
                .ne => .{ .integer = @intFromBool(!equal) },
                else => Error.MismatchingBinOpTypes,
            };
        }

        if (lhs == .array) {
            if (op != .plus) return Error.WrongBinOpTypes;
            if (rhs != .array) return Error.MismatchingBinOpTypes;
            const la = lhs.array;
            const ra = rhs.array;
            const buf = try self.allocator.alloc(Value, la.len + ra.len);
            @memcpy(buf[0..la.len], la);
            @memcpy(buf[la.len..], ra);
            try self.owned_arrays.append(self.allocator, buf);
            return .{ .array = buf };
        }

        const lhs_is_num = lhs == .integer or lhs == .float;
        const rhs_is_num = rhs == .integer or rhs == .float;
        if (!lhs_is_num or !rhs_is_num) return Error.MismatchingBinOpTypes;

        if (lhs == .integer and rhs == .integer) {
            const l = lhs.integer;
            const r = rhs.integer;
            return switch (op) {
                .plus => .{ .integer = l + r },
                .minus => .{ .integer = l - r },
                .mul => .{ .integer = l * r },
                .div => .{ .integer = @divTrunc(l, r) },
                .mod => .{ .integer = @mod(l, r) },
                .lt => .{ .integer = @intFromBool(l < r) },
                .le => .{ .integer = @intFromBool(l <= r) },
                .eq => .{ .integer = @intFromBool(l == r) },
                .ne => .{ .integer = @intFromBool(l != r) },
                .ge => .{ .integer = @intFromBool(l >= r) },
                .gt => .{ .integer = @intFromBool(l > r) },
                else => Error.NotImplemented,
            };
        }

        const l: f64 = switch (lhs) {
            .integer => |i| @floatFromInt(i),
            .float => |f| f,
            else => unreachable,
        };
        const r: f64 = switch (rhs) {
            .integer => |i| @floatFromInt(i),
            .float => |f| f,
            else => unreachable,
        };
        return switch (op) {
            .plus => .{ .float = l + r },
            .minus => .{ .float = l - r },
            .mul => .{ .float = l * r },
            .div => .{ .float = l / r },
            .mod => Error.MismatchingBinOpTypes,
            .lt => .{ .integer = @intFromBool(l < r) },
            .le => .{ .integer = @intFromBool(l <= r) },
            .eq => .{ .integer = @intFromBool(l == r) },
            .ne => .{ .integer = @intFromBool(l != r) },
            .ge => .{ .integer = @intFromBool(l >= r) },
            .gt => .{ .integer = @intFromBool(l > r) },
            else => Error.NotImplemented,
        };
    }

    pub fn pushStackFrame(self: *Self) !void {
        dbg.print("\n", .{}, @src());
        const frame = try StackFrame.init(self.allocator);
        try self.stack.append(self.allocator, frame);
        dbg.print("Stack: capacity = {}, length = {}\n", .{ self.stack.capacity, self.stack.items.len }, @src());
    }

    pub fn popStackFrame(self: *Self) !void {
        dbg.print("\n", .{}, @src());
        var frame = @constCast(&self.stack.getLast());
        frame.deinit();
        _ = self.stack.pop();
    }

    fn visitInteger(_: *Self, node: *const Num) EvalResult {
        dbg.print("{}\n", .{node.value}, @src());
        return EvalResult.ok(Value{ .integer = node.value });
    }

    fn visitString(_: *Self, node: *const String) EvalResult {
        return EvalResult.ok(Value{ .string = @constCast(node) });
    }

    fn visitSubscript(self: *Self, node: *const Subscript) anyerror!EvalResult {
        const target_res = try self.visit(node.target);
        if (target_res.isError()) return target_res;
        const idx_res = try self.visit(node.index);
        if (idx_res.isError()) return idx_res;
        const idx = (try idx_res.getValue()).integer;
        return switch (try target_res.getValue()) {
            .string => |s| {
                if (idx < 0 or idx >= @as(i64, @intCast(s.value.len)))
                    return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .line = node.token.line });
                const i: usize = @intCast(idx);
                const ch = try String.initFromSlice(node.token, s.value[i .. i + 1], self.allocator);
                const ptr = try self.allocator.create(String);
                ptr.* = ch;
                try self.owned_strings.append(self.allocator, ptr);
                return EvalResult.ok(.{ .string = ptr });
            },
            .array => |arr| {
                if (idx < 0 or idx >= @as(i64, @intCast(arr.len)))
                    return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .line = node.token.line });
                const i: usize = @intCast(idx);
                return EvalResult.ok(arr[i]);
            },
            else => EvalResult.failure(.{ .type = Error.NotImplemented, .line = node.token.line }),
        };
    }

    fn visitSlice(self: *Self, node: *const Slice) anyerror!EvalResult {
        const target_res = try self.visit(node.target);
        if (target_res.isError()) return target_res;
        const lo_res = try self.visit(node.lo);
        if (lo_res.isError()) return lo_res;
        const hi_res = try self.visit(node.hi);
        if (hi_res.isError()) return hi_res;
        const lo_raw = (try lo_res.getValue()).integer;
        const hi_raw = (try hi_res.getValue()).integer;
        return switch (try target_res.getValue()) {
            .string => |s| {
                if (lo_raw < 0 or hi_raw < 0 or lo_raw > hi_raw or @as(usize, @intCast(hi_raw)) > s.value.len)
                    return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .line = node.token.line });
                const lo: usize = @intCast(lo_raw);
                const hi: usize = @intCast(hi_raw);
                const ptr = try self.allocator.create(String);
                ptr.* = try String.initFromSlice(node.token, s.value[lo..hi], self.allocator);
                try self.owned_strings.append(self.allocator, ptr);
                return EvalResult.ok(.{ .string = ptr });
            },
            .array => |arr| {
                if (lo_raw < 0 or hi_raw < 0 or lo_raw > hi_raw or @as(usize, @intCast(hi_raw)) > arr.len)
                    return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .line = node.token.line });
                const lo: usize = @intCast(lo_raw);
                const hi: usize = @intCast(hi_raw);
                const buf = try self.allocator.alloc(Value, hi - lo);
                @memcpy(buf, arr[lo..hi]);
                try self.owned_arrays.append(self.allocator, buf);
                return EvalResult.ok(.{ .array = buf });
            },
            else => EvalResult.failure(.{ .type = Error.NotImplemented, .line = node.token.line }),
        };
    }

    fn visitArray(self: *Self, node: *const Array) anyerror!EvalResult {
        const elems = node.elements.items;
        const buf = try self.allocator.alloc(Value, elems.len);
        for (elems, 0..) |elem, i| {
            buf[i] = try (try self.visit(elem)).getValue();
        }
        try self.owned_arrays.append(self.allocator, buf);
        return EvalResult.ok(.{ .array = buf });
    }

    fn visitStatements(self: *Self, statements: std.ArrayList(*Node)) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const state = struct {
            var i: usize = 0;
        };
        state.i += 1;
        dbg.print("nth callstack: {}\n", .{state.i}, @src());
        for (statements.items) |stmt| {
            dbg.printNodeUnion(stmt, @src());
            const res = try self.visit(stmt);
            if (res.isError() or res.isControlFlow()) {
                return res;
            }
        }
        return EvalResult.ok(.{ .void = {} });
    }

    fn visitVariable(self: *Self, node: *const Variable) anyerror!EvalResult {
        dbg.print("variable id={s}\n", .{node.id}, @src());
        const frames = self.stack.items;
        // Check current (top) frame first, then global (bottom) frame.
        if (frames.len > 0) {
            if (frames[frames.len - 1].symbols.get(node.id)) |r| return r;
        }
        if (frames.len > 1) {
            if (frames[0].symbols.get(node.id)) |r| return r;
        }
        return EvalResult.failure(.{ .type = Error.VariableIsNotDeclared, .line = node.token.line });
    }

    fn visitFuncCall(self: *Self, func_call: *const FunctionCall) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const id = func_call.id;
        if (self.global_funcs.get(id)) |func| {
            if (self.call_depth >= MAX_CALL_DEPTH)
                return EvalResult.failure(.{ .type = Error.CallStackOverflow, .line = func_call.token.line });
            if (func_call.args.items.len != func.args.items.len)
                return EvalResult.failure(.{ .type = Error.InterpreterError, .line = func_call.token.line });

            // Evaluate arguments in the caller's frame before pushing the callee's frame.
            var arg_vals = try self.allocator.alloc(Value, func_call.args.items.len);
            defer self.allocator.free(arg_vals);
            for (func_call.args.items, 0..) |arg_node, i| {
                const res = try self.visit(arg_node);
                if (res.isError()) return res;
                arg_vals[i] = try res.getValue();
            }

            try self.call_trace.append(self.allocator, .{ .name = id, .line = func_call.token.line });
            try self.pushStackFrame();
            self.call_depth += 1;

            // Bind parameters in the callee's frame.
            const frame = &self.stack.items[self.stack.items.len - 1];
            for (func.args.items, 0..) |param_node, i| {
                const param_name = param_node.variable.id;
                const key = self.allocator.dupe(u8, param_name) catch
                    return EvalResult.failure(.{ .type = Error.InterpreterError, .line = func_call.token.line });
                frame.symbols.put(key, EvalResult.ok(arg_vals[i])) catch
                    return EvalResult.failure(.{ .type = Error.InterpreterError, .line = func_call.token.line });
            }

            const result = try self.visitStatements(func.statements);
            self.call_depth -= 1;
            try self.popStackFrame();
            if (!result.isError()) _ = self.call_trace.pop();
            return switch (result) {
                .return_val => EvalResult.ok(result.return_val),
                else => result,
            };
        } else {
            return EvalResult.failure(.{ .type = Error.FunctionIsNotDeclared, .line = func_call.token.line });
        }
    }

    fn visitIfBlock(self: *Self, if_block: *const IfBlock) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const cond_res = try self.visit(if_block.condition);
        if (cond_res.isError()) {
            return cond_res;
        }
        const if_cond_val: Value = try cond_res.getValue();
        const is_if_truethy = isTruethy(if_cond_val) catch |err| return EvalResult.failure(.{ .type = err, .line = if_block.token.line });
        if (is_if_truethy) {
            return self.visitStatements(if_block.statements);
        }
        var curr_else = if_block.next_else orelse return EvalResult.ok(.{ .void = {} });
        while (true) {
            if (curr_else.condition) |else_cond| {
                const else_res = try self.visit(else_cond);
                if (else_res.isError()) {
                    return else_res;
                }
                const else_cond_val = try else_res.getValue();
                const is_else_truethy = isTruethy(else_cond_val) catch |err| return EvalResult.failure(.{ .type = err, .line = curr_else.token.line });
                if (is_else_truethy) {
                    return self.visitStatements(curr_else.statements);
                }
            } else {
                return self.visitStatements(curr_else.statements);
            }

            curr_else = curr_else.next_else orelse break;
        }

        return EvalResult.ok(.{ .void = {} });
    }

    fn visitWhileBlock(self: *Self, while_block: *const WhileBlock) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        while (true) {
            const cond_res = try self.visit(while_block.condition);
            if (cond_res.isError()) {
                return cond_res;
            }
            const cond_res_val = cond_res.getValue() catch |err| return EvalResult.failure(.{ .type = err, .line = while_block.token.line });
            const is_truethy = try isTruethy(cond_res_val);
            if (!is_truethy) break;

            const body_res = try self.visitStatements(while_block.statements);
            if (body_res.isError()) return body_res;
            if (body_res.isControlFlow()) {
                switch (body_res) {
                    .break_stmt => break,
                    .continue_stmt => continue,
                    .return_val => return body_res,
                    else => unreachable,
                }
            }
        }
        return EvalResult.ok(.{ .void = {} });
    }

    fn visitLoopStatement(_: *Self, node: *const Node) EvalResult {
        return switch (node.*) {
            .break_stmt => EvalResult.break_stmt,
            .continue_stmt => EvalResult.continue_stmt,
            else => unreachable,
        };
    }

    fn visitReturnStmt(self: *Self, node: *const ReturnStatement) !EvalResult {
        const res = try self.visit(node.expr);
        if (res.isError()) {
            return res;
        }
        if (res == .return_val) {
            return res;
        }
        return EvalResult{ .return_val = res.value };
    }
    fn visit(self: *Self, node: *const Node) anyerror!EvalResult {
        dbg.printNodeUnion(node, @src());
        return switch (node.*) {
            .num => self.visitInteger(&node.num),
            .string => self.visitString(&node.string),
            .binop => self.visitBinOp(&node.binop),
            .unaryop => self.visitUnaryOp(&node.unaryop),
            .variable => self.visitVariable(&node.variable),
            .func_call => self.visitFuncCall(&node.func_call),
            .if_block => self.visitIfBlock(&node.if_block),
            .while_block => self.visitWhileBlock(&node.while_block),
            .break_stmt, .continue_stmt => self.visitLoopStatement(node),
            .ret => self.visitReturnStmt(&node.ret),
            .subscript => self.visitSubscript(&node.subscript),
            .slice => self.visitSlice(&node.slice),
            .array => self.visitArray(&node.array),
            else => EvalResult.failure(.{ .type = Error.NotImplemented, .line = 0 }),
        };
    }

    fn visitAssignment(self: *Self, binop: *const BinOp) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const rhs_result = try self.visit(binop.rhs);
        switch (binop.lhs.*) {
            .variable => |v| {
                const last_item_ptr = &self.stack.items[self.stack.items.len - 1];
                const locals_ptr = &last_item_ptr.*.symbols;
                if (locals_ptr.*.getPtr(v.id)) |val_ptr| {
                    val_ptr.* = rhs_result;
                } else {
                    const key = self.allocator.dupe(u8, v.id) catch return EvalResult.failure(.{ .type = Error.InterpreterError, .line = binop.token.line });
                    locals_ptr.*.put(key, rhs_result) catch return EvalResult.failure(.{ .type = Error.InterpreterError, .line = binop.token.line });
                }
                var it = last_item_ptr.*.symbols.iterator();
                while (it.next()) |item| {
                    dbg.print("{s}\n", .{item.key_ptr.*}, @src());
                    dbg.print("{}\n", .{item.value_ptr.*}, @src());
                }
            },
            .subscript => |sub| {
                const target_val = try (try self.visit(sub.target)).getValue();
                const idx_raw = (try (try self.visit(sub.index)).getValue()).integer;
                switch (target_val) {
                    .array => |arr| {
                        if (idx_raw < 0 or idx_raw >= @as(i64, @intCast(arr.len)))
                            return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .line = sub.token.line });
                        const i: usize = @intCast(idx_raw);
                        arr[i] = try rhs_result.getValue();
                    },
                    else => return EvalResult.failure(.{ .type = Error.NotImplemented, .line = sub.token.line }),
                }
            },
            else => return EvalResult.failure(.{ .type = Error.InterpreterError, .line = binop.token.line }),
        }
        return rhs_result;
    }

    fn visitBinOp(self: *Self, binop: *const BinOp) anyerror!EvalResult {
        dbg.print("\"{s}\"\n", .{binop.token.lexeme.?}, @src());
        if (binop.token.type == TokenType.assign) {
            return self.visitAssignment(binop);
        }

        if (binop.token.type == TokenType.and_op) {
            const lhs_res = try self.visit(binop.lhs);
            const lhs_val = try lhs_res.getValue();
            if (!try isTruethy(lhs_val)) return EvalResult.ok(.{ .integer = 0 });
            const rhs_res = try self.visit(binop.rhs);
            const rhs_val = try rhs_res.getValue();
            return EvalResult.ok(.{ .integer = @intFromBool(try isTruethy(rhs_val)) });
        }
        if (binop.token.type == TokenType.or_op) {
            const lhs_res = try self.visit(binop.lhs);
            const lhs_val = try lhs_res.getValue();
            if (try isTruethy(lhs_val)) return EvalResult.ok(.{ .integer = 1 });
            const rhs_res = try self.visit(binop.rhs);
            const rhs_val = try rhs_res.getValue();
            return EvalResult.ok(.{ .integer = @intFromBool(try isTruethy(rhs_val)) });
        }

        const lhs_res = try self.visit(binop.lhs);
        const lhs_val = try lhs_res.getValue();
        const rhs_res = try self.visit(binop.rhs);
        const rhs_val = try rhs_res.getValue();
        dbg.print("{}", .{lhs_val}, @src());
        dbg.print("{}", .{rhs_val}, @src());

        const result_val = self.computeBinOp(binop.token.type, lhs_val, rhs_val) catch |e| {
            return EvalResult.failure(.{ .type = @as(Error, @errorCast(e)), .line = binop.token.line });
        };
        return EvalResult.ok(result_val);
    }

    fn visitUnaryOp(self: *Self, unaryop: *const UnaryOp) anyerror!EvalResult {
        dbg.print("'{s}'\n", .{unaryop.token.lexeme.?}, @src());
        const result = try self.visit(unaryop.value);
        if (result.isError()) return result;
        const val = try result.getValue();
        return switch (unaryop.token.type) {
            .minus => switch (val) {
                .integer => |i| EvalResult.ok(.{ .integer = -i }),
                .float => |f| EvalResult.ok(.{ .float = -f }),
                else => EvalResult.failure(.{ .type = Error.MismatchingBinOpTypes, .line = unaryop.token.line }),
            },
            .not_op => EvalResult.ok(.{ .integer = @intFromBool(!try isTruethy(val)) }),
            else => result,
        };
    }

    fn printRuntimeError(self: *const Self, e: EvalResultErr) void {
        const stderr = std.fs.File.stderr();
        stderr.writeAll("Runtime error: ") catch {};
        stderr.writeAll(@errorName(e.type)) catch {};
        stderr.writeAll("\n") catch {};
        if (e.line < self.source_lines.len) {
            var buf: [32]u8 = undefined;
            const prefix = std.fmt.bufPrint(&buf, "  line {} | ", .{e.line + 1}) catch "  | ";
            stderr.writeAll(prefix) catch {};
            stderr.writeAll(self.source_lines[e.line]) catch {};
            stderr.writeAll("\n") catch {};
        }
        const MAX_TRACE_LINES = 100;
        const total = self.call_trace.items.len;
        const print_count = @min(total, MAX_TRACE_LINES);
        var i: usize = total;
        var printed: usize = 0;
        while (i > 0 and printed < print_count) {
            i -= 1;
            printed += 1;
            const frame = self.call_trace.items[i];
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "  in {s} (called at line {})\n", .{ frame.name, frame.line + 1 }) catch "";
            stderr.writeAll(msg) catch {};
        }
        if (total > MAX_TRACE_LINES) {
            var buf: [64]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "  ... ({} more frames)\n", .{total - MAX_TRACE_LINES}) catch "";
            stderr.writeAll(msg) catch {};
        }
        if (total == 0) {
            stderr.writeAll("  in <global>\n") catch {};
        }
    }

    pub fn interpret(self: *Self) !i64 {
        dbg.print("\n", .{}, @src());
        const functions = self.ast.functions;

        dbg.print("funcs_len: {}\n", .{self.ast.functions.items.len}, @src());
        for (functions.items) |func| {
            switch (func.*) {
                .func_decl => {
                    const id = func.func_decl.id;
                    if (self.global_funcs.contains(id)) {
                        return Error.DuplicateFunctionDeclaration;
                    }
                    const key = try self.allocator.dupe(u8, id);
                    try self.global_funcs.put(key, &func.func_decl);
                },
                else => return Error.InterpreterError,
            }
        }

        const global_statements = self.ast.global_statements;
        dbg.print("global_statements len: {}\n", .{global_statements.items.len}, @src());

        try self.pushStackFrame();
        const ret = try self.visitStatements(global_statements);
        dbg.print("ret={}\n", .{ret}, @src());
        const stack_items = self.stack.items;
        dbg.print("stack_items len: {}\n", .{stack_items.len}, @src());

        var i: usize = 0;
        const global_symbols = self.stack.items[0].symbols;
        var it = global_symbols.iterator();
        while (it.next()) |item| {
            dbg.print("{s}: {}\n", .{ item.key_ptr.*, item.value_ptr.* }, @src());
            i += 1;
        }
        if (ret.isError()) {
            try self.popStackFrame();
            self.printRuntimeError(ret.err);
            return 1;
        }

        if (self.global_funcs.get("main")) |main_func| {
            try self.pushStackFrame();
            const main_ret = try self.visitStatements(main_func.statements);
            try self.popStackFrame();
            try self.popStackFrame();
            return switch (main_ret) {
                .return_val => switch (main_ret.return_val) {
                    .integer => main_ret.return_val.integer,
                    else => 0,
                },
                .err => |e| {
                    self.printRuntimeError(e);
                    return 1;
                },
                else => 0,
            };
        }

        try self.popStackFrame();
        return switch (ret) {
            .return_val => switch (ret.return_val) {
                .integer => ret.return_val.integer,
                else => 0,
            },
            .err => |e| {
                self.printRuntimeError(e);
                return 1;
            },
            else => 0,
        };
    }
};

const expectEqual = std.testing.expectEqual;
test "visitInteger should correctly return the integer value" {
    var dummyAST = Program{
        .id = "",
        .functions = std.ArrayList(*Node){},
        .global_statements = std.ArrayList(*Node){},
    };

    defer dummyAST.functions.deinit(std.testing.allocator);
    defer dummyAST.global_statements.deinit(std.testing.allocator);

    var interp = try Interpreter.init(&dummyAST, std.testing.allocator, &[_][]const u8{});
    defer interp.deinit();

    const dummyToken = Token{ .lexeme = "", .allocator = undefined, .type = TokenType.eof, .line = 0 };
    var num = Num{ .token = dummyToken, .value = 42 };
    try expectEqual(@as(i64, 42), interp.visitInteger(&num).value.integer);
}
