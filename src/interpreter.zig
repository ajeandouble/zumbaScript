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
const TokenType = @import("./tokens.zig").TokenType;
const Token = @import("./tokens.zig").Token;

const NotImplemented = error{NotImplemented}.NotImplemented;
const Error = error{ NotImplemented, InterpreterError, DuplicateFunctionDeclaration, FunctionIsNotDeclared, MissingMainFunctionDeclaration, WrongBinOpTypes, MismatchingBinOpTypes, InvalidGlobalStatement, VariableIsNotDeclared, MainShouldReturnInteger, InvalidIfBlockExpression, InvalidElseBlockExpression, InvalidWhileBlockExpression, InvalidContinueStatementExpression, InvalidConditionType, UnexpectedControlFlow };
const ControlFlow = enum { Continue, Break, Return };
const ValueType = enum { integer, float, string, array, void };
const Value = union(enum) { integer: i64, float: f64, string: *String, array: []Value, void: void };
const EvalResultErr = struct { type: Error, msg: []u8 };
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
    ast: *const Program = undefined,

    pub fn init(ast: *Program, allocator: std.mem.Allocator) !Self {
        const stack = try std.ArrayList(StackFrame).initCapacity(allocator, 1024);
        const global_funcs = std.StringHashMap(*const FunctionDecl).init(allocator);
        return Self{ .allocator = allocator, .ast = ast, .stack = stack, .global_funcs = global_funcs };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        var it_funcs = self.global_funcs.iterator();
        while (it_funcs.next()) |item| {
            self.allocator.free(item.key_ptr.*);
        }
        self.global_funcs.deinit();
    }

    // Helpers

    pub inline fn isTruethy(value: Value) !bool {
        return switch (value) {
            .integer => value.integer != 0,
            .string => Error.InvalidConditionType, // TODO: implement on strings
            else => Error.InvalidConditionType,
        };
    }

    fn computeIntBinOp(self: *Self, binop: *const BinOp, lhs_result: EvalResult, rhs_result: EvalResult) !i64 {
        dbg.print("\"{s}\"\n", .{binop.token.lexeme.?}, @src());
        dbg.printNodeUnion(&Node{ .binop = binop.* }, @src());
        dbg.print("{} {} {}\n", .{ lhs_result, binop.token.type, rhs_result }, @src());
        if (lhs_result.value != Value.integer or rhs_result.value != Value.integer) {
            return Error.WrongBinOpTypes;
        }
        const lhs_val = lhs_result.value.integer;
        const rhs_val = rhs_result.value.integer;
        dbg.print("{} {} {}\n", .{ lhs_val, binop.token.type, rhs_val }, @src());
        var computed: i64 = undefined;
        switch (binop.token.type) {
            TokenType.plus => computed = lhs_result.value.integer + rhs_result.value.integer,
            TokenType.minus => computed = lhs_result.value.integer - rhs_result.value.integer,
            TokenType.mul => computed = lhs_result.value.integer * rhs_result.value.integer,
            TokenType.div => computed = @divTrunc(lhs_result.value.integer, rhs_result.value.integer),
            TokenType.mod => computed = @mod(lhs_result.value.integer, rhs_result.value.integer),

            TokenType.lt => computed = @intFromBool(lhs_result.value.integer < rhs_result.value.integer),
            TokenType.le => computed = @intFromBool(lhs_result.value.integer <= rhs_result.value.integer),
            TokenType.eq => computed = @intFromBool(lhs_result.value.integer == rhs_result.value.integer),
            TokenType.ge => computed = @intFromBool(lhs_result.value.integer >= rhs_result.value.integer),
            TokenType.gt => computed = @intFromBool(lhs_result.value.integer > rhs_result.value.integer),

            else => return NotImplemented,
        }
        _ = self;
        dbg.print("new_val == {}\n", .{computed}, @src());
        return computed;
    }

    // Stack
    pub fn pushStackFrame(self: *Self) !void {
        dbg.print("\n", .{}, @src());
        const frame = try StackFrame.init(self.allocator);
        try self.stack.append(frame);
        dbg.print("Stack: capacity = {}, length = {}\n", .{ self.stack.capacity, self.stack.items.len }, @src());
    }

    pub fn popStackFrame(self: *Self) !void {
        dbg.print("\n", .{}, @src());
        var frame = @constCast(&self.stack.getLast());
        frame.deinit();
        _ = self.stack.pop();
    }

    // Node visiting
    fn visitInteger(_: *Self, node: *const Num) EvalResult {
        dbg.print("{}\n", .{node.value}, @src());
        return EvalResult.ok(Value{ .integer = node.value });
    }

    fn visitStatements(self: *Self, statements: std.ArrayList(*Node)) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        // FIXME: delete this shit
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
        var locals = self.stack.getLast().symbols;
        if (locals.get(node.id)) |eval_result| {
            return eval_result;
        } else {
            return EvalResult.failure(.{ .type = Error.VariableIsNotDeclared, .msg = "" });
        }
    }

    fn visitFuncCall(self: *Self, func_call: *const FunctionCall) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const id = func_call.id;
        if (self.global_funcs.get(id)) |func| {
            try self.pushStackFrame();
            const result = try self.visitStatements(func.statements);
            try self.popStackFrame();
            return switch (result) {
                .return_val => EvalResult.ok(result.return_val),
                else => result,
            };
        } else {
            return EvalResult.failure(.{ .type = Error.FunctionIsNotDeclared, .msg = "" });
        }
    }

    // TODO: add a isTruethy function
    fn visitIfBlock(self: *Self, if_block: *const IfBlock) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const cond_res = try self.visit(if_block.condition);
        if (cond_res.isError()) {
            return cond_res;
        }
        const if_cond_val: Value = try cond_res.getValue();
        const is_if_truethy = isTruethy(if_cond_val) catch |err| return EvalResult.failure(.{ .type = err, .msg = "" });
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
                const is_else_truethy = isTruethy(else_cond_val) catch |err| return EvalResult.failure(.{ .type = err, .msg = "" });
                if (is_else_truethy) {
                    return self.visitStatements(if_block.statements);
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
            const cond_res_val = cond_res.getValue() catch |err| return EvalResult.failure(.{ .type = err, .msg = "" });
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
            .binop => self.visitBinOp(&node.binop),
            .unaryop => self.visitUnaryOp(&node.unaryop),
            .variable => self.visitVariable(&node.variable),
            .func_call => self.visitFuncCall(&node.func_call),
            .if_block => self.visitIfBlock(&node.if_block),
            .while_block => self.visitWhileBlock(&node.while_block),
            .break_stmt, .continue_stmt => self.visitLoopStatement(node),
            else => EvalResult.failure(.{ .type = Error.NotImplemented, .msg = "" }),
            .ret => self.visitReturnStmt(&node.ret),
        };
    }

    fn visitAssignment(self: *Self, binop: *const BinOp) anyerror!EvalResult {
        dbg.print("\n", .{}, @src());
        const last_item_ptr = &self.stack.items[self.stack.items.len - 1];
        const locals_ptr = &last_item_ptr.*.symbols;
        const rhs_result = try self.visit(binop.rhs);
        const id = binop.lhs.*.variable.id;
        if (locals_ptr.*.getPtr(id)) |val_ptr| {
            val_ptr.* = rhs_result;
        } else {
            const key = self.allocator.dupe(u8, id) catch return EvalResult.failure(.{ .type = Error.InterpreterError, .msg = "" });
            locals_ptr.*.put(key, rhs_result) catch return EvalResult.failure(.{ .type = Error.InterpreterError, .msg = "" });
        }
        var it = last_item_ptr.*.symbols.iterator();
        while (it.next()) |item| {
            dbg.print("{s}\n", .{item.key_ptr.*}, @src());
            dbg.print("{}\n", .{item.value_ptr.*}, @src());
        }
        return rhs_result;
    }

    fn visitBinOp(self: *Self, binop: *const BinOp) anyerror!EvalResult {
        dbg.print("\"{s}\"\n", .{binop.token.lexeme.?}, @src());
        if (binop.token.type == TokenType.assign) {
            return self.visitAssignment(binop);
        }

        const lhs_res = try self.visit(binop.lhs);
        const lhs_val = try lhs_res.getValue();
        const rhs_res = try self.visit(binop.rhs);
        const rhs_val = try rhs_res.getValue();
        dbg.print("{?}", .{lhs_val}, @src());
        dbg.print("{?}", .{rhs_val}, @src());

        switch (lhs_val) {
            .integer => {
                return EvalResult{ .value = .{ .integer = try self.computeIntBinOp(binop, lhs_res, rhs_res) } };
            },
            else => return NotImplemented, // NOTE: e.g. concat strings
        }
    }

    fn visitUnaryOp(self: *Self, unaryop: *const UnaryOp) anyerror!EvalResult {
        dbg.print("'{s}'\n", .{unaryop.token.lexeme.?}, @src());
        var result = try self.visit(unaryop.value);
        if (result.isError()) {
            return result;
        }
        if (unaryop.token.type == TokenType.minus) {
            result.value.integer = -result.value.integer;
        }
        return result;
    }

    // pub fn visitGlobalStatements() {
    // }

    // TODO: fix return values propagation
    // TODO: add array declaration
    pub fn interpret(self: *Self) !i64 {
        dbg.print("\n", .{}, @src());
        const functions = self.ast.functions;

        dbg.print("funcs_len: {}\n", .{self.ast.functions.items.len}, @src());
        for (functions.items) |func| {
            switch (func.*) {
                .func_decl => {
                    const id = func.func_decl.id;
                    // dbg.print("Function id: {s}, addr:{*} {}\n", .{ id, &(func.func_decl.statements.items), func.func_decl.statements.items.len }, @src());
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
        try self.popStackFrame();
        return switch (ret) {
            .return_val => return switch (ret.return_val) {
                .integer => (ret.return_val.integer),
                else => 0,
            },
            .err => 1,
            else => 0,
        };
    }
};

// Tests that need access to private methods
const expectEqual = std.testing.expectEqual;
test "visitInteger should correctly return the integer value" {
    var dummyAST = Program{
        .id = "",
        .functions = std.ArrayList(*Node).init(std.testing.allocator),
        .global_statements = std.ArrayList(*Node).init(std.testing.allocator),
    };

    defer dummyAST.functions.deinit();
    defer dummyAST.global_statements.deinit();

    var interp = try Interpreter.init(&dummyAST, std.testing.allocator);
    defer interp.deinit();

    const dummyToken = Token{ .lexeme = "", .allocator = undefined, .type = TokenType.eof, .line = 0 };
    var num = Num{ .token = dummyToken, .value = 42 };
    try expectEqual(42, interp.visitInteger(&num));
}
