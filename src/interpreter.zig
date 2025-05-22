const std = @import("std");
const dbg = @import("./debug.zig");
const AstNode = @import("./ast_nodes.zig");
const Node = @import("./ast_nodes.zig").Node;
const Program = @import("./ast_nodes.zig").Program;
const FunctionDecl = @import("./ast_nodes.zig").FunctionDecl;
const BinOp = @import("./ast_nodes.zig").BinOp;
const UnaryOp = @import("./ast_nodes.zig").UnaryOp;
const Num = @import("./ast_nodes.zig").Num;
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
const Error = error{ NotImplemented, InterpreterError, DuplicateFunctionDeclaration, FunctionIsNotDeclared, MissingMainFunctionDeclaration, WrongBinOpTypes, MismatchingBinOpTypes, InvalidGlobalStatement, VariableIsNotDeclared, MainShouldReturnInteger, InvalidIfBlockExpression, InvalidElseBlockExpression, InvalidWhileBlockExpression, InvalidContinueStatementExpression, InvalidConditionType };
const ControlFlow = enum { Continue, Break, Return };
const ValueType = enum { integer, float, string, array, void };
const Value = union(enum) { integer: i64, float: f64, string: []u8, array: []Value, void: void };
const EvalResultErr = struct { type: Error, msg: []u8 };
const EvalResult = struct {
    const Self = @This();
    value: ?Value,
    err: ?EvalResultErr,
    control: ?ControlFlow,

    pub fn ok(value: Value) Self {
        return Self{ .value = value, .control = null, .err = null };
    }

    pub fn failure(e: EvalResultErr) Self {
        return Self{ .err = e, .value = null, .control = null };
    }

    pub fn controlFlow(stmt: ControlFlow) Self {
        return Self{ .control = stmt, .value = null, .err = null };
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
            .string => Error.InvalidConditionType,
            else => Error.InvalidConditionType,
        };
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

    fn visitStatements(self: *Self, statements: std.ArrayList(*Node)) EvalResult {
        dbg.print("\n", .{}, @src());
        for (statements.items) |stmt| {
            dbg.printNodeUnion(stmt, @src());
            const res = self.visit(stmt);
            if (res.err != null or res.control != null) {
                return res;
            }
        }
        // FIXME: delete this shit
        const state = struct {
            var bar: usize = 0;
        };
        state.bar += 1;
        dbg.print("STATE_BAR {}\n", .{state.bar}, @src());
        return EvalResult.ok(.{ .void = {} });
    }

    fn visitVariable(self: *Self, node: *const Variable) EvalResult {
        dbg.print("variable id={s}\n", .{node.id}, @src());
        var locals = self.stack.getLast().symbols;
        if (locals.get(node.id)) |eval_result| {
            return eval_result;
        } else {
            return EvalResult.failure(.{ .type = Error.VariableIsNotDeclared, .msg = "" });
        }
    }

    fn visitFuncCall(self: *Self, func_call: *const FunctionCall) EvalResult {
        dbg.print("\n", .{}, @src());
        const id = func_call.id;
        if (self.global_funcs.get(id)) |func| {
            dbg.print("id: {s}, statements len: {}, {*} {*}\n", .{ func_call.id, func.statements.items.len, self.global_funcs.get(id), func.statements.getLast() }, @src());
            return self.visitStatements(func.statements);
        } else {
            return EvalResult.failure(.{ .type = Error.FunctionIsNotDeclared, .msg = "" });
        }
    }

    // TODO: add a isTruethy function
    fn visitIfBlock(self: *Self, if_block: *const IfBlock) EvalResult {
        dbg.print("\n", .{}, @src());
        const cond_res = self.visit(if_block.condition);
        if (cond_res.err != null) return cond_res;
        if (cond_res.value) |value| {
            const is_truethy = isTruethy(value) catch |err| return EvalResult.failure(.{ .type = err, .msg = "" });
            if (is_truethy) {
                return self.visitStatements(if_block.statements);
            }
        }
        var curr_else = if_block.next_else orelse return EvalResult.ok(.{ .void = {} });
        while (true) {
            if (curr_else.condition) |else_cond| {
                const else_res = self.visit(else_cond);
                if (else_res.err != null) return else_res;
                if (else_res.value) |value| {
                    const is_truethy = isTruethy(value) catch |err| return EvalResult.failure(.{ .type = err, .msg = "" });
                    if (is_truethy) {
                        return self.visitStatements(if_block.statements);
                    }
                }
            } else {
                return self.visitStatements(curr_else.statements);
            }

            curr_else = curr_else.next_else orelse break;
        }

        return EvalResult.ok(.{ .void = {} });
    }

    fn visitWhileBlock(self: *Self, while_block: *const WhileBlock) EvalResult {
        dbg.print("\n", .{}, @src());
        while (true) {
            const cond_res = self.visit(while_block.condition);
            if (cond_res.err != null) return cond_res;
            if (cond_res.value == null) {
                return EvalResult.failure(.{ .type = Error.InterpreterError, .msg = "" });
            }
            const is_truethy = isTruethy((cond_res.value orelse unreachable)) catch |err| return EvalResult.failure(.{ .type = err, .msg = "" });
            if (!is_truethy) break;

            const body_result = self.visitStatements(while_block.statements);
            if (body_result.err != null) return body_result;
            if (body_result.control != null) {
                switch ((body_result.control orelse unreachable)) {
                    ControlFlow.Return => return body_result,
                    ControlFlow.Break => break,
                    ControlFlow.Continue => continue,
                }
            }
        }
        return EvalResult.ok(.{ .void = {} });
    }

    fn visitLoopStatement(_: *Self, node: *const Node) EvalResult {
        return switch (node.*) {
            .break_stmt => EvalResult.controlFlow(ControlFlow.Break),
            .continue_stmt => EvalResult.controlFlow(ControlFlow.Continue),
            else => unreachable,
        };
    }

    fn visitReturnStmt(self: *Self, node: *const ReturnStatement) EvalResult {
        const res = self.visit(node.expr);
        if (res.err == null) {
            return res;
        }
        return EvalResult.controlFlow(ControlFlow.Return);
    }
    fn visit(self: *Self, node: *const Node) EvalResult {
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

    fn visitAssignment(self: *Self, binop: *const BinOp) EvalResult {
        dbg.print("\n", .{}, @src());
        const last_item_ptr = &self.stack.items[self.stack.items.len - 1];
        const locals_ptr = &last_item_ptr.*.symbols;
        const rhs_result = self.visit(binop.rhs);
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

    fn visitBinOp(self: *Self, binop: *const BinOp) EvalResult {
        dbg.print("\"{s}\"\n", .{binop.token.lexeme.?}, @src());
        if (binop.token.type == TokenType.assign) {
            return self.visitAssignment(binop);
        }

        const lhs_result = self.visit(binop.lhs);
        const rhs_result = self.visit(binop.rhs);
        switch (lhs_result.value) {
            .integer => {
                return EvalResult{ .value = .{ .integer = try self.computeIntBinOp(binop, lhs_result, rhs_result) } };
            },
            else => return NotImplemented, // NOTE: e.g. concat strings
        }
    }

    fn visitUnaryOp(self: *Self, unaryop: *const UnaryOp) EvalResult {
        dbg.print("'{s}'\n", .{unaryop.token.lexeme.?}, @src());
        var result = self.visit(unaryop.value);
        if (result.err != null) {
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
        if (global_statements.items.len == 0) {
            dbg.print("No global statements\n", .{}, @src());
            return 1;
        }

        try self.pushStackFrame();
        _ = try self.visitStatements(global_statements);
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

        const result = self.return_val orelse EvalResult{ .value = .{ .void = {} } };
        switch (result) {
            .value => {
                switch (result.value) {
                    .void => return 0,
                    .integer => return result.value.integer,
                    .string => return 0,
                    else => return 0,
                }
            },
            .err => return 1,
        }
        return result.ret.integer;
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
