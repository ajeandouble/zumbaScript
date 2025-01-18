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
const TokenType = @import("./tokens.zig").TokenType;
const Token = @import("./tokens.zig").Token;
const activeTag = std.meta.activeTag;

const NotImplented = error{NotImplemented}.NotImplemented;
const Error = error{ InterpretError, DuplicateFunctionDeclaration, MissingMainFunctionDeclaration, WrongBinOpTypes, MismatchingBinOpTypes, InvalidGlobalStatement, VariableIsNotDeclared, MainShouldReturnInteger, InvalidIfBlockExpression };

const ValueType = enum { integer, float, string, array, void };
const Value = union(enum) { integer: i64, float: f64, string: []u8, array: []Value, void: void };

const ResultType = enum { value, err };
const Result = union(ResultType) { value: Value, err: struct { type: Error, msg: []u8 } };

pub const StackFrame = struct {
    const Self = @This();
    symbols: std.StringHashMap(Result),

    pub fn init(allocator: std.mem.Allocator) !Self {
        const locals = std.StringHashMap(Result).init(allocator);
        return Self{ .symbols = locals };
    }

    pub fn deinit(self: *Self) void {
        self.symbols.deinit();
    }
};

pub const Interpreter = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    stack: std.ArrayList(StackFrame),
    global_funcs: std.StringHashMap(*FunctionDecl),
    ast: *const Program = undefined,
    return_val: ?Result = undefined,

    pub fn init(ast: *Program, allocator: std.mem.Allocator) !Self {
        const stack = try std.ArrayList(StackFrame).initCapacity(allocator, 1024);
        const global_funcs = std.StringHashMap(*FunctionDecl).init(allocator);
        return Self{ .allocator = allocator, .ast = ast, .stack = stack, .global_funcs = global_funcs };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.global_funcs.deinit();
    }

    // Stack
    pub fn pushStackFrame(self: *Self) !void {
        const frame = try StackFrame.init(self.allocator);
        try self.stack.append(frame);
        dbg.print("Stack: capacity = {}, length = {}\n", .{ self.stack.capacity, self.stack.items.len }, @src());
    }

    pub fn popStackFrame(self: *Self) !void {
        dbg.print("\n", .{}, @src());
        const frame = &self.stack.getLast();
        var it = frame.symbols.iterator();
        while (it.next()) |item| {
            dbg.print("{s}\n", .{item.key_ptr.*}, @src());
            dbg.print("{}\n", .{item.value_ptr.*}, @src());
        }
        _ = self.stack.pop();
    }

    // Node visiting
    fn visitInteger(self: *Self, num: *Num) i64 {
        dbg.print("{}\n", .{num.value}, @src());
        _ = self;
        return num.value;
    }

    // Interpretation of funtion body
    fn visitCompoundStatement(self: *Self, statements: std.ArrayList(*Node)) !Result {
        dbg.print("\n", .{}, @src());
        var result: Result = Result{ .value = .{ .void = {} } };
        for (statements.items) |stmt| {
            dbg.printNodeUnion(stmt, @src());
            if (self.return_val != null) {
                break;
            }
            switch (stmt.*) {
                .ret => {
                    dbg.print("ret\n", .{}, @src());
                    result = try self.visit(stmt.ret.expr);
                    self.return_val = result;
                    dbg.print("tmp_ret == {}\n", .{result}, @src());
                    break;
                },
                else => result = try self.visit(stmt),
            }
            dbg.print("{} {?}\n", .{ result, self.return_val }, @src());
        }
        return result;
    }

    fn visitFuncBody(self: *Self, func_decl: *const FunctionDecl) !Result {
        dbg.print("{s}\n", .{func_decl.id}, @src());
        try self.pushStackFrame();
        const statements = func_decl.statements.items;
        const result = try self.visitCompoundStatement(statements);
        try self.popStackFrame();
        return result;
    }

    fn visitVariable(self: *Self, node: *const Variable) !Result {
        dbg.print("variable id={s}\n", .{node.id}, @src());
        var locals = self.stack.getLast().symbols;
        if (locals.get(node.id)) |value| {
            return value;
        } else {
            return Error.VariableIsNotDeclared;
        }
    }

    fn visitFuncDecl(self: *Self, node: *const FunctionDecl) !Result {
        dbg.print("{s}\n", .{node.id}, @src());
        const id = node.id;
        if (self.global_funcs.get(id)) |value| {
            try self.pushStackFrame();
            _ = try self.visitCompoundStatement(value.statements);
            try self.popStackFrame();
            const result = self.return_val;
            if (result != null) {
                self.return_val = null;
                return result.?;
            } else {
                return Result{ .value = .{ .void = {} } };
            }
        } else {
            return Error.VariableIsNotDeclared;
        }
    }

    fn visitFuncCall(self: *Self, func_call: *const FunctionCall) !Result {
        dbg.print("\n", .{}, @src());
        const id = func_call.id;
        if (self.global_funcs.get(id)) |value| {
            dbg.print("{}\n", .{value.statements.items.len}, @src());
            return self.visitCompoundStatement(value.statements);
        } else {
            return Error.VariableIsNotDeclared;
        }
    }

    fn visitIfBlock(self: *Self, if_block: *const IfBlock) !Result {
        dbg.print("", .{}, @src());
        const expr_result = try self.visit(if_block.condition);
        var condition: bool = undefined;
        switch (expr_result.value) {
            .integer => condition = expr_result.value.integer != 0,
            else => return Error.InvalidIfBlockExpression,
        }
        if (condition) {
            return self.visitCompoundStatement(if_block.statements);
        }
        return Result{ .value = .{ .void = {} } };
    }

    fn visit(self: *Self, node: *const Node) anyerror!Result {
        dbg.printNodeUnion(node, @src());
        switch (node.*) {
            .program => return NotImplented,
            .num => return Result{ .value = .{ .integer = self.visitInteger(node.*.num) } },
            .binop => return try self.visitBinOp(node.*.binop),
            .unaryop => return try self.visitUnaryOp(node.*.unaryop),
            .variable => return try self.visitVariable(node.*.variable),
            .func_call => return try self.visitFuncCall(node.*.func_call),
            .if_block => {
                return try self.visitIfBlock(node.*.if_block);
            },
            else => {
                return NotImplented;
            },
        }
    }

    fn computeIntBinOp(self: *Self, binop: *const BinOp, lhs_result: Result, rhs_result: Result) !i64 {
        dbg.print("\"{s}\"\n", .{binop.token.lexeme.?}, @src());
        dbg.printNodeUnion(&Node{ .binop = @constCast(binop) }, @src());
        dbg.print("{} {} {}\n", .{ lhs_result, binop.token.type, rhs_result }, @src());
        if (lhs_result.value != Value.integer or rhs_result.value != Value.integer) {
            return Error.WrongBinOpTypes;
        }
        const lhs_val = lhs_result.value.integer;
        const rhs_val = rhs_result.value.integer;
        dbg.print("{} {} {}\n", .{ lhs_val, binop.token.type, rhs_val }, @src());
        var new_val: i64 = undefined;
        switch (binop.token.type) {
            TokenType.plus => new_val = lhs_result.value.integer + rhs_result.value.integer,
            TokenType.minus => new_val = lhs_result.value.integer - rhs_result.value.integer,
            TokenType.mul => new_val = lhs_result.value.integer * rhs_result.value.integer,
            TokenType.div => new_val = @divTrunc(lhs_result.value.integer, rhs_result.value.integer),
            TokenType.mod => new_val = @mod(lhs_result.value.integer, rhs_result.value.integer),

            TokenType.lt => new_val = @intFromBool(lhs_result.value.integer < rhs_result.value.integer),
            TokenType.le => new_val = @intFromBool(lhs_result.value.integer <= rhs_result.value.integer),
            TokenType.eq => new_val = @intFromBool(lhs_result.value.integer == rhs_result.value.integer),
            TokenType.ge => new_val = @intFromBool(lhs_result.value.integer >= rhs_result.value.integer),
            TokenType.gt => new_val = @intFromBool(lhs_result.value.integer > rhs_result.value.integer),

            else => return NotImplented,
        }
        _ = self;
        dbg.print("new_val == {}\n", .{new_val}, @src());
        return new_val;
    }

    fn visitAssignment(self: *Self, binop: *const BinOp) !Result {
        dbg.print("\n", .{}, @src());
        const locals_ptr = &(self.stack.items[self.stack.items.len - 1].symbols);
        var locals = locals_ptr.*;
        const id = try self.allocator.dupe(u8, binop.*.lhs.variable.id);
        const rhs_result = try self.visit(binop.rhs);
        try locals.put(id, rhs_result);
        var it = self.stack.getLast().symbols.iterator();
        while (it.next()) |item| {
            dbg.print("{s}\n", .{item.key_ptr.*}, @src());
            dbg.print("{}\n", .{item.value_ptr.*}, @src());
        }
        locals_ptr.* = locals;
        return rhs_result;
    }

    fn visitBinOp(self: *Self, binop: *const BinOp) !Result {
        dbg.print("\"{s}\"\n", .{binop.token.lexeme.?}, @src());
        if (binop.token.type == TokenType.assign) {
            return try self.visitAssignment(binop);
        }

        const lhs_result = try self.visit(binop.lhs);
        const rhs_result = try self.visit(binop.rhs);
        switch (lhs_result.value) {
            .integer => {
                return Result{ .value = .{ .integer = try self.computeIntBinOp(binop, lhs_result, rhs_result) } };
            },
            else => return NotImplented, // NOTE: e.g. concat strings
        }
    }

    fn visitUnaryOp(self: *Self, unaryop: *const UnaryOp) !Result {
        dbg.print("'{s}'\n", .{unaryop.token.lexeme.?}, @src());
        var result = try self.visit(unaryop.value);

        switch (result.value) {
            .integer => {
                if (unaryop.token.type == TokenType.minus) {
                    result.value.integer = -result.value.integer;
                }
            },
            else => return NotImplented, // NOTE: e.g. coerce to number
        }
        return result;
    }

    pub fn interpret(self: *Self) !i64 {
        dbg.print("\n", .{}, @src());
        const functions = self.ast.functions;
        const global_statements = self.ast.global_statements;
        for (global_statements.items) |stmt| {
            switch (stmt.*) {
                .binop => {
                    _ = try self.visitAssignment(stmt.*.binop);
                },
                else => return Error.InvalidGlobalStatement,
            }
        }
        dbg.print("funcs_len: {}\n", .{self.ast.functions.items.len}, @src());
        for (functions.items) |func| {
            switch (func.*) {
                .func_decl => {
                    const decl = func.func_decl;
                    const id = decl.*.id;
                    dbg.print("Function id={s}\n", .{id}, @src());
                    if (self.global_funcs.contains(id)) {
                        return Error.DuplicateFunctionDeclaration;
                    }
                    const key = try self.allocator.dupe(u8, decl.id);
                    dbg.print("{s}\n", .{decl.id}, @src());
                    try self.global_funcs.put(key, decl);
                },
                else => return Error.InterpretError,
            }
        }
        dbg.print("global_len: {}\n", .{self.ast.global_statements.items.len}, @src());
        var i: usize = 0;
        try self.pushStackFrame();
        const result = try self.visitFuncDecl(self.global_funcs.get("main") orelse return Error.MissingMainFunctionDeclaration);
        switch (result) {
            .value => {
                switch (result.value) {
                    .void => return 0,
                    .integer => return result.value.integer,
                    .string => return 0,
                    else => return 0,
                }
            },
            .err => return Error.MainShouldReturnInteger,
        }
        const global_symbols = self.stack.items[0].symbols;
        var it = global_symbols.iterator();
        while (it.next()) |item| {
            dbg.print("{s}\n", .{item.key_ptr.*}, @src());
            dbg.print("{}\n", .{item.value_ptr.*}, @src());
            i += 1;
        }
        try self.popStackFrame();
        return result.ret.integer;
    }
};

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
    try std.testing.expectEqual(42, interp.visitInteger(&num));
}
