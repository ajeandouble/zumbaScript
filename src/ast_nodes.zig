const std = @import("std");
const dbg = @import("./debug.zig");
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;

pub const Num = struct {
    const Self = @This();
    token: Token,
    value: i64,

    pub fn make(num: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = num;
        return instance;
    }
};

pub const BinOp = struct {
    const Self = @This();
    token: Token = undefined,
    lhs: *const Node = undefined,
    rhs: *const Node = undefined,

    pub fn make(binop: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = binop;
        return instance;
    }
};

pub const UnaryOp = struct {
    const Self = @This();
    token: Token = undefined,
    value: *const Node = undefined,

    pub fn make(unaryop: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = unaryop;
        return instance;
    }
};

pub const Variable = struct {
    const Self = @This();
    token: Token = undefined,
    id: []const u8,

    pub fn make(variable: Self, allocator: std.mem.Allocator) anyerror!*Self {
        dbg.print(
            "id {s} lexeme={s}\n",
            .{ variable.id, variable.token.lexeme.? },
            @src(),
        );
        const instance = try allocator.create(Self);
        instance.* = variable;
        return instance;
    }
};

pub const FunctionCall = struct {
    const Self = @This();
    token: Token = undefined,
    id: []const u8,
    args: std.ArrayList(*Node),

    pub fn make(func_call: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = func_call;
        return instance;
    }
};

pub const FunctionDecl = struct {
    const Self = @This();
    token: Token = undefined,
    id: []const u8,
    args: std.ArrayList(*Node),
    statements: std.ArrayList(*Node),

    pub fn make(func_decl: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = func_decl;
        return instance;
    }
};

pub const Program = struct {
    const Self = @This();
    id: []const u8,
    global_statements: std.ArrayList(*Node),
    functions: std.ArrayList(*Node),

    pub fn make(program: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = program;
        return instance;
    }
};

pub const Return = struct {
    const Self = @This();
    token: Token = undefined,
    expr: *const Node = undefined,

    pub fn make(return_stmt: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = return_stmt;
        return instance;
    }
};

pub const IfBlock = struct {
    const Self = @This();
    token: Token = undefined,
    condition: *const Node = undefined,
    statements: std.ArrayList(*Node) = undefined,
    next_else: ?*ElseBlock = null,

    pub fn make(if_block: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = if_block;
        return instance;
    }
};

pub const ElseBlock = struct {
    const Self = @This();
    token: Token = undefined,
    condition: ?*const Node = undefined,
    statements: std.ArrayList(*Node) = undefined,
    next_else: ?*Self = undefined,

    pub fn make(else_block: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = else_block;
        return instance;
    }
};

pub const WhileBlock = struct {
    const Self = @This();
    token: Token = undefined,
    condition: *const Node = undefined,
    statements: std.ArrayList(*Node) = undefined,

    pub fn make(while_block: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = while_block;
        return instance;
    }
};

pub const BreakStatement = struct {
    const Self = @This();
    token: Token = undefined,

    pub fn make(break_stmt: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = break_stmt;
        return instance;
    }
};

pub const ContinueStatement = struct {
    const Self = @This();
    token: Token = undefined,

    pub fn make(break_stmt: Self, allocator: std.mem.Allocator) anyerror!*Self {
        const instance = try allocator.create(Self);
        instance.* = break_stmt;
        return instance;
    }
};

pub const Node = union(enum) { num: *Num, binop: *BinOp, unaryop: *UnaryOp, variable: *Variable, func_call: *FunctionCall, func_decl: *FunctionDecl, program: *Program, ret: *Return, if_block: *IfBlock, else_block: *ElseBlock, while_block: *WhileBlock, break_stmt: *BreakStatement, continue_stmt: *ContinueStatement };
