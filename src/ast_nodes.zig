const std = @import("std");
const dbg = @import("./debug.zig");
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;

pub const Num = struct {
    const Self = @This();
    token: Token,
    value: i64,
};

pub const Array = struct {
    const Self = @This();
    token: Token,
    elements: std.ArrayList(*Node),
};

pub const BinOp = struct {
    const Self = @This();
    token: Token = undefined,
    lhs: *const Node = undefined,
    rhs: *const Node = undefined,
};

pub const UnaryOp = struct {
    const Self = @This();
    token: Token = undefined,
    value: *const Node = undefined,
};

pub const Variable = struct {
    const Self = @This();
    token: Token = undefined,
    id: []const u8,
};

pub const FunctionCall = struct {
    const Self = @This();
    token: Token = undefined,
    id: []const u8,
    args: std.ArrayList(*Node),
};

pub const FunctionDecl = struct {
    const Self = @This();
    token: Token = undefined,
    id: []const u8,
    args: std.ArrayList(*Node),
    statements: std.ArrayList(*Node),
};

pub const Program = struct {
    const Self = @This();
    id: []const u8,
    global_statements: std.ArrayList(*Node),
    functions: std.ArrayList(*Node),
};

pub const Return = struct {
    const Self = @This();
    token: Token = undefined,
    expr: *const Node = undefined,
};

pub const IfBlock = struct {
    const Self = @This();
    token: Token = undefined,
    condition: *const Node = undefined,
    statements: std.ArrayList(*Node) = undefined,
    next_else: ?*ElseBlock = null,
};

pub const ElseBlock = struct {
    const Self = @This();
    token: Token = undefined,
    condition: ?*const Node = undefined,
    statements: std.ArrayList(*Node) = undefined,
    next_else: ?*Self = undefined,
};

pub const WhileBlock = struct {
    const Self = @This();
    token: Token = undefined,
    condition: *const Node = undefined,
    statements: std.ArrayList(*Node) = undefined,
};

pub const BreakStatement = struct {
    const Self = @This();
    token: Token = undefined,
};

pub const ContinueStatement = struct {
    const Self = @This();
    token: Token = undefined,
};

pub const Node = union(enum) { num: Num, array: Array, binop: BinOp, unaryop: UnaryOp, variable: Variable, func_call: FunctionCall, func_decl: FunctionDecl, program: Program, ret: Return, if_block: IfBlock, else_block: ElseBlock, while_block: WhileBlock, break_stmt: BreakStatement, continue_stmt: ContinueStatement };
