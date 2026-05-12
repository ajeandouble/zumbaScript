const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;
const Program = @import("./ast_nodes.zig").Program;
const AstNode = @import("./ast_nodes.zig");
const Node = @import("./ast_nodes.zig").Node;
const Num = @import("./ast_nodes.zig").Num;
const String = @import("./ast_nodes.zig").String;
const UnaryOp = @import("./ast_nodes.zig").UnaryOp;
const Variable = @import("./ast_nodes.zig").Variable;
const BinOp = @import("./ast_nodes.zig").BinOp;
const IfBlock = @import("./ast_nodes.zig").IfBlock;
const ElseBlock = @import("./ast_nodes.zig").ElseBlock;
const WhileBlock = @import("./ast_nodes.zig").WhileBlock;
const BreakStatement = @import("./ast_nodes.zig").BreakStatement;
const ContinueStatement = @import("./ast_nodes.zig").ContinueStatement;
const ReturnStatement = @import("./ast_nodes.zig").ReturnStatement;
const FunctionDecl = @import("./ast_nodes.zig").FunctionDecl;

const expect = std.testing.expectEqual;
const expectEqual = std.testing.expectEqual;

test "control flow: while, nested if break and nested else continue" {
    const allocator = std.testing.allocator;

    // Tokens
    const id_i_tok = Token{ .type = TokenType.id, .lexeme = "i", .line = 0, .allocator = allocator };
    const num_0_tok = Token{ .type = TokenType.integer, .lexeme = "0", .line = 0, .allocator = allocator };
    const num_1_tok = Token{ .type = TokenType.integer, .lexeme = "1", .line = 0, .allocator = allocator };
    const num_42_tok = Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = allocator };
    const lt_tok = Token{ .type = TokenType.lt, .lexeme = "<", .line = 0, .allocator = allocator };
    const ge_tok = Token{ .type = TokenType.ge, .lexeme = ">=", .line = 0, .allocator = allocator };
    const plus_tok = Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };
    const break_tok = Token{ .type = TokenType.break_kw, .lexeme = "break", .line = 0, .allocator = allocator };
    const continue_tok = Token{ .type = TokenType.continue_kw, .lexeme = "continue", .line = 0, .allocator = allocator };

    // Nodes — Node union fields are values, not pointers
    var id_i = Node{ .variable = Variable{ .id = "i", .token = id_i_tok } };
    var num_0 = Node{ .num = Num{ .value = 0, .token = num_0_tok } };
    var num_1 = Node{ .num = Num{ .value = 1, .token = num_1_tok } };
    var num_42 = Node{ .num = Num{ .value = 42, .token = num_42_tok } };

    // Initial assignment: i = 0
    var init_assign = Node{ .binop = BinOp{ .lhs = &id_i, .rhs = &num_0, .token = assign_tok } };

    // Increment: i = i + 1
    var plus_i = Node{ .binop = BinOp{ .lhs = &id_i, .rhs = &num_1, .token = plus_tok } };
    var inc_i = Node{ .binop = BinOp{ .lhs = &id_i, .rhs = &plus_i, .token = assign_tok } };

    // Condition: i < 42
    var while_cond = Node{ .binop = BinOp{ .lhs = &id_i, .rhs = &num_42, .token = lt_tok } };

    // Condition: i >= 42
    var break_cond = Node{ .binop = BinOp{ .lhs = &id_i, .rhs = &num_42, .token = ge_tok } };

    // Break / continue nodes
    var break_stmt = Node{ .break_stmt = BreakStatement{ .token = break_tok } };
    var continue_stmt = Node{ .continue_stmt = ContinueStatement{ .token = continue_tok } };

    // If block with break
    var if_block_stmts = std.ArrayList(*Node){};
    try if_block_stmts.append(allocator, &break_stmt);
    defer if_block_stmts.deinit(allocator);

    var if_block = Node{ .if_block = IfBlock{ .condition = &break_cond, .statements = if_block_stmts } };

    // Else block with increment + continue
    var else_block_stmts = std.ArrayList(*Node){};
    try else_block_stmts.append(allocator, &inc_i);
    try else_block_stmts.append(allocator, &continue_stmt);
    defer else_block_stmts.deinit(allocator);

    var else_block_obj = ElseBlock{ .condition = null, .statements = else_block_stmts };

    // Connect if → else
    if_block.if_block.next_else = &else_block_obj;

    // While block
    var while_block_stmts = std.ArrayList(*Node){};
    try while_block_stmts.append(allocator, &if_block);
    defer while_block_stmts.deinit(allocator);

    var while_block = Node{ .while_block = WhileBlock{ .condition = &while_cond, .statements = while_block_stmts } };

    // Main function statements
    var main_stmts = std.ArrayList(*Node){};
    try main_stmts.append(allocator, &init_assign);
    try main_stmts.append(allocator, &while_block);
    defer main_stmts.deinit(allocator);

    var return_node = Node{ .ret = ReturnStatement{ .expr = &id_i } };
    try main_stmts.append(allocator, &return_node);

    // Main function declaration
    var main_args = std.ArrayList(*Node){};
    defer main_args.deinit(allocator);

    var main_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, &main_func);
    defer functions.deinit(allocator);

    var global_statements = std.ArrayList(*Node){};
    defer global_statements.deinit(allocator);

    var dummyAST = Program{
        .id = "",
        .functions = functions,
        .global_statements = global_statements,
    };

    var interp = try Interpreter.init(&dummyAST, allocator);
    defer interp.deinit();

    const result = try interp.interpret();
    try std.testing.expectEqual(@as(i64, 42), result);
}

// Helper: build a minimal main() that returns a single expression node.
fn makeMainReturning(allocator: std.mem.Allocator, expr: *Node) !Program {
    const ret_stmts = try allocator.create(std.ArrayList(*Node));
    ret_stmts.* = std.ArrayList(*Node){};
    const ret_node = try allocator.create(Node);
    ret_node.* = Node{ .ret = ReturnStatement{ .expr = expr } };
    try ret_stmts.append(allocator, ret_node);

    const main_node = try allocator.create(Node);
    main_node.* = Node{ .func_decl = FunctionDecl{
        .id = "main",
        .args = std.ArrayList(*Node){},
        .statements = ret_stmts.*,
    } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, main_node);

    return Program{
        .id = "",
        .functions = functions,
        .global_statements = std.ArrayList(*Node){},
    };
}

test "typing: zero integer is falsy via if condition" {
    const allocator = std.testing.allocator;
    const eof_tok = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };

    // Build: if (0) { return 1 } else { return 0 }
    var num_0 = Node{ .num = Num{ .value = 0, .token = eof_tok } };
    var num_1 = Node{ .num = Num{ .value = 1, .token = eof_tok } };
    var num_0b = Node{ .num = Num{ .value = 0, .token = eof_tok } };
    _ = assign_tok;
    _ = eq_tok;

    var ret1 = Node{ .ret = ReturnStatement{ .expr = &num_1 } };
    var ret0 = Node{ .ret = ReturnStatement{ .expr = &num_0b } };

    var if_stmts = std.ArrayList(*Node){};
    try if_stmts.append(allocator, &ret1);
    defer if_stmts.deinit(allocator);

    var else_stmts = std.ArrayList(*Node){};
    try else_stmts.append(allocator, &ret0);
    defer else_stmts.deinit(allocator);

    var else_block = ElseBlock{ .condition = null, .statements = else_stmts };
    var if_node = Node{ .if_block = IfBlock{ .condition = &num_0, .statements = if_stmts, .next_else = &else_block } };

    var main_stmts = std.ArrayList(*Node){};
    try main_stmts.append(allocator, &if_node);
    defer main_stmts.deinit(allocator);

    var main_args = std.ArrayList(*Node){};
    defer main_args.deinit(allocator);
    var main_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, &main_func);
    defer functions.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = functions, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 0), try interp.interpret());
}

test "typing: nonzero integer is truthy via if condition" {
    const allocator = std.testing.allocator;
    const eof_tok = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };

    var num_42 = Node{ .num = Num{ .value = 42, .token = eof_tok } };
    var num_1 = Node{ .num = Num{ .value = 1, .token = eof_tok } };
    var num_0 = Node{ .num = Num{ .value = 0, .token = eof_tok } };

    var ret1 = Node{ .ret = ReturnStatement{ .expr = &num_1 } };
    var ret0 = Node{ .ret = ReturnStatement{ .expr = &num_0 } };

    var if_stmts = std.ArrayList(*Node){};
    try if_stmts.append(allocator, &ret1);
    defer if_stmts.deinit(allocator);
    var else_stmts = std.ArrayList(*Node){};
    try else_stmts.append(allocator, &ret0);
    defer else_stmts.deinit(allocator);

    var else_block = ElseBlock{ .condition = null, .statements = else_stmts };
    var if_node = Node{ .if_block = IfBlock{ .condition = &num_42, .statements = if_stmts, .next_else = &else_block } };

    var main_stmts = std.ArrayList(*Node){};
    try main_stmts.append(allocator, &if_node);
    defer main_stmts.deinit(allocator);
    var main_args = std.ArrayList(*Node){};
    defer main_args.deinit(allocator);
    var main_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, &main_func);
    defer functions.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = functions, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}

test "typing: !0 == 1 and !42 == 0" {
    const allocator = std.testing.allocator;
    const eof_tok = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const not_tok = Token{ .type = TokenType.not_op, .lexeme = "!", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };

    // !0 == 1
    var num_0 = Node{ .num = Num{ .value = 0, .token = eof_tok } };
    var not_0 = Node{ .unaryop = UnaryOp{ .token = not_tok, .value = &num_0 } };
    var num_1 = Node{ .num = Num{ .value = 1, .token = eof_tok } };
    var eq_node = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &not_0, .rhs = &num_1 } };

    // !42 == 0
    var num_42 = Node{ .num = Num{ .value = 42, .token = eof_tok } };
    var not_42 = Node{ .unaryop = UnaryOp{ .token = not_tok, .value = &num_42 } };
    var num_0b = Node{ .num = Num{ .value = 0, .token = eof_tok } };
    var eq_node2 = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &not_42, .rhs = &num_0b } };

    // result = (!0 == 1) * (!42 == 0)  →  1 * 1 = 1 (both true)
    const mul_tok = Token{ .type = TokenType.mul, .lexeme = "*", .line = 0, .allocator = allocator };
    var result_id = Node{ .variable = Variable{ .id = "r", .token = eof_tok } };
    var mul_node = Node{ .binop = BinOp{ .token = mul_tok, .lhs = &eq_node, .rhs = &eq_node2 } };
    var assign_node = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &result_id, .rhs = &mul_node } };
    var ret_node = Node{ .ret = ReturnStatement{ .expr = &result_id } };

    var main_stmts = std.ArrayList(*Node){};
    try main_stmts.append(allocator, &assign_node);
    try main_stmts.append(allocator, &ret_node);
    defer main_stmts.deinit(allocator);
    var main_args = std.ArrayList(*Node){};
    defer main_args.deinit(allocator);
    var main_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, &main_func);
    defer functions.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = functions, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}

test "typing: string equality - same content" {
    const allocator = std.testing.allocator;
    const eof_tok = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    var str_a = try String.initFromSlice(eof_tok, "hello", allocator);
    defer str_a.deinit();
    var str_b = try String.initFromSlice(eof_tok, "hello", allocator);
    defer str_b.deinit();

    var lhs = Node{ .string = str_a };
    var rhs = Node{ .string = str_b };
    var eq_node = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &lhs, .rhs = &rhs } };

    var result_id = Node{ .variable = Variable{ .id = "r", .token = eof_tok } };
    var assign_node = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &result_id, .rhs = &eq_node } };
    var ret_node = Node{ .ret = ReturnStatement{ .expr = &result_id } };

    var main_stmts = std.ArrayList(*Node){};
    try main_stmts.append(allocator, &assign_node);
    try main_stmts.append(allocator, &ret_node);
    defer main_stmts.deinit(allocator);
    var main_args = std.ArrayList(*Node){};
    defer main_args.deinit(allocator);
    var main_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, &main_func);
    defer functions.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = functions, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}

test "typing: string equality - different content" {
    const allocator = std.testing.allocator;
    const eof_tok = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const ne_tok = Token{ .type = TokenType.ne, .lexeme = "!=", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    var str_a = try String.initFromSlice(eof_tok, "foo", allocator);
    defer str_a.deinit();
    var str_b = try String.initFromSlice(eof_tok, "bar", allocator);
    defer str_b.deinit();

    var lhs = Node{ .string = str_a };
    var rhs = Node{ .string = str_b };
    var ne_node = Node{ .binop = BinOp{ .token = ne_tok, .lhs = &lhs, .rhs = &rhs } };

    var result_id = Node{ .variable = Variable{ .id = "r", .token = eof_tok } };
    var assign_node = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &result_id, .rhs = &ne_node } };
    var ret_node = Node{ .ret = ReturnStatement{ .expr = &result_id } };

    var main_stmts = std.ArrayList(*Node){};
    try main_stmts.append(allocator, &assign_node);
    try main_stmts.append(allocator, &ret_node);
    defer main_stmts.deinit(allocator);
    var main_args = std.ArrayList(*Node){};
    defer main_args.deinit(allocator);
    var main_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts } };

    var functions = std.ArrayList(*Node){};
    try functions.append(allocator, &main_func);
    defer functions.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = functions, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}
