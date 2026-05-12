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
const Subscript = @import("./ast_nodes.zig").Subscript;

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

test "string concatenation: hello + world" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const plus_tok = Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    var s_hello = try String.initFromSlice(eof, "hello", allocator);
    defer s_hello.deinit();
    var s_world = try String.initFromSlice(eof, " world", allocator);
    defer s_world.deinit();
    var s_expected = try String.initFromSlice(eof, "hello world", allocator);
    defer s_expected.deinit();

    var n_hello = Node{ .string = s_hello };
    var n_world = Node{ .string = s_world };
    var n_expected = Node{ .string = s_expected };

    var concat = Node{ .binop = BinOp{ .token = plus_tok, .lhs = &n_hello, .rhs = &n_world } };
    var var_c = Node{ .variable = Variable{ .id = "c", .token = eof } };
    var assign_c = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &var_c, .rhs = &concat } };
    var var_c2 = Node{ .variable = Variable{ .id = "c", .token = eof } };
    var cmp = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &var_c2, .rhs = &n_expected } };
    var ret_sc = Node{ .ret = ReturnStatement{ .expr = &cmp } };

    var sc_stmts = std.ArrayList(*Node){};
    try sc_stmts.append(allocator, &assign_c);
    try sc_stmts.append(allocator, &ret_sc);
    defer sc_stmts.deinit(allocator);
    var sc_args = std.ArrayList(*Node){};
    defer sc_args.deinit(allocator);
    var sc_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = sc_args, .statements = sc_stmts } };

    var sc_funcs = std.ArrayList(*Node){};
    try sc_funcs.append(allocator, &sc_func);
    defer sc_funcs.deinit(allocator);
    var sc_global = std.ArrayList(*Node){};
    defer sc_global.deinit(allocator);

    var sc_ast = Program{ .id = "", .functions = sc_funcs, .global_statements = sc_global };
    var sc_interp = try Interpreter.init(&sc_ast, allocator);
    defer sc_interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try sc_interp.interpret());
}

test "string + integer returns MismatchingBinOpTypes" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const plus_tok = Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = allocator };

    var s_hi = try String.initFromSlice(eof, "hi", allocator);
    defer s_hi.deinit();
    var n_str = Node{ .string = s_hi };
    var n_num = Node{ .num = Num{ .token = eof, .value = 1 } };
    var bad_add = Node{ .binop = BinOp{ .token = plus_tok, .lhs = &n_str, .rhs = &n_num } };
    var err_ret = Node{ .ret = ReturnStatement{ .expr = &bad_add } };

    var err_stmts = std.ArrayList(*Node){};
    try err_stmts.append(allocator, &err_ret);
    defer err_stmts.deinit(allocator);
    var err_args = std.ArrayList(*Node){};
    defer err_args.deinit(allocator);
    var err_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = err_args, .statements = err_stmts } };

    var err_funcs = std.ArrayList(*Node){};
    try err_funcs.append(allocator, &err_func);
    defer err_funcs.deinit(allocator);
    var err_global = std.ArrayList(*Node){};
    defer err_global.deinit(allocator);

    var err_ast = Program{ .id = "", .functions = err_funcs, .global_statements = err_global };
    var err_interp = try Interpreter.init(&err_ast, allocator);
    defer err_interp.deinit();
    try std.testing.expectError(error.MismatchingBinOpTypes, err_interp.interpret());
}

test "string subscript in-bounds returns single char" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };

    // s = "hello"
    var s_hello = try String.initFromSlice(eof, "hello", allocator);
    defer s_hello.deinit();
    var n_hello = Node{ .string = s_hello };
    var var_s = Node{ .variable = Variable{ .id = "s", .token = eof } };
    var assign_s = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &var_s, .rhs = &n_hello } };

    // ch = s[1]
    var n_one = Node{ .num = Num{ .token = eof, .value = 1 } };
    var var_s2 = Node{ .variable = Variable{ .id = "s", .token = eof } };
    var sub = Node{ .subscript = Subscript{ .token = eof, .target = &var_s2, .index = &n_one } };
    var var_ch = Node{ .variable = Variable{ .id = "ch", .token = eof } };
    var assign_ch = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &var_ch, .rhs = &sub } };

    // return ch == "e"
    var s_e = try String.initFromSlice(eof, "e", allocator);
    defer s_e.deinit();
    var n_e = Node{ .string = s_e };
    var var_ch2 = Node{ .variable = Variable{ .id = "ch", .token = eof } };
    var cmp = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &var_ch2, .rhs = &n_e } };
    var ret = Node{ .ret = ReturnStatement{ .expr = &cmp } };

    var stmts = std.ArrayList(*Node){};
    try stmts.append(allocator, &assign_s);
    try stmts.append(allocator, &assign_ch);
    try stmts.append(allocator, &ret);
    defer stmts.deinit(allocator);
    var args = std.ArrayList(*Node){};
    defer args.deinit(allocator);
    var func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args, .statements = stmts } };
    var funcs = std.ArrayList(*Node){};
    try funcs.append(allocator, &func);
    defer funcs.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = funcs, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}

test "string subscript out-of-bounds returns IndexOutOfBounds" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    var s_hi = try String.initFromSlice(eof, "hi", allocator);
    defer s_hi.deinit();
    var n_hi = Node{ .string = s_hi };
    var var_s = Node{ .variable = Variable{ .id = "s", .token = eof } };
    var assign_s = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &var_s, .rhs = &n_hi } };

    var n_nine = Node{ .num = Num{ .token = eof, .value = 9 } };
    var var_s2 = Node{ .variable = Variable{ .id = "s", .token = eof } };
    var sub = Node{ .subscript = Subscript{ .token = eof, .target = &var_s2, .index = &n_nine } };
    var ret = Node{ .ret = ReturnStatement{ .expr = &sub } };

    var stmts = std.ArrayList(*Node){};
    try stmts.append(allocator, &assign_s);
    try stmts.append(allocator, &ret);
    defer stmts.deinit(allocator);
    var args = std.ArrayList(*Node){};
    defer args.deinit(allocator);
    var func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args, .statements = stmts } };
    var funcs = std.ArrayList(*Node){};
    try funcs.append(allocator, &func);
    defer funcs.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var ast = Program{ .id = "", .functions = funcs, .global_statements = global };
    var interp = try Interpreter.init(&ast, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}
