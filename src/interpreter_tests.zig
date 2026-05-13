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
const Slice = @import("./ast_nodes.zig").Slice;
const Array = @import("./ast_nodes.zig").Array;

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

test "string slice in-bounds returns correct substring" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };

    var s_hello = try String.initFromSlice(eof, "hello", allocator);
    defer s_hello.deinit();
    var n_hello = Node{ .string = s_hello };
    var n_lo = Node{ .num = Num{ .token = eof, .value = 1 } };
    var n_hi = Node{ .num = Num{ .token = eof, .value = 3 } };
    var slice_node = Node{ .slice = Slice{ .token = eof, .target = &n_hello, .lo = &n_lo, .hi = &n_hi } };

    var s_el = try String.initFromSlice(eof, "el", allocator);
    defer s_el.deinit();
    var n_el = Node{ .string = s_el };
    var cmp = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &slice_node, .rhs = &n_el } };
    var ret = Node{ .ret = ReturnStatement{ .expr = &cmp } };

    var stmts = std.ArrayList(*Node){};
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

    var prog = Program{ .id = "", .functions = funcs, .global_statements = global };
    var interp = try Interpreter.init(&prog, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}

test "string slice lo > hi returns IndexOutOfBounds" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };

    var s_hello2 = try String.initFromSlice(eof, "hello", allocator);
    defer s_hello2.deinit();
    var n_hello2 = Node{ .string = s_hello2 };
    var n_lo2 = Node{ .num = Num{ .token = eof, .value = 3 } };
    var n_hi2 = Node{ .num = Num{ .token = eof, .value = 1 } };
    var slice2 = Node{ .slice = Slice{ .token = eof, .target = &n_hello2, .lo = &n_lo2, .hi = &n_hi2 } };
    var ret2 = Node{ .ret = ReturnStatement{ .expr = &slice2 } };

    var stmts2 = std.ArrayList(*Node){};
    try stmts2.append(allocator, &ret2);
    defer stmts2.deinit(allocator);
    var args2 = std.ArrayList(*Node){};
    defer args2.deinit(allocator);
    var func2 = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args2, .statements = stmts2 } };
    var funcs2 = std.ArrayList(*Node){};
    try funcs2.append(allocator, &func2);
    defer funcs2.deinit(allocator);
    var global2 = std.ArrayList(*Node){};
    defer global2.deinit(allocator);

    var prog2 = Program{ .id = "", .functions = funcs2, .global_statements = global2 };
    var interp2 = try Interpreter.init(&prog2, allocator);
    defer interp2.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp2.interpret());
}

test "string slice hi > len returns IndexOutOfBounds" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };

    var s_hi3 = try String.initFromSlice(eof, "hi", allocator);
    defer s_hi3.deinit();
    var n_str3 = Node{ .string = s_hi3 };
    var n_lo3 = Node{ .num = Num{ .token = eof, .value = 0 } };
    var n_hi3 = Node{ .num = Num{ .token = eof, .value = 10 } };
    var slice3 = Node{ .slice = Slice{ .token = eof, .target = &n_str3, .lo = &n_lo3, .hi = &n_hi3 } };
    var ret3 = Node{ .ret = ReturnStatement{ .expr = &slice3 } };

    var stmts3 = std.ArrayList(*Node){};
    try stmts3.append(allocator, &ret3);
    defer stmts3.deinit(allocator);
    var args3 = std.ArrayList(*Node){};
    defer args3.deinit(allocator);
    var func3 = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args3, .statements = stmts3 } };
    var funcs3 = std.ArrayList(*Node){};
    try funcs3.append(allocator, &func3);
    defer funcs3.deinit(allocator);
    var global3 = std.ArrayList(*Node){};
    defer global3.deinit(allocator);

    var prog3 = Program{ .id = "", .functions = funcs3, .global_statements = global3 };
    var interp3 = try Interpreter.init(&prog3, allocator);
    defer interp3.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp3.interpret());
}

test "empty array literal evaluates without error" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    const empty_elems = std.ArrayList(*Node){};
    var n_arr = Node{ .array = Array{ .token = eof, .elements = empty_elems } };
    var var_a = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var assign_a = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &var_a, .rhs = &n_arr } };
    var n_zero = Node{ .num = Num{ .token = eof, .value = 0 } };
    var ret_zero = Node{ .ret = ReturnStatement{ .expr = &n_zero } };

    var stmts_arr = std.ArrayList(*Node){};
    try stmts_arr.append(allocator, &assign_a);
    try stmts_arr.append(allocator, &ret_zero);
    defer stmts_arr.deinit(allocator);
    var args_arr = std.ArrayList(*Node){};
    defer args_arr.deinit(allocator);
    var func_arr = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args_arr, .statements = stmts_arr } };
    var funcs_arr = std.ArrayList(*Node){};
    try funcs_arr.append(allocator, &func_arr);
    defer funcs_arr.deinit(allocator);
    var global_arr = std.ArrayList(*Node){};
    defer global_arr.deinit(allocator);

    var prog_arr = Program{ .id = "", .functions = funcs_arr, .global_statements = global_arr };
    var interp_arr = try Interpreter.init(&prog_arr, allocator);
    defer interp_arr.deinit();
    try std.testing.expectEqual(@as(i64, 0), try interp_arr.interpret());
}

test "array literal with elements evaluates without error" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    var e1 = Node{ .num = Num{ .token = eof, .value = 10 } };
    var e2 = Node{ .num = Num{ .token = eof, .value = 20 } };
    var e3 = Node{ .num = Num{ .token = eof, .value = 30 } };
    var arr_elems = std.ArrayList(*Node){};
    try arr_elems.append(allocator, &e1);
    try arr_elems.append(allocator, &e2);
    try arr_elems.append(allocator, &e3);
    defer arr_elems.deinit(allocator);

    var n_arr3 = Node{ .array = Array{ .token = eof, .elements = arr_elems } };
    var var_a3 = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var assign_a3 = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &var_a3, .rhs = &n_arr3 } };
    var n_zero3 = Node{ .num = Num{ .token = eof, .value = 0 } };
    var ret_zero3 = Node{ .ret = ReturnStatement{ .expr = &n_zero3 } };

    var stmts3e = std.ArrayList(*Node){};
    try stmts3e.append(allocator, &assign_a3);
    try stmts3e.append(allocator, &ret_zero3);
    defer stmts3e.deinit(allocator);
    var args3e = std.ArrayList(*Node){};
    defer args3e.deinit(allocator);
    var func3e = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args3e, .statements = stmts3e } };
    var funcs3e = std.ArrayList(*Node){};
    try funcs3e.append(allocator, &func3e);
    defer funcs3e.deinit(allocator);
    var global3e = std.ArrayList(*Node){};
    defer global3e.deinit(allocator);

    var prog3e = Program{ .id = "", .functions = funcs3e, .global_statements = global3e };
    var interp3e = try Interpreter.init(&prog3e, allocator);
    defer interp3e.deinit();
    try std.testing.expectEqual(@as(i64, 0), try interp3e.interpret());
}

test "array subscript read returns correct element" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };

    // a = [10, 20, 30]; return a[1] == 20;
    var e10 = Node{ .num = Num{ .token = eof, .value = 10 } };
    var e20 = Node{ .num = Num{ .token = eof, .value = 20 } };
    var e30 = Node{ .num = Num{ .token = eof, .value = 30 } };
    var rd_elems = std.ArrayList(*Node){};
    try rd_elems.append(allocator, &e10);
    try rd_elems.append(allocator, &e20);
    try rd_elems.append(allocator, &e30);
    defer rd_elems.deinit(allocator);

    var rd_arr = Node{ .array = Array{ .token = eof, .elements = rd_elems } };
    var rd_var = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var rd_assign = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &rd_var, .rhs = &rd_arr } };

    var rd_idx = Node{ .num = Num{ .token = eof, .value = 1 } };
    var rd_var2 = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var rd_sub = Node{ .subscript = Subscript{ .token = eof, .target = &rd_var2, .index = &rd_idx } };
    var rd_expected = Node{ .num = Num{ .token = eof, .value = 20 } };
    var rd_cmp = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &rd_sub, .rhs = &rd_expected } };
    var rd_ret = Node{ .ret = ReturnStatement{ .expr = &rd_cmp } };

    var rd_stmts = std.ArrayList(*Node){};
    try rd_stmts.append(allocator, &rd_assign);
    try rd_stmts.append(allocator, &rd_ret);
    defer rd_stmts.deinit(allocator);
    var rd_args = std.ArrayList(*Node){};
    defer rd_args.deinit(allocator);
    var rd_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = rd_args, .statements = rd_stmts } };
    var rd_funcs = std.ArrayList(*Node){};
    try rd_funcs.append(allocator, &rd_func);
    defer rd_funcs.deinit(allocator);
    var rd_global = std.ArrayList(*Node){};
    defer rd_global.deinit(allocator);

    var rd_prog = Program{ .id = "", .functions = rd_funcs, .global_statements = rd_global };
    var rd_interp = try Interpreter.init(&rd_prog, allocator);
    defer rd_interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try rd_interp.interpret());
}

test "array subscript write mutates in place" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };
    const eq_tok = Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = allocator };

    // a = [1, 2, 3]; a[0] = 99; return a[0] == 99;
    var w1 = Node{ .num = Num{ .token = eof, .value = 1 } };
    var w2 = Node{ .num = Num{ .token = eof, .value = 2 } };
    var w3 = Node{ .num = Num{ .token = eof, .value = 3 } };
    var wr_elems = std.ArrayList(*Node){};
    try wr_elems.append(allocator, &w1);
    try wr_elems.append(allocator, &w2);
    try wr_elems.append(allocator, &w3);
    defer wr_elems.deinit(allocator);

    var wr_arr = Node{ .array = Array{ .token = eof, .elements = wr_elems } };
    var wr_var = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var wr_assign = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &wr_var, .rhs = &wr_arr } };

    var wr_idx0a = Node{ .num = Num{ .token = eof, .value = 0 } };
    var wr_var2 = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var wr_sub_lhs = Node{ .subscript = Subscript{ .token = eof, .target = &wr_var2, .index = &wr_idx0a } };
    var wr_99 = Node{ .num = Num{ .token = eof, .value = 99 } };
    var wr_sub_assign = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &wr_sub_lhs, .rhs = &wr_99 } };

    var wr_idx0b = Node{ .num = Num{ .token = eof, .value = 0 } };
    var wr_var3 = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var wr_sub_read = Node{ .subscript = Subscript{ .token = eof, .target = &wr_var3, .index = &wr_idx0b } };
    var wr_expected = Node{ .num = Num{ .token = eof, .value = 99 } };
    var wr_cmp = Node{ .binop = BinOp{ .token = eq_tok, .lhs = &wr_sub_read, .rhs = &wr_expected } };
    var wr_ret = Node{ .ret = ReturnStatement{ .expr = &wr_cmp } };

    var wr_stmts = std.ArrayList(*Node){};
    try wr_stmts.append(allocator, &wr_assign);
    try wr_stmts.append(allocator, &wr_sub_assign);
    try wr_stmts.append(allocator, &wr_ret);
    defer wr_stmts.deinit(allocator);
    var wr_args = std.ArrayList(*Node){};
    defer wr_args.deinit(allocator);
    var wr_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = wr_args, .statements = wr_stmts } };
    var wr_funcs = std.ArrayList(*Node){};
    try wr_funcs.append(allocator, &wr_func);
    defer wr_funcs.deinit(allocator);
    var wr_global = std.ArrayList(*Node){};
    defer wr_global.deinit(allocator);

    var wr_prog = Program{ .id = "", .functions = wr_funcs, .global_statements = wr_global };
    var wr_interp = try Interpreter.init(&wr_prog, allocator);
    defer wr_interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try wr_interp.interpret());
}

test "array subscript read out of bounds returns error" {
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    var ob1 = Node{ .num = Num{ .token = eof, .value = 1 } };
    var ob_elems = std.ArrayList(*Node){};
    try ob_elems.append(allocator, &ob1);
    defer ob_elems.deinit(allocator);
    var ob_arr = Node{ .array = Array{ .token = eof, .elements = ob_elems } };
    var ob_var = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var ob_assign = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &ob_var, .rhs = &ob_arr } };

    var ob_idx = Node{ .num = Num{ .token = eof, .value = 9 } };
    var ob_var2 = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var ob_sub = Node{ .subscript = Subscript{ .token = eof, .target = &ob_var2, .index = &ob_idx } };
    var ob_ret = Node{ .ret = ReturnStatement{ .expr = &ob_sub } };

    var ob_stmts = std.ArrayList(*Node){};
    try ob_stmts.append(allocator, &ob_assign);
    try ob_stmts.append(allocator, &ob_ret);
    defer ob_stmts.deinit(allocator);
    var ob_args = std.ArrayList(*Node){};
    defer ob_args.deinit(allocator);
    var ob_func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = ob_args, .statements = ob_stmts } };
    var ob_funcs = std.ArrayList(*Node){};
    try ob_funcs.append(allocator, &ob_func);
    defer ob_funcs.deinit(allocator);
    var ob_global = std.ArrayList(*Node){};
    defer ob_global.deinit(allocator);

    var ob_prog = Program{ .id = "", .functions = ob_funcs, .global_statements = ob_global };
    var ob_interp = try Interpreter.init(&ob_prog, allocator);
    defer ob_interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try ob_interp.interpret());
}

test "array subscript write out of bounds returns error" {
    // main() { a = [1]; a[9] = 99; return 42; }  → exits 1 (OOB on write)
    const allocator = std.testing.allocator;
    const eof = Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = allocator };
    const assign_tok = Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = allocator };

    // a = [1]
    var elem1 = Node{ .num = Num{ .token = eof, .value = 1 } };
    var elems = std.ArrayList(*Node){};
    try elems.append(allocator, &elem1);
    defer elems.deinit(allocator);
    var arr_node = Node{ .array = Array{ .token = eof, .elements = elems } };
    var a_var = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var a_assign = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &a_var, .rhs = &arr_node } };

    // a[9] = 99  →  binop{ lhs=subscript{a,9}, rhs=99 }
    var idx_node = Node{ .num = Num{ .token = eof, .value = 9 } };
    var a_var2 = Node{ .variable = Variable{ .id = "a", .token = eof } };
    var sub_node = Node{ .subscript = Subscript{ .token = eof, .target = &a_var2, .index = &idx_node } };
    var rhs_node = Node{ .num = Num{ .token = eof, .value = 99 } };
    var write_node = Node{ .binop = BinOp{ .token = assign_tok, .lhs = &sub_node, .rhs = &rhs_node } };

    // return 42
    var ret_val = Node{ .num = Num{ .token = eof, .value = 42 } };
    var ret_node = Node{ .ret = ReturnStatement{ .expr = &ret_val } };

    var stmts = std.ArrayList(*Node){};
    try stmts.append(allocator, &a_assign);
    try stmts.append(allocator, &write_node);
    try stmts.append(allocator, &ret_node);
    defer stmts.deinit(allocator);
    var args = std.ArrayList(*Node){};
    defer args.deinit(allocator);
    var func = Node{ .func_decl = FunctionDecl{ .id = "main", .args = args, .statements = stmts } };
    var funcs = std.ArrayList(*Node){};
    try funcs.append(allocator, &func);
    defer funcs.deinit(allocator);
    var global = std.ArrayList(*Node){};
    defer global.deinit(allocator);

    var prog = Program{ .id = "", .functions = funcs, .global_statements = global };
    var interp = try Interpreter.init(&prog, allocator);
    defer interp.deinit();
    try std.testing.expectEqual(@as(i64, 1), try interp.interpret());
}
