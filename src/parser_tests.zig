const std = @import("std");
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;
const Parser = @import("./parser.zig").Parser;
const Node = @import("./ast_nodes.zig").Node;

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

// Tests
fn setupParserTest(tokens: []Token) !Parser {
    const allocator = std.testing.allocator;
    return try Parser.init(tokens, allocator);
}

fn destroyParser(parser: *Parser) void {
    parser.deinit();
}

// Helper type checking functions
fn isBinOp(node: *const Node) bool {
    return switch (node.*) {
        .binop => true,
        else => false,
    };
}

fn isNum(node: *const Node) bool {
    return switch (node.*) {
        .num => true,
        else => false,
    };
}

fn isVariable(node: *const Node) bool {
    return switch (node.*) {
        .variable => true,
        else => false,
    };
}

fn isFuncCall(node: *const Node) bool {
    return switch (node.*) {
        .func_call => true,
        else => false,
    };
}

fn isFuncDecl(node: *const Node) bool {
    return switch (node.*) {
        .func_decl => true,
        else => false,
    };
}

fn isProgram(node: *const Node) bool {
    return switch (node.*) {
        .program => true,
        else => false,
    };
}

fn isIfBlock(node: *const Node) bool {
    return (node.* == .if_block);
}

fn isElseBlock(node: *const Node) bool {
    return (node.* == .else_block);
}

test "parser init and cleanup" {
    var tokens = [_]Token{
        Token{ .type = TokenType.integer, .lexeme = "41", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    destroyParser(&parser);
}

test "parseExpr - simple arithmetic" {
    var tokens = [_]Token{
        Token{ .type = TokenType.integer, .lexeme = "3", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "4", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer destroyParser(&parser);

    const ast = try parser.parseExpr();

    try std.testing.expect(isBinOp(ast));
    try std.testing.expectEqual(ast.binop.token.type, TokenType.plus);

    try std.testing.expect(isNum(ast.binop.lhs));
    try std.testing.expectEqualStrings(ast.binop.lhs.*.num.token.lexeme.?, "3");
    try std.testing.expectEqual(ast.binop.lhs.*.num.value, 3);

    try std.testing.expect(isNum(ast.binop.rhs));
    try std.testing.expectEqual(ast.binop.rhs.*.num.value, 4);
}

test "parseExpr - arithmetic, parentheses" {
    var tokens = [_]Token{
        Token{ .type = TokenType.integer, .lexeme = "41", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "1", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.div, .lexeme = "/", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "9", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.mul, .lexeme = "*", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "3", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 1, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseExpr();
    try std.testing.expect(isBinOp(ast));
    try std.testing.expectEqual(ast.binop.token.type, TokenType.plus);

    try std.testing.expect(isNum(ast.binop.lhs));
    const node_41 = ast.binop.lhs.*.num;
    try std.testing.expectEqual(node_41.value, 41);

    try std.testing.expect(isBinOp(ast.binop.rhs));
    const node_binop_div = ast.binop.rhs.*.binop;
    try std.testing.expectEqual(node_binop_div.token.type, TokenType.div);

    const node_1 = node_binop_div.lhs.*.num;
    try std.testing.expectEqual(node_1.token.type, TokenType.integer);
    try std.testing.expectEqual(node_1.value, 1);

    const node_binop_mul = node_binop_div.rhs.*.binop;
    try std.testing.expectEqual(node_binop_mul.token.type, TokenType.mul);

    const node_9 = node_binop_mul.lhs.*.num;
    try std.testing.expectEqualStrings(node_9.token.lexeme.?, "9");
    try std.testing.expectEqual(node_9.token.type, TokenType.integer);
    try std.testing.expectEqual(node_9.value, 9);

    const node_3 = node_binop_mul.rhs.*.num;
    try std.testing.expectEqualStrings(node_3.token.lexeme.?, "3");
    try std.testing.expectEqual(node_3.token.type, TokenType.integer);
    try std.testing.expectEqual(node_3.value, 3);
}

test "parseExpr - arithmetic, parentheses, variable" {
    var tokens = [_]Token{
        Token{ .type = TokenType.integer, .lexeme = "1", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "a", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.mul, .lexeme = "*", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "9", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 1, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseExpr();
    try std.testing.expect(isBinOp(ast));
    try std.testing.expectEqual(ast.binop.token.type, TokenType.plus);

    try std.testing.expect(isNum(ast.binop.lhs));
    const num_1 = ast.binop.lhs.*.num;
    try std.testing.expectEqual(num_1.value, 1);

    try std.testing.expect(isBinOp(ast.binop.rhs));
    const binop_mul = ast.binop.rhs.*.binop;
    try std.testing.expectEqual(binop_mul.token.type, TokenType.mul);

    try std.testing.expect(isVariable(binop_mul.lhs));
    const var_a = binop_mul.lhs.*.variable;
    try std.testing.expectEqual(var_a.token.type, TokenType.id);
    try std.testing.expectEqualStrings(var_a.id, "a");

    const num_9 = binop_mul.rhs.*.num;
    try std.testing.expectEqual(num_9.token.type, TokenType.integer);
    try std.testing.expectEqual(num_9.value, 9);
}

test "parseExpr - arithmetic, parentheses, variable, comparison" {
    var tokens = [_]Token{
        Token{ .type = TokenType.integer, .lexeme = "1", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.plus, .lexeme = "+", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.gt, .lexeme = ">", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "9", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eq, .lexeme = "==", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "3", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
    };
    // // 1 [+] ( a > 9 == 3 )
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseExpr();
    try std.testing.expect(isBinOp(ast));
    try std.testing.expectEqual(ast.binop.token.type, TokenType.plus);

    // [1] + ( a > 9 == 3 )
    try std.testing.expect(isNum(ast.binop.lhs));
    const num_1 = ast.binop.lhs.*.num;
    try std.testing.expectEqual(num_1.value, 1);

    // 1 + ( a [>] 9 == 3 )
    try std.testing.expect(isBinOp(ast.binop.rhs));
    const binop_gt = ast.binop.rhs.*.binop;
    try std.testing.expectEqual(binop_gt.token.type, TokenType.gt);

    // 1 + ( [a] > 9 == 3 )
    try std.testing.expect(isVariable((binop_gt.lhs)));
    const var_a = binop_gt.lhs.*.variable;
    try std.testing.expectEqual(var_a.token.type, TokenType.id);
    try std.testing.expectEqualStrings(var_a.id, "a");

    // 1 + ( a > 9 [==] 3 )
    try std.testing.expect(isBinOp((binop_gt.rhs)));
    const binop_eq = binop_gt.rhs.*.binop;
    try std.testing.expectEqual(binop_eq.token.type, TokenType.eq);

    // 1 + ( a > [9] == 3 )
    try std.testing.expect(isNum((binop_eq.lhs)));
    const num_9 = binop_eq.lhs.*.num;
    try std.testing.expectEqual(num_9.token.type, TokenType.integer);
    try std.testing.expectEqual(num_9.value, 9);

    // 1 + ( a > 9 == [3] )
    try std.testing.expect(isNum((binop_eq.rhs)));
    const num_3 = binop_eq.rhs.*.num;
    try std.testing.expectEqual(num_3.token.type, TokenType.integer);
    try std.testing.expectEqual(num_3.value, 3);
}

test "parseAssignment" {
    var tokens = [_]Token{
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "2", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const ast = try parser.parseAssignment();
    try std.testing.expect(isBinOp(ast));
}

test "parseAssignment - arithmetic, variables, parentheses" {
    var tokens = [_]Token{
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "b", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.mul, .lexeme = "*", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "c", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.plus, .lexeme = "+", .line = 1, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "3", .line = 3, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 3, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseAssignment();
    try std.testing.expect(isBinOp(ast));
    try std.testing.expectEqual(ast.binop.token.type, TokenType.assign);

    try std.testing.expect(isVariable(ast.binop.lhs));
    const var_a = ast.binop.lhs.*.variable;
    try std.testing.expectEqual(var_a.token.type, TokenType.id);
    try std.testing.expectEqualStrings(var_a.token.lexeme.?, "a");
    try std.testing.expectEqualStrings(var_a.id, "a");

    try std.testing.expect(isBinOp(ast.binop.rhs));
    const binop_mul = ast.binop.rhs.*.binop;
    try std.testing.expectEqual(binop_mul.token.type, TokenType.mul);

    try std.testing.expect(isVariable(binop_mul.lhs));
    const var_b = binop_mul.lhs.*.variable;
    try std.testing.expectEqual(var_b.token.type, TokenType.id);
    try std.testing.expectEqualStrings(var_b.id, "b");

    try std.testing.expect(isBinOp(binop_mul.rhs));
    const binop_plus = binop_mul.rhs.*.binop;
    try std.testing.expectEqual(binop_plus.token.type, TokenType.plus);

    try std.testing.expect(isVariable(binop_plus.lhs));
    const var_c = binop_plus.lhs.*.variable;
    try std.testing.expectEqual(var_c.token.type, TokenType.id);

    try std.testing.expect(isNum(binop_plus.rhs));
    const num_3 = binop_plus.rhs.*.num;
    try std.testing.expectEqual(num_3.token.type, TokenType.integer);
    try std.testing.expectEqual(num_3.value, 3);
}

test "parseExpr - function call - 1 arg" {
    var tokens = [_]Token{ Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined }, Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined }, Token{ .type = TokenType.id, .lexeme = "b", .line = 1, .allocator = undefined }, Token{ .type = TokenType.rparen, .lexeme = ")", .line = 1, .allocator = undefined } };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseExpr();

    try std.testing.expect(isFuncCall(ast));
    try std.testing.expectEqualStrings(ast.func_call.id, "a");

    try std.testing.expect(isVariable(ast.func_call.*.args.items[0]));
    const expr_b = ast.func_call.*.args.items[0].*.variable;
    try std.testing.expectEqual(expr_b.token.type, TokenType.id);
    try std.testing.expectEqualStrings(expr_b.token.lexeme.?, "b");
    try std.testing.expectEqualStrings(expr_b.id, "b");
}

test "parseExpr - function call - 2 args" {
    var tokens = [_]Token{
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "b", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.comma, .lexeme = ",", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "c", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseExpr();
    try std.testing.expect(isFuncCall(ast));
    try std.testing.expectEqualStrings(ast.func_call.id, "a");

    try std.testing.expect(isVariable(ast.func_call.*.args.items[0]));
    const expr_b = ast.func_call.*.args.items[0].*.variable;
    try std.testing.expectEqual(expr_b.token.type, TokenType.id);
    try std.testing.expectEqualStrings(expr_b.token.lexeme.?, "b");
    try std.testing.expectEqualStrings(expr_b.id, "b");

    try std.testing.expect(isVariable(ast.func_call.*.args.items[1]));
    const expr_c = ast.func_call.*.args.items[1].*.variable;
    try std.testing.expectEqual(expr_c.token.type, TokenType.id);
    try std.testing.expectEqualStrings(expr_c.token.lexeme.?, "c");
    try std.testing.expectEqualStrings(expr_c.id, "c");
}

test "parseCompoundStatement - function call - 2 args - 2 exprs" {
    var tokens = [_]Token{
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "b", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();
    const ast = try parser.parseCompoundStatement();

    try std.testing.expect(isBinOp(ast.items[0]));
    const statement_1 = ast.items[0].binop.*;
    try std.testing.expectEqual(statement_1.token.type, TokenType.assign);

    try std.testing.expect(isVariable(statement_1.lhs));
    const var_a = statement_1.lhs.variable.*;
    try std.testing.expectEqualStrings(var_a.id, "a");

    try std.testing.expect(isVariable(statement_1.rhs));
    const var_b = statement_1.rhs.variable.*;
    try std.testing.expectEqualStrings(var_b.id, "b");
}

test "parseProgram - function main" {
    var tokens = [_]Token{
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 1);
    try std.testing.expectEqual(program.global_statements.items.len, 0);

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 0);
}

test "parseProgram - function main and function declaration" {
    var tokens = [_]Token{
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "myFunc", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 2);
    try std.testing.expectEqual(program.global_statements.items.len, 0);

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 0);

    const func_myFunc = program.functions.items[1].*.func_decl;
    try std.testing.expectEqualStrings(func_myFunc.id, "myFunc");
    try std.testing.expectEqual(func_myFunc.statements.items.len, 1);

    const myFunc_stmt = func_myFunc.statements.items;
    const myFunc_a_assign = myFunc_stmt[0];
    try std.testing.expect(isBinOp(myFunc_a_assign));
    try std.testing.expect(isVariable(myFunc_a_assign.binop.lhs));
    try std.testing.expect(isNum(myFunc_a_assign.binop.rhs));
}

test "parseProgram - main, func and global statements" {
    var tokens = [_]Token{
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "b", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.assign, .lexeme = "=", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "a", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 1);
    try std.testing.expectEqual(program.global_statements.items.len, 2);

    try std.testing.expect(isBinOp(program.global_statements.items[0]));
    const stmt_a_assign_42 = program.global_statements.items[0].*.binop;
    try std.testing.expect(isVariable(stmt_a_assign_42.lhs));
    const stmt_a_assign_42_var_a = stmt_a_assign_42.lhs.*.variable;
    try std.testing.expectEqualStrings(
        "a",
        stmt_a_assign_42_var_a.id,
    );

    try std.testing.expect(isNum(stmt_a_assign_42.rhs));

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 0);
}

test "parseProgram - main, if block with statement" {
    var tokens = [_]Token{
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.if_kw, .lexeme = "if", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "0", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };
    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 1);
    try std.testing.expectEqual(program.global_statements.items.len, 0);

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 1);

    const if_stmt = func_main.statements.items[0];
    try std.testing.expect(isIfBlock(if_stmt));
    try std.testing.expectEqualStrings("if", if_stmt.if_block.token.lexeme.?);
    const if_cond_expr = if_stmt.if_block.condition;
    try std.testing.expect(isNum(if_cond_expr));
    const n = if_cond_expr.num;
    try std.testing.expectEqual(42, n.value);

    try std.testing.expectEqual(1, if_stmt.if_block.statements.items.len);
    const if_block_stmt = if_stmt.if_block.statements.items[0];
    try std.testing.expect(isNum(if_block_stmt));
    try std.testing.expectEqual(0, if_block_stmt.num.value);
}

test "parseProgram - main, if block with statement and else block" {
    var tokens = [_]Token{
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },

        // If statement
        Token{ .type = TokenType.if_kw, .lexeme = "if", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "0", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        // Else block
        Token{ .type = TokenType.else_kw, .lexeme = "else", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "1", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };

    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 1);
    try std.testing.expectEqual(program.global_statements.items.len, 0);

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 1);

    const if_stmt = func_main.statements.items[0];
    try std.testing.expect(isIfBlock(if_stmt));
    try std.testing.expectEqualStrings("if", if_stmt.if_block.token.lexeme.?);

    // Test if condition
    const if_condition = if_stmt.if_block.condition;
    try std.testing.expect(isNum(if_condition));
    try std.testing.expectEqual(42, if_condition.num.value);

    // Test if block statements
    try std.testing.expectEqual(1, if_stmt.if_block.statements.items.len);
    const if_block_stmt = if_stmt.if_block.statements.items[0];
    try std.testing.expect(isNum(if_block_stmt));
    try std.testing.expectEqual(0, if_block_stmt.num.value);

    // Test else block exists
    try std.testing.expect(if_stmt.if_block.next_else != null);
    const else_block = if_stmt.if_block.next_else.?;

    // Test else block has no condition
    try std.testing.expect(else_block.condition == null);

    // // Test else block statements
    try std.testing.expectEqual(1, else_block.statements.items.len);
    const else_block_stmt = else_block.statements.items[0];
    try std.testing.expect(isNum(else_block_stmt));
    try std.testing.expectEqual(1, else_block_stmt.num.value);

    // Test that there are no more else blocks
    try std.testing.expect(else_block.next_else == null);
}

test "parseProgram - main, if block with statement and else if block" {
    var tokens = [_]Token{
        // `function main ( ) {`
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },

        // `if ( 0 ) { 42 ; }`
        Token{ .type = TokenType.if_kw, .lexeme = "if", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "0", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        // `else if ( 1 ) { 42 ; }`
        Token{ .type = TokenType.else_kw, .lexeme = "else", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.if_kw, .lexeme = "if", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "1", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "42", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.semi, .lexeme = ";", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        // "}"
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };

    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 1);
    try std.testing.expectEqual(program.global_statements.items.len, 0);

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 1);

    const if_stmt = func_main.statements.items[0];
    try std.testing.expect(isIfBlock(if_stmt));
    try std.testing.expectEqualStrings("if", if_stmt.if_block.token.lexeme.?);

    // Test if condition
    const if_condition = if_stmt.if_block.condition;
    try std.testing.expect(isNum(if_condition));
    try std.testing.expectEqual(0, if_condition.num.value);

    // Test if block statements
    try std.testing.expectEqual(1, if_stmt.if_block.statements.items.len);
    const if_block_stmt = if_stmt.if_block.statements.items[0];
    try std.testing.expect(isNum(if_block_stmt));
    try std.testing.expectEqual(42, if_block_stmt.num.value);

    // Test else if block exists
    try std.testing.expect(if_stmt.if_block.next_else != null);
    const else_block = if_stmt.if_block.next_else.?;

    // Test else block condition
    try std.testing.expect(else_block.condition != null);
    try std.testing.expect(isNum(else_block.condition.?));
    const else_if_block_expr_stmt = else_block.condition.?.num;
    try std.testing.expectEqual(1, else_if_block_expr_stmt.value);

    // Test else block statements
    try std.testing.expectEqual(1, else_block.statements.items.len);
    const else_block_stmt = else_block.statements.items[0];
    try std.testing.expect(isNum(else_block_stmt));
    try std.testing.expectEqual(42, else_block_stmt.num.value);

    // Test that there are no more else blocks
    try std.testing.expect(else_block.next_else == null);
}

test "parseProgram - main, if block with statement, else if block and else block" {
    var tokens = [_]Token{
        // `function main ( ) {`
        Token{ .type = TokenType.function_kw, .lexeme = "function", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.id, .lexeme = "main", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },

        // `if ( 0 ) { 42 ; }`
        Token{ .type = TokenType.if_kw, .lexeme = "if", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "0", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        // `else if ( 1 ) {  }`
        Token{ .type = TokenType.else_kw, .lexeme = "else", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.if_kw, .lexeme = "if", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lparen, .lexeme = "(", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.integer, .lexeme = "1", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rparen, .lexeme = ")", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        // `else { }`
        Token{ .type = TokenType.else_kw, .lexeme = "else", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.lbrace, .lexeme = "{", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },

        // `}`
        Token{ .type = TokenType.rbrace, .lexeme = "}", .line = 0, .allocator = undefined },
        Token{ .type = TokenType.eof, .lexeme = "", .line = 0, .allocator = undefined },
    };

    var parser = try setupParserTest(&tokens);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(program.functions.items.len, 1);
    try std.testing.expectEqual(program.global_statements.items.len, 0);

    const func_main = program.functions.items[0].*.func_decl;
    try std.testing.expectEqualStrings(func_main.id, "main");
    try std.testing.expectEqual(func_main.statements.items.len, 1);

    const if_stmt = func_main.statements.items[0];
    try std.testing.expect(isIfBlock(if_stmt));
    try std.testing.expectEqualStrings("if", if_stmt.if_block.token.lexeme.?);

    // Test if condition
    const if_condition = if_stmt.if_block.condition;
    try std.testing.expect(isNum(if_condition));
    try std.testing.expectEqual(0, if_condition.num.value);

    // Test NO if block statements
    try std.testing.expectEqual(0, if_stmt.if_block.statements.items.len);

    // Test else if block exists
    try std.testing.expect(if_stmt.if_block.next_else != null);
    const else_block = if_stmt.if_block.next_else.?;

    // Test else if block condition
    try std.testing.expect(else_block.condition != null);
    try std.testing.expect(isNum(else_block.condition.?));
    const else_if_block_expr_stmt = else_block.condition.?.num;
    try std.testing.expectEqual(1, else_if_block_expr_stmt.value);

    // // Test NO else if block statements
    try std.testing.expectEqual(0, else_block.statements.items.len);

    // Test last else block
    try std.testing.expect(else_block.next_else != null);
    const next_else = else_block.next_else.?;
    try std.testing.expectEqual(0, next_else.statements.items.len);
    try std.testing.expectEqual(null, next_else.condition);
}
