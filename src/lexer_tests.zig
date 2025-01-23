const std = @import("std");
const Lexer = @import("./lexer.zig").Lexer;
const reserved = @import("./lex_constants.zig").reserved;
const TokenType = @import("./tokens.zig").TokenType;
const Error = @import("./lexer.zig").Error;

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;
const expectError = std.testing.expectError;

fn setupLextStringTest(s: []const u8) anyerror!Lexer {
    const allocator = std.testing.allocator;
    var lexer = try Lexer.init(s, allocator);
    try lexer.tokenize();
    return lexer;
}

fn teardownLexStringTest(lexer: *Lexer) void {
    lexer.deinit();
}

test "lexer init and cleanup" {
    var lexer = try setupLextStringTest("function return if else while for break continue ");
    try lexer.tokenize();
    teardownLexStringTest(&lexer);
}

test "lexer reserved keywords" {
    var lexer = try setupLextStringTest("function return if else while for break continue ");
    try lexer.tokenize();
    const tokens = try lexer.getTokens();
    // function
    const tok_0 = try tokens.items[0].getToken();
    const tok_0_lexeme = try tokens.items[0].getLexeme();
    try expect(std.mem.eql(u8, tok_0_lexeme, reserved._function));
    try expect(tok_0.type == TokenType.function_kw);
    try expect(tok_0.line == 0);

    // return
    const tok_1 = try tokens.items[1].getToken();
    const tok_1_lexeme = try tok_1.getLexeme();
    try expect(std.mem.eql(u8, tok_1_lexeme, reserved._return));
    try expect(tok_1.type == TokenType.return_kw);
    try expect(tok_1.line == 0);

    // if
    const tok_2 = try tokens.items[2].getToken();
    const tok_2_lexeme = try tok_2.getLexeme();
    try expect(std.mem.eql(u8, tok_2_lexeme, reserved._if));
    try expect(tok_2.type == TokenType.if_kw);
    try expect(tok_2.line == 0);

    // else
    const tok_3 = try tokens.items[3].getToken();
    const tok_3_lexeme = try tok_3.getLexeme();
    try expect(std.mem.eql(u8, tok_3_lexeme, reserved._else));
    try expect(tok_3.type == TokenType.else_kw);
    try expect(tok_3.line == 0);

    // while
    const tok_4 = try tokens.items[4].getToken();
    const tok_4_lexeme = try tok_4.getLexeme();
    try expect(std.mem.eql(u8, tok_4_lexeme, reserved._while));
    try expect(tok_4.type == TokenType.while_kw);
    try expect(tok_4.line == 0);

    // for
    const tok_5 = try tokens.items[5].getToken();
    const tok_5_lexeme = try tok_5.getLexeme();
    try expect(std.mem.eql(u8, tok_5_lexeme, reserved._for));
    try expect(tok_5.type == TokenType.for_kw);
    try expect(tok_5.line == 0);

    // break
    const tok_6 = try tokens.items[6].getToken();
    const tok_6_lexeme = try tok_6.getLexeme();
    try expect(std.mem.eql(u8, tok_6_lexeme, reserved._break));
    try expect(tok_6.type == TokenType.break_kw);
    try expect(tok_6.line == 0);

    // continue
    const tok_7 = try tokens.items[7].getToken();
    const tok_7_lexeme = try tok_7.getLexeme();
    try expect(std.mem.eql(u8, tok_7_lexeme, reserved._continue));
    try expect(tok_7.type == TokenType.continue_kw);
    try expect(tok_7.line == 0);

    teardownLexStringTest(&lexer);
}

test "lexer operators - assignments and math" {
    var lexer = try setupLextStringTest("= + - * / % ");
    try lexer.tokenize();
    const tokens = try lexer.getTokens();

    try expect(tokens.items[0].type == TokenType.assign);
    try expect(tokens.items[1].type == TokenType.plus);
    try expect(tokens.items[2].type == TokenType.minus);
    try expect(tokens.items[3].type == TokenType.mul);
    try expect(tokens.items[4].type == TokenType.div);
    try expect(tokens.items[5].type == TokenType.mod);

    teardownLexStringTest(&lexer);
}

test "lexer delimiters" {
    var lexer = try setupLextStringTest("( )\n{}\n[] ,;");
    try lexer.tokenize();
    const tokens = try lexer.getTokens();

    try expect(tokens.items[0].type == TokenType.lparen);
    try expect(tokens.items[1].type == TokenType.rparen);
    try expect(tokens.items[2].type == TokenType.lbrace);
    try expect(tokens.items[3].type == TokenType.rbrace);
    try expect(tokens.items[4].type == TokenType.lbrack);
    try expect(tokens.items[5].type == TokenType.rbrack);
    try expect(tokens.items[6].type == TokenType.comma);
    try expect(tokens.items[7].type == TokenType.semi);

    teardownLexStringTest(&lexer);
}

test "lexer comparisons" {
    var lexer = try setupLextStringTest("< <= == >= >");
    try lexer.tokenize();
    const tokens = try lexer.getTokens();

    try expect(tokens.items[0].type == TokenType.lt);
    try expect(tokens.items[1].type == TokenType.le);
    try expect(tokens.items[2].type == TokenType.eq);
    try expect(tokens.items[3].type == TokenType.ge);
    try expect(tokens.items[4].type == TokenType.gt);

    teardownLexStringTest(&lexer);
}

test "lexer values" {
    var lexer = try setupLextStringTest("\"hello\" 42 myVariable");
    try lexer.tokenize();
    const tokens = try lexer.getTokens();

    // String token
    const tok_str = try tokens.items[0].getToken();
    const tok_str_lexeme = try tok_str.getLexeme();
    try expect(tok_str.type == TokenType.string);
    try expectEqualStrings(tok_str_lexeme, "\"hello\"");

    // Integer token
    const tok_int = try tokens.items[1].getToken();
    const tok_int_lexeme = try tok_int.getLexeme();
    try expect(tok_int.type == TokenType.integer);
    try expect(std.mem.eql(u8, tok_int_lexeme, "42"));

    // Identifier token
    const tok_id = try tokens.items[2].getToken();
    const tok_id_lexeme = try tok_id.getLexeme();
    try expect(tok_id.type == TokenType.id);
    try expect(std.mem.eql(u8, tok_id_lexeme, "myVariable"));

    teardownLexStringTest(&lexer);
}

test "lexer line counting" {
    var lexer = try setupLextStringTest("0\n1\n2\n3");
    try lexer.tokenize();
    defer teardownLexStringTest(&lexer);

    const tokens = try lexer.getTokens();

    try expect(tokens.items[0].line == 0);
    try expect(tokens.items[1].line == 1);
    try expect(tokens.items[2].line == 2);
    try expect(tokens.items[3].line == 3);
}

test "lexer mixed expression" {
    var lexer = try setupLextStringTest("function foobar(foo, bar) { return foo + bar; }");
    try lexer.tokenize();
    defer teardownLexStringTest(&lexer);

    const tokens = try lexer.getTokens();

    try expect(tokens.items[0].type == TokenType.function_kw);
    try expect(tokens.items[1].type == TokenType.id);
    try expect(tokens.items[2].type == TokenType.lparen);
    try expect(tokens.items[3].type == TokenType.id);
    try expect(tokens.items[4].type == TokenType.comma);
    try expect(tokens.items[5].type == TokenType.id);
    try expect(tokens.items[6].type == TokenType.rparen);
    try expect(tokens.items[7].type == TokenType.lbrace);
    try expect(tokens.items[8].type == TokenType.return_kw);
    try expect(tokens.items[9].type == TokenType.id);
    try expect(tokens.items[10].type == TokenType.plus);
    try expect(tokens.items[11].type == TokenType.id);
    try expect(tokens.items[12].type == TokenType.semi);
    try expect(tokens.items[13].type == TokenType.rbrace);
}

test "lexer error cases" {
    const allocator = std.testing.allocator;

    // Test invalid character
    var lexer_invalid_1 = try Lexer.init("@", allocator);
    errdefer allocator.free(lexer_invalid_1.source.?);
    try expectError(Error.BadToken, lexer_invalid_1.nextToken());

    // Test unterminated string
    var lexer_invalid_2 = try Lexer.init("\"hello", allocator);
    errdefer allocator.free(lexer_invalid_2.source.?);
    try expectError(Error.UnterminatedString, lexer_invalid_2.nextToken());
}
