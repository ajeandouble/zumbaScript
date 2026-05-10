const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;
const Program = @import("./ast_nodes.zig").Program;
const AstNode = @import("./ast_nodes.zig");
const Node = @import("./ast_nodes.zig").Node;
const Num = @import("./ast_nodes.zig").Num;
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
