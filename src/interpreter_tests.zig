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
const Assign = @import("./ast_nodes.zig").Assign;
const IfBlock = @import("./ast_nodes.zig").IfBlock;
const ElseBlock = @import("./ast_nodes.zig").ElseBlock;
const WhileBlock = @import("./ast_nodes.zig").WhileBlock;
const BreakStatement = @import("./ast_nodes.zig").BreakStatement;
const ContinueStatement = @import("./ast_nodes.zig").ContinueStatement;
const Return = @import("./ast_nodes.zig").Return;
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

    // Nodes
    var id_i_var = Variable{ .id = "i", .token = id_i_tok };
    var id_i = Node{ .variable = &id_i_var };

    var num_0_obj = Num{ .value = 0, .token = num_0_tok };
    var num_0 = Node{ .num = &num_0_obj };

    var num_1_obj = Num{ .value = 1, .token = num_1_tok };
    var num_1 = Node{ .num = &num_1_obj };

    var num_42_obj = Num{ .value = 42, .token = num_42_tok };
    var num_42 = Node{ .num = &num_42_obj };

    // Initial assignment: i = 0
    var init_assign_obj = BinOp{ .lhs = &id_i, .rhs = &num_0, .token = assign_tok };
    var init_assign = Node{ .binop = &init_assign_obj };

    // Increment: i = i + 1
    var plus_i_obj = BinOp{ .lhs = &id_i, .rhs = &num_1, .token = plus_tok };
    var plus_i = Node{ .binop = &plus_i_obj };

    var inc_i_obj = BinOp{ .lhs = &id_i, .rhs = &plus_i, .token = assign_tok };
    var inc_i = Node{ .binop = &inc_i_obj };

    // Condition: i < 42
    var while_cond_obj = BinOp{ .lhs = &id_i, .rhs = &num_42, .token = lt_tok };
    var while_cond = Node{ .binop = &while_cond_obj };

    // Condition: i >= 42
    var break_cond_obj = BinOp{ .lhs = &id_i, .rhs = &num_42, .token = ge_tok };
    var break_cond = Node{ .binop = &break_cond_obj };

    // Break statement
    var break_stmt_obj = BreakStatement{ .token = break_tok };
    var break_stmt = Node{ .break_stmt = &break_stmt_obj };

    // Continue statement
    var continue_stmt_obj = ContinueStatement{ .token = continue_tok };
    var continue_stmt = Node{ .continue_stmt = &continue_stmt_obj };

    // If block with break
    var if_block_stmts = std.ArrayList(*Node).init(allocator);
    try if_block_stmts.append(&break_stmt);
    defer if_block_stmts.deinit();

    var if_block_obj = IfBlock{ .condition = &break_cond, .statements = if_block_stmts };
    var if_block = Node{ .if_block = &if_block_obj };

    // Else block with continue and increment
    var else_block_stmts = std.ArrayList(*Node).init(allocator);
    try else_block_stmts.append(&inc_i);
    try else_block_stmts.append(&continue_stmt);
    defer else_block_stmts.deinit();

    var else_block_obj = ElseBlock{ .condition = null, .statements = else_block_stmts };

    // Connect if and else blocks
    if_block.if_block.next_else = &else_block_obj;

    // While block statements
    var while_block_stmts = std.ArrayList(*Node).init(allocator);
    try while_block_stmts.append(&if_block);
    defer while_block_stmts.deinit();

    // While block
    var while_block_obj = WhileBlock{ .condition = &while_cond, .statements = while_block_stmts };
    var while_block = Node{ .while_block = &while_block_obj };

    // Main function statements
    var main_stmts = std.ArrayList(*Node).init(allocator);
    try main_stmts.append(&init_assign);
    try main_stmts.append(&while_block);
    defer main_stmts.deinit();

    // Return node for main function
    var return_obj = Return{ .expr = &id_i };
    var return_node = Node{ .ret = &return_obj };
    try main_stmts.append(&return_node);

    // Main function declaration
    var main_args = std.ArrayList(*Node).init(allocator);
    defer main_args.deinit();

    var main_func_obj = FunctionDecl{ .id = "main", .args = main_args, .statements = main_stmts };
    var main_func = Node{ .func_decl = &main_func_obj };

    var functions = std.ArrayList(*Node).init(allocator);
    try functions.append(&main_func);
    // Program setup
    var dummyAST = Program{
        .id = "",
        .functions = functions,
        .global_statements = std.ArrayList(*Node).init(allocator),
    };
    defer functions.deinit();
    defer dummyAST.global_statements.deinit();

    var interp = try Interpreter.init(&dummyAST, allocator);
    defer interp.deinit();

    const result = try interp.interpret();
    try std.testing.expectEqual(@as(i64, 42), result);
}
