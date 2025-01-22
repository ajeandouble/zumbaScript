const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;
const Program = @import("./ast_nodes.zig").Program;
const Node = @import("./ast_nodes.zig").Node;
const Num = @import("./ast_nodes.zig").Num;

const expect = std.testing.expectEqual;
const expectEqual = std.testing.expectEqual;
