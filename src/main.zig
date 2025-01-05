const std = @import("std");
const dbg = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("tokens.zig").Token;
const TokenType = @import("tokens.zig").TokenType;
const Parser = @import("./parser.zig").Parser;
const Interpreter = @import("./interpreter.zig").Interpreter;

const AstNode = @import("./ast_nodes.zig"); // FIXME: delete this shit!

const MAX_STDIN_SIZE = 4096;

fn parseArgs(args: [][:0]u8) void {
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
            dbg.verbose = true;
        }
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    parseArgs(args);
    defer std.process.argsFree(allocator, args);

    const stdin = std.io.getStdIn().reader();
    const input_stdin = try stdin.readAllAlloc(allocator, MAX_STDIN_SIZE);
    defer allocator.free(input_stdin);

    var lexer = Lexer.init(input_stdin, allocator) catch |err| {
        dbg.print("Error tokenizing buffer {}", .{err}, @src());
    };
    defer lexer.deinit();
    lexer.tokenize() catch |err| {
        dbg.print("Error tokenizing buffer {}\t", .{err}, @src());
        return 1;
    };
    var parser = try Parser.init(lexer.tokens.?.items, allocator);
    const ast = try parser.parse();
    defer parser.deinit();

    var gpa_ = (std.heap.GeneralPurposeAllocator(.{}){});
    const allocator_ = gpa_.allocator();
    var interpreter = try Interpreter.init(ast, allocator_);
    const ret: u8 = @intCast(try interpreter.interpret());
    defer interpreter.deinit();
    return ret;
}

test "main" {
    // TODO!
}
