const std = @import("std");
const dbg = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("tokens.zig").Token;
const TokenType = @import("tokens.zig").TokenType;
const Parser = @import("./parser.zig").Parser;
const Interpreter = @import("./interpreter.zig").Interpreter;
const Program = @import("./ast_nodes.zig").Program;

const MAX_STDIN_SIZE = 4096;
const INTERPRETER_STACK_SIZE = 64 * 1024 * 1024; // 64 MB — enough for 1000 interpreter frames

fn parseArgs(args: [][:0]u8) !void {
    var i: usize = 1;
    while (i < args.len) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--verbose")) {
            dbg.verbose = true;
            i += 1;
        } else {
            _ = std.fs.File.stderr().writeAll("Wrong argument: ") catch {};
            _ = std.fs.File.stderr().writeAll(arg) catch {};
            return error{WrongArgument}.WrongArgument;
        }
    }
}

const ThreadContext = struct {
    ast: *Program,
    allocator: std.mem.Allocator,
    source_lines: []const []const u8,
    ret: i64 = 0,
    err: ?anyerror = null,
};

fn runInterpreter(ctx: *ThreadContext) void {
    var interpreter = Interpreter.init(ctx.ast, ctx.allocator, ctx.source_lines) catch |e| {
        ctx.err = e;
        return;
    };
    defer interpreter.deinit();
    ctx.ret = interpreter.interpret() catch |e| {
        ctx.err = e;
        return;
    };
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    try parseArgs(args);
    defer std.process.argsFree(allocator, args);

    const input_buf = try allocator.alloc(u8, MAX_STDIN_SIZE);
    defer allocator.free(input_buf);
    const bytes_read = try std.fs.File.stdin().readAll(input_buf);
    const input_stdin = input_buf[0..bytes_read];

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

    var line_buf = std.ArrayList([]const u8){};
    defer line_buf.deinit(allocator);
    var line_it = std.mem.splitScalar(u8, input_stdin, '\n');
    while (line_it.next()) |line| try line_buf.append(allocator, line);

    var ctx = ThreadContext{ .ast = ast, .allocator = allocator, .source_lines = line_buf.items };
    const thread = try std.Thread.spawn(.{ .stack_size = INTERPRETER_STACK_SIZE }, runInterpreter, .{&ctx});
    thread.join();
    if (ctx.err) |e| return e;

    const ret_u8: u8 = @intCast(@min(@max(ctx.ret, 0), 255));
    dbg.print("ret: {}\n", .{ret_u8}, @src());
    return ret_u8;
}

test "main" {
    // TODO!
}
