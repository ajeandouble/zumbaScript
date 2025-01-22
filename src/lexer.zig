const std = @import("std");
const dbg = @import("./debug.zig");
const Token = @import("./tokens.zig").Token;
const TokenType = @import("./tokens.zig").TokenType;
const reserved = @import("./lex_constants.zig").reserved;
const op_math = @import("./lex_constants.zig").math;
const op_cmp = @import("./lex_constants.zig").cmp;
const op_assign = @import("./lex_constants.zig").assign;
const delimeters = @import("./lex_constants.zig").delimeters;
const separators = @import("./lex_constants.zig").separators;

pub const Error = error{ NullSource, NullTokens, BadToken, OutOfBounds, UnterminatedString };

const reserved_kws = std.StaticStringMap(TokenType).initComptime(.{
    .{ reserved._function, TokenType.function_kw },
    .{ reserved._return, TokenType.return_kw },
    .{ reserved._if, TokenType.if_kw },
    .{ reserved._else, TokenType.else_kw },
    .{ reserved._while, TokenType.while_kw },
    .{ reserved._for, TokenType.for_kw },
    .{ reserved._break, TokenType.break_kw },
    .{ reserved._continue, TokenType.continue_kw },
});

const single_chr_toks = std.StaticStringMap(TokenType).initComptime(.{
    .{ op_math.plus, TokenType.plus },
    .{ op_math.minus, TokenType.minus },
    .{ op_math.mul, TokenType.mul },
    .{ op_math.div, TokenType.div },
    .{ op_math.mod, TokenType.mod },
    .{ op_cmp.lt, TokenType.lt },
    .{ op_assign.assign, TokenType.assign },
    .{ op_cmp.gt, TokenType.gt },
    .{ delimeters.lparen, TokenType.lparen },
    .{ delimeters.rparen, TokenType.rparen },
    .{ delimeters.lbrace, TokenType.lbrace },
    .{ delimeters.rbrace, TokenType.rbrace },
    .{ delimeters.lbrack, TokenType.lbrack },
    .{ delimeters.rbrack, TokenType.rbrack },
    .{ separators.comma, TokenType.comma },
    .{ separators.semi, TokenType.semi },
    .{ "\n", TokenType.eol },
});

const mult_chr_toks = std.StaticStringMap(TokenType).initComptime(.{
    .{ "==", TokenType.eq },
    .{ "<=", TokenType.le },
    .{ ">=", TokenType.ge },
});

const whitespaces_no_nl = std.StaticStringMap(undefined).initComptime(.{
    .{" "},
    .{"\t"},
    .{"\r"},
});

pub const Lexer: type = struct {
    const Self = @This();
    allocator: std.mem.Allocator = undefined,
    source: ?([]const u8) = undefined,
    pos: usize = 0,
    line: usize = 0,
    tokens: ?std.ArrayList(Token),

    pub fn init(buffer: []const u8, allocator: std.mem.Allocator) !Self {
        const tokens = std.ArrayList(Token).init(allocator);
        const lexer = Self{ .allocator = allocator, .source = buffer, .tokens = tokens };
        return lexer;
    }

    pub fn deinit(self: *Self) void {
        for (self.tokens.?.items) |*tok| {
            tok.deinit();
            tok.lexeme = null;
        }
        self.tokens.?.deinit();
        self.tokens = null;
    }

    pub fn tokenize(self: *Self) !void {
        if (self.tokens) |*tokens| {
            var next_tok = try self.nextToken();
            try tokens.append(next_tok);
            while (next_tok.type != TokenType.eof) {
                dbg.print("{}: '{s}' L:{}\n", .{ next_tok.type, next_tok.lexeme.?, next_tok.line }, @src());
                next_tok = try self.nextToken();
                try tokens.*.append(next_tok);
            }
        } else {
            return Error.NullTokens;
        }
    }

    pub fn getTokens(self: *const Self) !std.ArrayList(Token) {
        return self.tokens orelse Error.NullTokens;
    }

    pub fn nextToken(self: *Self) !Token {
        self.skipWhitespace() catch {
            return Token.init(TokenType.eof, "", self.line, self.allocator);
        };

        if (self.isAtEnd()) {
            return Token.init(TokenType.eof, "", self.line, self.allocator);
        }

        for (reserved_kws.keys()) |kw| {
            if (std.mem.eql(u8, self.lookAhead(kw.len), kw)) {
                const following_chr = self.peek(kw.len);
                if (following_chr != 0x0 and std.ascii.isAlphanumeric(following_chr)) {
                    continue;
                }
                try self.advance(kw.len);
                const tokType = reserved_kws.get(kw) orelse unreachable;
                return Token.init(tokType, kw, self.line, self.allocator);
            }
        }

        for (mult_chr_toks.keys()) |kw| {
            if (std.mem.eql(u8, self.lookAhead(kw.len), kw)) {
                try self.advance(kw.len);
                const tokType = mult_chr_toks.get(kw) orelse unreachable;
                return Token.init(tokType, kw, self.line, self.allocator);
            }
        }

        if (self.peek(0) == '"') {
            return try self.string();
        }

        for (single_chr_toks.keys()) |kw| {
            const single_chr = kw[0];
            if (self.peek(0) == single_chr) {
                try self.advance(1);
                const tokType = single_chr_toks.get(kw) orelse unreachable;
                return Token.init(tokType, kw, self.line, self.allocator);
            }
        }

        if (std.ascii.isDigit(self.peek(0))) {
            return try self.num();
        }

        if (std.ascii.isAlphabetic(self.peek(0))) {
            return try self.id();
        }

        try self.advance(1);
        return Error.BadToken;
    }

    // Helper functions
    fn string(self: *Self) !Token {
        const start = self.pos;
        try self.advance(1);
        while (self.peek(0) != '"' and self.peek(0) != 0x00) {
            try self.advance(1);
        }
        if (self.pos == self.source.?.len) {
            return Error.UnterminatedString;
        }
        try self.advance(1);
        return Token.init(TokenType.string, self.source.?[start..self.pos], self.line, self.allocator);
    }

    fn num(self: *Self) !Token {
        const start = self.pos;
        var n: i28 = 0;
        while (std.ascii.isDigit(self.peek(0))) {
            n *= 10;
            n += @intCast(self.peek(0));
            try self.advance(1);
        }
        return Token.init(TokenType.integer, self.source.?[start..self.pos], self.line, self.allocator);
    }

    fn id(self: *Self) !Token {
        const start = self.pos;
        while (std.ascii.isAlphanumeric(self.peek(0))) {
            try self.advance(1);
        }
        return Token.init(TokenType.id, self.source.?[start..self.pos], self.line, self.allocator);
    }

    // Utils functions
    inline fn peek(self: *const Self, offset: ?usize) u8 {
        const offs = offset orelse 0;
        return if (self.pos + offs < self.source.?.len) self.source.?[self.pos + offs] else 0x00;
    }

    inline fn lookAhead(self: *const Self, len: usize) []const u8 {
        if (self.pos + len >= self.source.?.len) {
            return "";
        }
        return self.source.?[self.pos .. self.pos + len];
    }

    inline fn advance(self: *Self, offset: ?usize) !void {
        const offs = offset orelse 1;
        if (self.pos + offs <= self.source.?.len) {
            self.pos += offs;
        } else {
            return Error.OutOfBounds;
        }
    }

    inline fn isAtEnd(self: *const Self) bool {
        return self.pos >= (self.source orelse unreachable).len;
    }

    fn skipWhitespace(self: *Self) Error!void {
        while (!self.isAtEnd()) {
            switch (self.peek(0)) {
                ' ', '\r', '\t' => try self.advance(1),
                '\n' => {
                    self.line += 1;
                    try self.advance(1);
                },
                else => break,
            }
        } else {
            return Error.OutOfBounds;
        }
    }
};
