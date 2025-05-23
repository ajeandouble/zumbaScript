const std = @import("std");
const Token = @import("./tokens.zig").Token;
const Node = @import("./ast_nodes.zig").Node;

pub var verbose = false;

pub inline fn print(comptime fmt: []const u8, args: anytype, comptime src: std.builtin.SourceLocation) void {
    if (!verbose) return;

    const filename = std.fs.path.basename(src.file);
    std.debug.print("{s}:{}\t{s}\t" ++ fmt, .{ filename, src.line, src.fn_name } ++ args);
}

pub inline fn printNodeUnion(object: *const Node, comptime src: std.builtin.SourceLocation) void {
    if (!verbose) return;

    const info = @typeInfo(@TypeOf(object.*));
    if (info != .@"union") {
        @compileError("Not a tagged union");
    }

    const un = info.@"union";
    if (un.tag_type != null) {
        inline for (un.fields) |field| {
            if (std.mem.eql(u8, field.name, @tagName(object.*))) {
                const field_value = @field(object, field.name);
                inline for (@typeInfo(@TypeOf(field_value)).@"struct".fields) |fld| {
                    if (std.mem.eql(u8, fld.name, "token")) {
                        const fld_val = @field(field_value, fld.name);
                        if (@TypeOf(fld_val) == Token) {
                            print("{s}\n", .{fld_val.lexeme orelse return}, src);
                        }
                    }
                }
                break;
            }
        }
    } else {
        @compileError("Union has no tag type");
    }
}

pub inline fn printUnion(comptime T: type, object: T, comptime src: std.builtin.SourceLocation) void {
    if (!verbose) return;
    const info = @typeInfo(Node);

    if (info != .@"union") {
        @compileError("Not a tagged union");
    }

    const un = info.@"union";
    if (un.tag_type) |TT| {
        _ = TT;
        inline for (un.fields) |field| {
            if (std.mem.eql(u8, field.name, @tagName(object))) {
                const field_value = @field(object, field.name);
                print("{}\n", field_value, src);
                break;
            }
        }
    } else {
        @compileError("Union has no tag type");
    }
}
