# Step 08 — Runtime error trace

**Depends on:** All prior steps. Tokens already carry `line: usize`; this step wires that into error reporting.

---

## Goal

When a runtime error occurs the user currently sees:

```
Runtime error: IndexOutOfBounds
```

After this step they see:

```
Runtime error: IndexOutOfBounds
  line 4 | return a[3];
  in main
  in <global>
```

---

## Changes

### `EvalResultErr` — add line number

```zig
const EvalResultErr = struct { type: Error, line: usize, msg: []const u8 = "" };
```

`msg` can remain empty for now but change it to `[]const u8` (it was `[]u8` for no reason).

---

### `Interpreter` — two new fields

```zig
source_lines: []const []const u8,   // source split on '\n', owned by caller
call_trace: std.ArrayList(TraceEntry),
```

```zig
const TraceEntry = struct { name: []const u8, line: usize };
```

Add `source_lines` as a parameter to `Interpreter.init`. In `main.zig`, split `input_stdin` on `'\n'` before constructing the interpreter and pass the slice in.

Add `call_trace` to `init` / `deinit` like the existing `stack`.

---

### `visitFuncCall` — push/pop trace entries

```zig
fn visitFuncCall(self: *Self, func_call: *const FunctionCall) anyerror!EvalResult {
    const id = func_call.id;
    if (self.global_funcs.get(id)) |func| {
        try self.call_trace.append(self.allocator, .{ .name = id, .line = func_call.token.line });
        try self.pushStackFrame();
        const result = try self.visitStatements(func.statements);
        try self.popStackFrame();
        _ = self.call_trace.pop();
        return switch (result) {
            .return_val => EvalResult.ok(result.return_val),
            else => result,
        };
    }
    return EvalResult.failure(.{ .type = Error.FunctionIsNotDeclared, .line = func_call.token.line });
}
```

---

### All `EvalResult.failure(...)` call sites — add `.line`

Every call site already has access to a token (node.token, sub.token, etc.).  
Global scope errors (e.g. `DuplicateFunctionDeclaration`) can use line 0.

Pattern:
```zig
// before
return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .msg = "" });

// after
return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .line = node.token.line });
```

---

### `printRuntimeError` — format the full trace

```zig
fn printRuntimeError(self: *const Self, e: EvalResultErr) void {
    const stderr = std.fs.File.stderr();
    stderr.writeAll("Runtime error: ") catch {};
    stderr.writeAll(@errorName(e.type)) catch {};
    stderr.writeAll("\n") catch {};

    if (e.line > 0 and e.line <= self.source_lines.len) {
        const src = self.source_lines[e.line - 1];
        var buf: [32]u8 = undefined;
        const prefix = std.fmt.bufPrint(&buf, "  line {} | ", .{e.line}) catch "  | ";
        stderr.writeAll(prefix) catch {};
        stderr.writeAll(src) catch {};
        stderr.writeAll("\n") catch {};
    }

    // walk call trace from outermost to innermost
    var i: usize = self.call_trace.items.len;
    while (i > 0) {
        i -= 1;
        const frame = self.call_trace.items[i];
        var buf: [256]u8 = undefined;
        const line = std.fmt.bufPrint(&buf, "  in {s} (called at line {})\n", .{ frame.name, frame.line }) catch "";
        stderr.writeAll(line) catch {};
    }
    if (self.call_trace.items.len == 0) {
        stderr.writeAll("  in <global>\n") catch {};
    }
}
```

Update all three call sites in `interpret()` to pass `self` (or make the method take `self`):
```zig
self.printRuntimeError(ret.err);
```

---

## `main.zig` — split source into lines

```zig
// after reading input_stdin:
var line_buf = std.ArrayList([]const u8){};
defer line_buf.deinit(allocator);
var it = std.mem.splitScalar(u8, input_stdin, '\n');
while (it.next()) |line| try line_buf.append(allocator, line);

var interpreter = try Interpreter.init(ast, allocator, line_buf.items);
```

---

## `debug_trace.zig` (optional helper)

If the formatting logic grows, extract it into a small `debug_trace.zig` module that exports a single `printTrace(stderr, source_lines, err, call_trace)` function. Keep it out of `interpreter.zig` to avoid bloat.

---

## Tests

Add to `interpreter_tests.zig`:

```zig
test "error trace includes line number" {
    // build AST with a token at a known line, trigger IndexOutOfBounds,
    // verify interpret() returns 1 (stderr output is not captured in unit tests —
    // integration coverage comes from the .zmb programs).
}
```

The line-printing is hard to unit-test (it goes to stderr). Cover it with a `.zmb` integration program and manual inspection. The unit test just verifies the interpreter still returns the right exit code.

---

## `.zmb` — `test_programs/error_trace.zmb`

```zumba
a = [1, 2];
function bad() {
    return a[9];
}
function main() {
    return bad();
}
```

Expected output on stderr:
```
Runtime error: IndexOutOfBounds
  line 3 |     return a[9];
  in bad (called at line 6)
```

---

## Acceptance criteria

- A runtime error prints: error name, source line, call chain.
- Line numbers are 1-based and match the source.
- No panic, no change to the exit code (still 1 on error).
- All existing tests still pass.
- `msg: []u8` changed to `[]const u8` throughout.
