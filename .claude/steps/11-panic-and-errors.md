# Step 10 — `panic()` builtin + runtime IndexError

**Depends on:** Steps 02, 05, 07 (subscript/slice paths must emit `IndexOutOfBounds`).

---

## Part A — `panic(msg)` builtin

### Semantics

```zumba
function main() {
    panic("something went wrong");
    return 0;    # unreachable
}
```

Calling `panic(msg)` immediately terminates the program with a non-zero exit code and prints the message to stderr.

### Implementation — no new AST node needed

Handle `panic` as a **built-in function** resolved before `global_funcs` lookup in `visitFuncCall`:

```zig
fn visitFuncCall(self: *Self, func_call: *const FunctionCall) anyerror!EvalResult {
    if (std.mem.eql(u8, func_call.id, "panic")) {
        // evaluate first argument
        const msg_val = try (try self.visit(func_call.args.items[0])).getValue();
        const msg = switch (msg_val) {
            .string => |s| s.value,
            else    => "(non-string panic message)",
        };
        std.debug.print("panic: {s}\n", .{msg});
        std.process.exit(1);
    }
    // ... existing global_funcs lookup
}
```

---

## Part B — `IndexOutOfBounds` error type

Steps 02, 05, 07 reference `Error.IndexOutOfBounds` — add it to the `Error` set in `interpreter.zig`:

```zig
const Error = error{
    ...
    IndexOutOfBounds,
};
```

Ensure all subscript/slice bounds checks use this error (not `unreachable`, not `std.debug.panic`).

---

## Part C — surface runtime errors to the user

Currently `EvalResult.err` carries an error type but the top-level `interpret()` just returns `1`.  
Improve this to print a human-readable message before exiting:

```zig
.err => |e| {
    std.debug.print("runtime error: {}\n", .{e.type});
    return 1;
},
```

---

## Tests

```zig
test "panic builtin exits with code 1"
    // Requires running the binary as a subprocess; mark as integration test.

test "IndexOutOfBounds on string subscript does not call unreachable"
test "IndexOutOfBounds on array subscript does not call unreachable"
test "runtime error message is printed to stderr"
```

---

## `.zmb` — `test_programs/panic.zmb`

```zumba
function main() {
    panic("intentional panic");
    return 0;
}
```

Expected: exits non-zero, prints `panic: intentional panic` to stderr.

---

## Acceptance criteria

- `panic("msg")` prints to stderr and exits with code 1.
- `IndexOutOfBounds` is a named error, never `unreachable`.
- Out-of-bounds access on strings and arrays both produce the same error type.
- `zig build` succeeds; no existing tests regress.
