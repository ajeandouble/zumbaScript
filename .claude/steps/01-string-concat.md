# Step 01 — String concatenation (`+`)

**Status:** `String` AST node exists. Lexer and parser already produce `string` tokens and `Node.string`.  
The interpreter's `visitBinOp` falls through to `NotImplemented` for non-integer operands.

---

## What to implement

`"hello" + ", " + "world"` → `"hello, world"`

No other string arithmetic is planned at this step (no `*` repeat).

---

## 1. `ast_nodes.zig`

`String.append` is currently broken — it calls `std.mem.concat` but discards the result and does not free the old buffer.  
Fix it:

```zig
pub fn append(self: *String, more: []const u8) !void {
    const new_buf = try self.allocator.alloc(u8, self.value.len + more.len);
    @memcpy(new_buf[0..self.value.len], self.value);
    @memcpy(new_buf[self.value.len..], more);
    self.allocator.free(self.value);
    self.value = new_buf;
}
```

---

## 2. `interpreter.zig`

In `visitBinOp`, after the `.integer` branch add a `.string` branch:

```zig
.string => {
    if (binop.token.type != TokenType.plus) return Error.WrongBinOpTypes;
    const lhs_str = lhs_val.string;
    const rhs_str = try rhs_res.getValue();
    if (rhs_str != .string) return Error.MismatchingBinOpTypes;
    // allocate a fresh String for the result
    var result = try String.initFromSlice(
        binop.token, lhs_str.value, self.allocator
    );
    try result.append(rhs_str.string.value);
    const result_ptr = try self.allocator.create(String);
    result_ptr.* = result;
    return EvalResult.ok(.{ .string = result_ptr });
},
```

Also update `visitAssignment` — when storing a string value, no extra work is needed since `Value.string` is a pointer.

---

## 3. Tests — `interpreter_tests.zig`

```zig
test "string concatenation" {
    // "hello" + ", " + "world" == "hello, world"
}
test "string + non-string returns error" {
    // "hi" + 1  should surface WrongBinOpTypes
}
```

---

## 4. `.zmb` integration program — `test_programs/strings_concat.zmb`

```zumba
function main() {
    a = "hello";
    b = ", world";
    c = a + b;
    return 0;
}
```

---

## Acceptance criteria

- `zig test src/interpreter_tests.zig` passes all string tests.
- `zig build` succeeds.
- Running `strings_concat.zmb` exits 0 without panicking.
