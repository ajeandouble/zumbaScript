# Step 02 — String subscript (read-only `s[i]`)

**Depends on:** Step 01 (string value lives as `*String` in the interpreter).

---

## Grammar

`ArraySubscript <- LValue LBRACK Expr RBRACK` already covers this.  
No grammar change needed; strings share the same subscript production as arrays.

---

## 1. `ast_nodes.zig`

Reuse the existing `Array`-path or add a dedicated node. A shared `Subscript` node keeps the interpreter simpler:

```zig
pub const Subscript = struct {
    token:  Token,
    target: *const Node,  // Variable or nested Subscript
    index:  *const Node,  // integer Expr
};
// Add to Node union:
//   subscript: Subscript,
```

---

## 2. `parser.zig`

In `parseFactor`, after matching an identifier check whether `[` follows.  
If yes, parse `Subscript { target: Variable(id), index: parseExpr() }` instead of a bare `Variable`.  
This must happen *before* the `FunctionCall` check (since both start with `ID`), or share the leading `ID` consumption and branch on the next token.

---

## 3. `interpreter.zig`

Add `visitSubscript` and wire it into `visit`:

```zig
fn visitSubscript(self: *Self, node: *const Subscript) anyerror!EvalResult {
    const target_res = try self.visit(node.target);
    const idx_res    = try self.visit(node.index);
    const idx: i64   = (try idx_res.getValue()).integer;

    return switch (try target_res.getValue()) {
        .string => |s| {
            if (idx < 0 or idx >= @as(i64, @intCast(s.value.len)))
                return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .msg = "" });
            const ch = try String.initFromSlice(
                node.token,
                s.value[@intCast(usize, idx) .. @intCast(usize, idx) + 1],
                self.allocator,
            );
            const ptr = try self.allocator.create(String);
            ptr.* = ch;
            return EvalResult.ok(.{ .string = ptr });
        },
        else => EvalResult.failure(.{ .type = Error.NotImplemented, .msg = "" }),
    };
}
```

---

## 4. Tests — `interpreter_tests.zig`

```zig
test "string subscript in-bounds returns single char" {
    // "hello"[1] == "e"
}
test "string subscript out of bounds returns IndexOutOfBounds" {
    // "hi"[9] must NOT panic
}
```

---

## 5. `.zmb` — `test_programs/strings_subscript.zmb`

```zumba
function main() {
    s = "hello";
    ch = s[0];
    return 0;
}
```

---

## Acceptance criteria

- Parser produces a `Subscript` node for `s[i]`.
- In-bounds: returns a 1-character string.
- Out-of-bounds: returns `IndexOutOfBounds` (no panic, no `unreachable`).
- `zig test` passes; `zig build` succeeds; `strings_subscript.zmb` exits 0.
