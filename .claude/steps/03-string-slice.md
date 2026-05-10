# Step 03 — String slicing (`s[lo..hi]`)

**Depends on:** Step 02 (subscript node and visitor exist).

---

## Grammar change — `grammar.peg`

Uncomment and add:

```peg
DOTDOT       <- ".."
StringSlice  <- LValue LBRACK Expr DOTDOT Expr RBRACK
```

Add `StringSlice` as an alternative in `Factor` before `ArraySubscript` (longer match wins):

```peg
Factor <- ... / StringSlice / ArraySubscript / ...
```

---

## 1. `tokens.zig`

Add `TokenType.dotdot`.

## 2. `lexer.zig`

Add `".."` to `mult_chr_toks`:

```zig
.{ "..", TokenType.dotdot },
```

---

## 3. `ast_nodes.zig`

```zig
pub const Slice = struct {
    token: Token,
    target: *const Node,
    lo:     *const Node,
    hi:     *const Node,  // exclusive upper bound
};
// Add to Node union: slice: Slice,
```

---

## 4. `parser.zig`

After consuming `lvalue LBRACK expr`, peek at the next token:
- `DOTDOT` → consume it, parse `hi` expr, expect `RBRACK` → emit `Slice` node.
- Otherwise → emit `Subscript` node (Step 02 path).

---

## 5. `interpreter.zig`

```zig
fn visitSlice(self: *Self, node: *const Slice) anyerror!EvalResult {
    const target = try (try self.visit(node.target)).getValue();
    const lo: usize = @intCast((try (try self.visit(node.lo)).getValue()).integer);
    const hi: usize = @intCast((try (try self.visit(node.hi)).getValue()).integer);

    return switch (target) {
        .string => |s| {
            if (hi > s.value.len or lo > hi)
                return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .msg = "" });
            const ptr = try self.allocator.create(String);
            ptr.* = try String.initFromSlice(node.token, s.value[lo..hi], self.allocator);
            return EvalResult.ok(.{ .string = ptr });
        },
        else => EvalResult.failure(.{ .type = Error.NotImplemented, .msg = "" }),
    };
}
```

---

## 6. Tests

```zig
test "string slice returns correct substring" {
    // "hello"[1..3] == "el"
}
test "string slice lo > hi returns error" {}
test "string slice hi > len returns error" {}
```

---

## 7. `.zmb` — `test_programs/strings_slice.zmb`

```zumba
function main() {
    s = "hello";
    sub = s[1..4];
    return 0;
}
```

---

## Acceptance criteria

- `..` lexes as `dotdot`, not two `.` tokens.
- `s[1..3]` produces a fresh string value; original is unchanged.
- All boundary error cases return errors, not panics.
