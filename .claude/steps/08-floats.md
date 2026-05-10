# Step 08 — Float literals and arithmetic

**Depends on:** nothing (orthogonal to string/array steps).

---

## Grammar

```peg
FLOAT <- [0-9]+ "." [0-9]+
```
Already listed in `grammar.peg` under `Factor`. No further grammar change needed.

---

## 1. `tokens.zig` — add `TokenType.float`

---

## 2. `lexer.zig`

Before the `isDigit → num()` branch, peek ahead for a `.` to distinguish floats:

```zig
if (std.ascii.isDigit(self.peek(0))) {
    var j: usize = 1;
    while (std.ascii.isDigit(self.peek(j))) j += 1;
    return if (self.peek(j) == '.') self.float() else self.num();
}
```

Add `float()` helper (same shape as `num()` but advances past the `.` and fractional digits, emits `TokenType.float`).

---

## 3. `ast_nodes.zig`

```zig
pub const Float = struct { token: Token, value: f64 };
// Add to Node union: float: Float,
```

---

## 4. `parser.zig`

Handle `TokenType.float` in `parseFactor` the same way as `integer`:

```zig
.float => {
    const val = try std.fmt.parseFloat(f64, tok.lexeme.?);
    _ = try self.eat(.float);
    return self.makeNode(.{ .float = .{ .token = tok, .value = val } });
},
```

---

## 5. `interpreter.zig`

Add `visitFloat` (mirrors `visitInteger`).  
In `visitBinOp`, add a `.float` branch. Mixed `integer op float` → promote the integer:

```zig
.float => |lf| {
    const rf: f64 = switch (rhs_val) {
        .float   => |f| f,
        .integer => |i| @as(f64, @floatFromInt(i)),
        else     => return Error.WrongBinOpTypes,
    };
    // compute and return Value{ .float = result }
},
.integer => |li| {
    // existing int path, but also handle rhs == .float (promotion)
},
```

---

## 6. Tests

```zig
test "float literal evaluates correctly"   // 3.14 → f64 3.14
test "float addition"                      // 1.5 + 2.5 == 4.0
test "int + float promotes to float"       // 1 + 0.5 == 1.5
test "float division is not truncated"     // 7.0 / 2.0 == 3.5
```

---

## `.zmb` — `test_programs/floats.zmb`

```zumba
function main() {
    x = 1.5;
    y = 2.5;
    z = x + y;
    return 0;
}
```

---

## Acceptance criteria

- `3.14` lexes as `float`, not two `integer` + `.` tokens.
- `1.5 + 2.5` → `4.0`, no truncation.
- Integer operands auto-promote in mixed expressions.
- All existing integer tests still pass.
