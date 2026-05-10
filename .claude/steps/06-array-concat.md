# Step 06 — Array concatenation (`a + b`)

**Depends on:** Step 04 (arrays evaluate), Step 05 (arrays are mutable slices).

---

## Semantics

`[1, 2] + [3, 4]` → `[1, 2, 3, 4]`  
The result is always a **new** allocation; neither operand is mutated.  
Concatenating arrays of mixed element types is allowed (dynamic typing).

---

## `interpreter.zig` — extend `visitBinOp`

Add an `.array` branch after `.string`:

```zig
.array => |lhs_arr| {
    if (binop.token.type != TokenType.plus) return Error.WrongBinOpTypes;
    const rhs_val = try rhs_res.getValue();
    if (rhs_val != .array) return Error.MismatchingBinOpTypes;
    const rhs_arr = rhs_val.array;

    const buf = try self.allocator.alloc(Value, lhs_arr.len + rhs_arr.len);
    @memcpy(buf[0..lhs_arr.len], lhs_arr);
    @memcpy(buf[lhs_arr.len..], rhs_arr);
    return EvalResult.ok(.{ .array = buf });
},
```

---

## Tests

```zig
test "array concat produces new array with all elements" {
    // [1, 2] + [3, 4] → len 4, elems 1 2 3 4
}
test "array concat is non-mutating" {
    // original arrays unchanged after +
}
test "array + non-array returns MismatchingBinOpTypes" {}
```

---

## `.zmb` — `test_programs/arrays_concat.zmb`

```zumba
function main() {
    a = [1, 2];
    b = [3, 4];
    c = a + b;
    return c[3];    # 4
}
```

---

## Acceptance criteria

- `[1] + [2, 3]` returns `[1, 2, 3]`.
- Operand arrays are not modified.
- Type mismatch produces an interpreter error, not a crash.
