# Step 07 — Array slicing (`a[lo..hi]`)

**Depends on:** Step 03 (`.Slice` AST node and `..` token), Step 05 (array subscript).

---

## Semantics

`a[lo..hi]` returns a **new** array containing elements `lo` (inclusive) to `hi` (exclusive).  
The original array is not mutated.

---

## `interpreter.zig` — extend `visitSlice`

The `visitSlice` added in Step 03 already handles strings.  
Add an `.array` branch:

```zig
.array => |arr| {
    if (lo > hi or hi > arr.len)
        return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .msg = "" });
    const buf = try self.allocator.alloc(Value, hi - lo);
    @memcpy(buf, arr[lo..hi]);
    return EvalResult.ok(.{ .array = buf });
},
```

---

## Tests

```zig
test "array slice returns correct sub-array" {
    // [10, 20, 30, 40][1..3] → [20, 30]
}
test "array slice lo == hi returns empty array" {
    // [1, 2, 3][2..2] → []
}
test "array slice out of bounds returns error" {
    // [1, 2][0..5] → IndexOutOfBounds
}
```

---

## `.zmb` — `test_programs/arrays_slice.zmb`

```zumba
function main() {
    a = [10, 20, 30, 40];
    sub = a[1..3];
    return sub[0];    # 20
}
```

---

## Acceptance criteria

- Slice result is independent of the original array (copy semantics).
- `lo == hi` → empty array `[]`, not an error.
- Out-of-range bounds → `IndexOutOfBounds`, no panic.
- String slicing (Step 03) still passes; the only change is inside the `.array` branch.
