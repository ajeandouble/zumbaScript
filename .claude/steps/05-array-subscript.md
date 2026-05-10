# Step 05 — Array subscript (read + write)

**Depends on:** Step 02 (shared `Subscript` node), Step 04 (arrays evaluate to `Value.array`).

---

## Read: `a[i]`

Extend `visitSubscript` (from Step 02) with an `.array` branch:

```zig
.array => |arr| {
    if (idx < 0 or idx >= @as(i64, @intCast(arr.len)))
        return EvalResult.failure(.{ .type = Error.IndexOutOfBounds, .msg = "" });
    return EvalResult.ok(arr[@intCast(usize, idx)]);
},
```

---

## Write: `a[i] = expr`

Assignment currently handles `lvalue = expr` where `lvalue` is an `ID`.  
Now `lvalue` can be `Subscript`, which means we need to mutate an element in place.

### `visitAssignment` — extend for subscript lvalue

```zig
fn visitAssignment(self: *Self, binop: *const BinOp) anyerror!EvalResult {
    const rhs = try (try self.visit(binop.rhs)).getValue();

    switch (binop.lhs.*) {
        .variable => |v| {
            // existing path: store into symbol table
        },
        .subscript => |sub| {
            // resolve array, mutate element
            const target_res = try (try self.visit(sub.target)).getValue();
            const idx: usize = @intCast((try (try self.visit(sub.index)).getValue()).integer);
            switch (target_res) {
                .array => |arr| {
                    if (idx >= arr.len) return EvalResult.failure(...);
                    arr[idx] = rhs;  // slice is mutable because Value.array is []Value
                },
                else => return EvalResult.failure(...),
            }
        },
        else => return EvalResult.failure(.{ .type = Error.InterpreterError, .msg = "" }),
    }
    return EvalResult.ok(rhs);
}
```

> **Note:** `Value.array` must be `[]Value` (not `[]const Value`) for in-place mutation.  
> Verify the field declaration in `interpreter.zig`.

---

## Tests

```zig
test "array read subscript returns correct value" {
    // [10, 20, 30][1] == 20
}
test "array write subscript mutates in place" {
    // a = [1, 2, 3]; a[0] = 99;  a[0] == 99
}
test "array subscript out of bounds returns error" {}
```

---

## `.zmb` — `test_programs/arrays_subscript.zmb`

```zumba
function main() {
    a = [10, 20, 30];
    first = a[0];
    a[1] = 99;
    return a[1];    # exits 99
}
```

---

## Acceptance criteria

- Read and write subscript work for integer-valued arrays.
- Out-of-bounds on read or write → `IndexOutOfBounds` error, no panic.
- String subscript (Step 02) still passes; it shares the same `visitSubscript`.
