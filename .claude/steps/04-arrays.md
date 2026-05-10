# Step 04 — Array literals + interpreter evaluation

**Depends on:** nothing (independent of string steps).

---

## Current state

- `ast_nodes.zig` has `Array { token, elements: ArrayList(*Node) }` — already defined.
- The parser likely emits `Node.array` for `[]` and `[e1, e2, ...]`.
- `interpreter.zig` has no `visitArray`; `visit` falls to `NotImplemented`.
- `test_programs/arrays1.zmb` contains `a = [];` — this currently fails at the interpreter.

Verify with:
```sh
zig build && echo 'function main() { a = []; return 0; }' | zig-out/bin/zumba
```

---

## 1. `interpreter.zig` — `Value`

`Value` already has `.array: []Value`. Arrays are heap-allocated slices.

---

## 2. `interpreter.zig` — `visitArray`

```zig
fn visitArray(self: *Self, node: *const Array) anyerror!EvalResult {
    const elems = node.elements.items;
    const buf = try self.allocator.alloc(Value, elems.len);
    for (elems, 0..) |elem, i| {
        buf[i] = try (try self.visit(elem)).getValue();
    }
    return EvalResult.ok(.{ .array = buf });
}
```

Wire into `visit`:
```zig
.array => self.visitArray(&node.array),
```

---

## 3. Assignment of arrays

`visitAssignment` stores `EvalResult` values — no extra work needed since `Value.array` is a slice pointer.

---

## 4. Tests — `interpreter_tests.zig`

```zig
test "empty array literal evaluates without error" {
    // a = [];  → Value.array with len 0
}
test "array literal with elements has correct length and values" {
    // a = [1, 2, 3];  → len 3, a[0]==1
}
```

---

## 5. `.zmb` programs

`test_programs/arrays1.zmb` — update to `[1, 2, 3]` once subscript (Step 05) is ready.  
For this step just verify `a = [];` exits 0.

---

## Acceptance criteria

- `a = [];` and `a = [1, 2, 3];` both parse and interpret without error.
- `visit` never hits `NotImplemented` for an `array` node.
- All prior tests still pass.
