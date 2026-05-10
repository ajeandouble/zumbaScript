# Step 00 — Typing rules (decide before implementing anything else)

This is a design document, not a code change. Pin down every decision here before starting Step 01.
Update `language.md` to reflect the final choices.

---

## Type system character

Zumbascript is **dynamically typed**: types are carried by values at runtime, not declared in the source.
Type errors are runtime errors, not compile-time errors.

---

## Truthiness

| Value            | Truthy? |
| ---------------- | ------- |
| `integer != 0`   | yes     |
| `integer == 0`   | no      |
| `float != 0.0`   | yes     |
| `float == 0.0`   | no      |
| non-empty string | yes     |
| `""`             | no      |
| non-empty array  | yes     |
| `[]`             | no      |
| `void`           | no      |

**Required change:** `isTruethy` in `interpreter.zig` currently returns `InvalidConditionType` for strings — extend it to cover all types using the table above.

---

## Equality (`==`)

| LHS type                 | RHS type | Result                                                   |
| ------------------------ | -------- | -------------------------------------------------------- |
| integer                  | integer  | value equality                                           |
| float                    | float    | value equality                                           |
| integer                  | float    | promote int, value equality                              |
| string                   | string   | byte-by-byte content equality (not pointer equality)     |
| array                    | array    | **not supported** — runtime error `InvalidConditionType` |
| anything                 | void     | always 0                                                 |
| mismatched (non-numeric) | —        | runtime error `MismatchingBinOpTypes`                    |

---

## Arithmetic type coercion

| LHS           | RHS     | Operator    | Result type | Notes                   |
| ------------- | ------- | ----------- | ----------- | ----------------------- |
| integer       | integer | `+ - * / %` | integer     | truncating division     |
| float         | float   | `+ - * /`   | float       | no `%` on floats        |
| integer       | float   | `+ - * /`   | float       | integer promotes        |
| float         | integer | `+ - * /`   | float       | integer promotes        |
| string        | string  | `+`         | string      | concatenation           |
| array         | array   | `+`         | array       | concatenation           |
| anything else | —       | any         | error       | `MismatchingBinOpTypes` |

`int + string`, `string * int` (repeat), etc. are **not supported** — runtime error.
Rationale: implicit coercion hides bugs; if needed, an explicit `str(n)` builtin can be added later.

---

## Subscript return types

| Target type | `t[i]` returns                                    |
| ----------- | ------------------------------------------------- |
| string      | 1-character **string** (not an integer char code) |
| array       | the `Value` stored at index `i` (dynamic)         |

---

## Slice semantics

Slices use **view semantics** (zero-copy, pointer into original buffer):

- `s[lo..hi]` for strings: a `[]const u8` view — string mutations via `+` still produce a new allocation.
- `a[lo..hi]` for arrays: a `[]Value` view — writes through the slice (`sub[0] = x`) mutate the original array.

Rationale: consistent with Zig idioms, zero-cost, arena safety guaranteed.

---

## Mixed-type arrays

Arrays may contain values of different types: `[1, "hello", 2.5]` is legal.
Type checking happens at the operation level (e.g., arithmetic on array elements), not at array construction.

---

## Void

`void` is not a first-class value — it cannot be stored in a variable or passed as an argument.
A function that falls off the end without a `return` implicitly returns `void`.
Assigning `void` to a variable is a runtime error `InvalidGlobalStatement` (reuse existing error).

---

## Action items before Step 01

- [ ] Extend `isTruethy` in `interpreter.zig` to cover all types per the table above.
- [ ] Extend `==` handling in `computeIntBinOp` / new `computeStringEq` for string equality.
- [ ] Document the rules in `language.md` under a new "Type system" section.
- [ ] Add tests for each truthiness and equality case.
