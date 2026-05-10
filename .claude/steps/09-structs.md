# Step 09 — Structs

**Depends on:** Steps 01–08 all complete (structs are the most complex addition).

---

## Proposed syntax

```zumba
struct Point {
    x;
    y;
}

function main() {
    p = Point { x: 1, y: 2 };
    p.x = 10;
    return p.x + p.y;    # 12
}
```

---

## Grammar additions — `grammar.peg`

```peg
StructDecl    <- STRUCT ID LBRACE StructFields RBRACE
StructFields  <- (ID SEMI)*

StructLiteral <- ID LBRACE FieldInits RBRACE
FieldInits    <- (ID COLON Expr (COMMA ID COLON Expr)*)?

FieldAccess   <- Expr DOT ID
```

Add `StructDecl` alongside `FunctionDecl` at the top level.  
Add `StructLiteral` and `FieldAccess` to `Factor` / `LValue`.

---

## 1. `tokens.zig`

Add: `TokenType.struct_kw`, `TokenType.dot`, `TokenType.colon`.

## 2. `lexer.zig`

- Add `"struct"` to `reserved_kws`.
- Add `"."`, `":"` to `single_chr_toks`.

---

## 3. `ast_nodes.zig`

```zig
pub const StructDecl = struct {
    token:  Token,
    id:     []const u8,
    fields: []const []const u8,  // field names in declaration order
};

pub const StructLiteral = struct {
    token:      Token,
    struct_id:  []const u8,
    field_names: []const []const u8,
    field_vals:  []const *Node,
};

pub const FieldAccess = struct {
    token:  Token,
    target: *const Node,
    field:  []const u8,
};

// Add to Node union:
//   struct_decl:    StructDecl,
//   struct_literal: StructLiteral,
//   field_access:   FieldAccess,
```

---

## 4. `interpreter.zig`

### Value representation

```zig
// Add to Value union:
//   object: *std.StringHashMap(Value),
```

### New visitors

- `visitStructDecl` — register the struct definition (field list) in a `global_structs` map on `Interpreter`, similar to `global_funcs`.
- `visitStructLiteral` — look up the struct definition, allocate a `StringHashMap(Value)`, populate fields.
- `visitFieldAccess` — resolve the target to `.object`, look up the field name.

### Assignment to fields

Extend `visitAssignment` for `FieldAccess` lvalue (similar to Step 05 subscript write path).

---

## 5. Tests

```zig
test "struct literal creates object with correct fields"
test "field read returns correct value"
test "field write mutates object"
test "accessing undeclared field returns error"
test "struct literal with wrong field name returns error"
```

---

## `.zmb` — `test_programs/structs.zmb`

```zumba
struct Point {
    x;
    y;
}

function main() {
    p = Point { x: 3, y: 4 };
    p.x = 10;
    return p.x + p.y;    # 14
}
```

---

## Acceptance criteria

- Struct declarations are hoisted (order in file doesn't matter).
- Field access on a non-object returns an interpreter error, not a panic.
- Nested structs (struct containing struct) work via the same mechanism.
