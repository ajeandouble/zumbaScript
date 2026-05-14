# Zumbascript code conventions

## Naming

| Thing | Convention | Example |
|---|---|---|
| Types, unions, structs | PascalCase | `EvalResult`, `StackFrame`, `FunctionDecl` |
| Functions, methods | camelCase | `visitFuncCall`, `pushStackFrame` |
| Local variables | camelCase | `argVals`, `frameIdx` |
| Constants | camelCase | `maxCallDepth` — or ALL_CAPS for compile-time limits |
| Errors | PascalCase | `IndexOutOfBounds`, `CallStackOverflow` |
| Test names | Full sentence, lowercase | `"array subscript read out of bounds returns error"` |
| Source files | snake_case | `ast_nodes.zig`, `lexer_tests.zig` |
| Script programs | snake_case | `arrays_slice.zmb`, `error_trace.zmb` |

## Comments

Write a comment only when the **why** is non-obvious. Never describe what the code does.

```zig
// owned_strings keeps interpreter-lifetime ownership of *String values
// so they survive the stack frame that created them.
self.owned_strings.append(self.allocator, s);
```

Do not write:
- Section headers (`// ── Step 06 ──`)
- Redundant narration (`// increment i`)
- Stale FIXMEs or TODOs inline in source (use a step file or issue instead)
- Commented-out debug prints

## Error handling

- **Zig errors (`!T`)**: interpreter-internal failures (OOM, I/O). Propagate with `try`.
- **`EvalResult.failure`**: user-visible runtime errors (type mismatch, OOB, undefined variable). Never let these escape as Zig errors — catch inside `visitBinOp` / `visitSubscript` etc.
- Never use `unreachable` on a path reachable by user input.

## Memory

- AST nodes are arena-allocated; the parser owns them.
- Runtime strings (`*String`) and arrays (`[]Value`) are heap-allocated and must be registered in `owned_strings` / `owned_arrays` so `deinit` can free them.
- Stack frame symbol tables are `StringHashMap(EvalResult)`; keys are duped strings owned by the frame.

## Tests

- Every new interpreter feature: at least one positive and one negative unit test in `interpreter_tests.zig`.
- Every new `.zmb` program: first line comment stating the expected exit code, e.g. `// expected exit: 42`.
- Test names are full sentences describing the scenario and expected outcome.
- Build AST nodes on the stack (local `var`); do not heap-allocate nodes inside tests.
