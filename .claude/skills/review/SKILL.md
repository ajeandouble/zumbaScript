# Review skill

Perform a full review of the Zumbascript codebase: run tests, audit architecture and code quality, fix issues, repeat until clean.

## Step 0 — Understand recent changes

Before touching any code, read the git history to understand *why* things changed:

```
git log --oneline -20
git diff main..HEAD
```

For each commit in scope, check:
- What problem was it solving? (commit message + diff together)
- Was it a bugfix, a feature, a refactor, or cleanup?
- Are there design decisions that look intentional but unusual — things that deserve the benefit of the doubt vs. things that look like oversights?

This prevents flagging intentional trade-offs as bugs and gives context for the steps below.

## Step 1 — Run tests and integration programs

```
zig test src/lexer_tests.zig
zig test src/parser_tests.zig
zig test src/interpreter_tests.zig
zig build
```

Then run every `.zmb` in `test_programs/` through the binary and record exit codes. Flag any unexpected failures before proceeding.

## Step 2 — Architecture review (coupling & layers)

The interpreter has three layers: **Lexer → Parser/AST → Interpreter**. Check:

- **Separation of Concerns**: each layer knows nothing about the layers above it. The lexer must not parse; the parser must not evaluate; the interpreter must not tokenize.
- **Open/Closed**: adding a new value type (e.g. float, struct) or a new AST node should require touching only the relevant layer files, not retrofitting every other file. Check that `Value`, `Node`, and `EvalResult` are extended by addition, not by modifying stable switch arms.
- **Ownership invariants**: `owned_strings` and `owned_arrays` exist because heap-allocated runtime values outlive their creating stack frame. Verify no new heap-allocated value escapes the interpreter without being registered there.
- **Error propagation**: Zig errors (`!T`) are for interpreter-internal failures; `EvalResult.failure` is for user-visible runtime errors. The boundary must be consistent — no `try computeBinOp(...)` that lets a runtime error escape as a Zig error.

## Step 3 — Code quality review

Check each source file against `CONVENTIONS.md`. Specifically:

- **Naming**: types PascalCase, functions/variables camelCase, test strings are full sentences, `.zmb` files are `snake_case`.
- **Zigesque**: use `defer` for cleanup, error unions over sentinel returns, avoid `unreachable` on user-facing paths, no raw `@intCast` without a comment when the cast is non-obvious.
- **Comments**: only when the WHY is non-obvious. No section headers, no `// TODO` cruft, no commented-out debug prints.
- **Dead code**: unused imports, unused variables, unreachable branches.
- **Test coverage**: every new interpreter feature has at least one positive and one negative unit test; every new `.zmb` program has an expected exit code documented in a comment.

## Step 4 — Fix issues then format

Apply fixes for anything found in steps 2 and 3, plus any obvious correctness bugs noticed while reading. Then:

```
zig fmt src/
```

Keep fixes minimal — no opportunistic refactors beyond what the review found.

## Step 5 — Re-run until clean

Repeat step 1. If anything fails, go back to step 4. Loop until all tests pass and all `.zmb` programs exit with their expected codes.

## Output

Report in four sections:

- **Tests**: pass counts before and after, any regressions fixed.
- **Architecture**: issues found and whether they were fixed or noted for a future step.
- **Code quality**: list of issues fixed; any left open with rationale.
- **Net verdict**: green / needs follow-up.
