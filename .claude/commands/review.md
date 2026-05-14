Review the latest commit (or the commit range given as $ARGUMENTS) for code quality, test coverage, and commit message hygiene. Follow these steps:

## 0. Format the code

Use `zig fmt`.

## 1. Identify the diff

If $ARGUMENTS is empty, use the latest commit (`HEAD~1..HEAD`).
Otherwise treat $ARGUMENTS as a git revision range or commit hash and diff against its parent.

Run:

```
git log -1 --format="%H %s%n%n%b" <commit>
git diff <range> --stat
git diff <range>
```

## 2. Review the commit message

Check:

- Subject line is ≤ 72 chars and written in imperative mood ("add X", not "added X" or "adds X").
- Subject is not vague ("fix stuff", "WIP", "misc changes").
- Body (if present) explains _why_, not just _what_.
- No trailing periods on the subject line.

Flag any violations and suggest a corrected message.

## 3. Review the code changes

For each changed file assess:

- **Correctness** — logic errors, off-by-ones, wrong error handling, undefined behaviour in Zig (especially unsafe pointer casts, use-after-free with the arena allocator).
- **Style** — consistent with the surrounding code; no commented-out debug prints left in.
- **Zig idioms** — prefer error unions over sentinel returns, avoid `unreachable` for user-facing paths, use `defer` for cleanup.
- **Scope** — changes are focused on what the commit message claims; no unrelated drive-by edits.

## 4. Review added/modified tests

For each test file touched:

- Are new language features covered by at least one positive and one negative test case?
- Do tests use descriptive names that explain the scenario?
- Are edge cases addressed (empty input, nested constructs, type mismatches)?
- Do integration `.zmb` scripts in `test_programs/` reflect the new behaviour?

## 5. Build & test

Run the full test suite to confirm nothing regressed:

```
zig test src/lexer_tests.zig
zig test src/parser_tests.zig
zig test src/interpreter_tests.zig
zig build
```

Report pass/fail for each step.

## 6. Summary

Produce a short verdict:

- **Commit message**: pass / needs fix (with suggestion)
- **Code**: list of issues found (or "looks good")
- **Tests**: adequate / missing coverage for X
- **Build**: green / failing

Make inline suggestions with the exact file and line number where possible. Apply fixes directly when they are unambiguous and small; otherwise describe what needs to change and ask before editing.
