# Implementation workflow

Follow this process for every step in `.claude/steps/`.

---

## For every code change

1. **Write the code** — implement the feature described in the step file.

2. **Write tests first (or alongside)** — add cases to the relevant `*_tests.zig` file before marking anything done. Each feature needs at minimum:
   - One positive test (happy path).
   - One negative test (error/edge case that must not panic).

3. **Run the unit tests** — run all three test suites and fix any failures before moving on:
   ```sh
   zig test src/lexer_tests.zig
   zig test src/parser_tests.zig
   zig test src/interpreter_tests.zig
   ```

4. **Build the binary**:
   ```sh
   zig build
   ```

5. **Run the integration programs** — pipe each relevant `.zmb` program through the compiled binary and verify the exit code and output:
   ```sh
   cat test_programs/<relevant>.zmb | zig-out/bin/zumba
   echo "exit: $?"
   ```
   Run this for the new program added in the step *and* for all previously passing programs to catch regressions.

6. **Use a sub-agent for steps 3–5** — delegate the build + test run + binary execution to a separate agent so failures are isolated and the main context stays clean. The sub-agent should:
   - Run the three `zig test` commands.
   - Run `zig build`.
   - Run every `.zmb` in `test_programs/` through `zig-out/bin/zumba`.
   - Report pass/fail per file, exit codes, and any stderr output.

---

## Definition of done for a step

- [ ] All three `zig test` suites pass with no new failures.
- [ ] `zig build` produces a clean binary.
- [ ] Every `.zmb` in `test_programs/` runs without panicking.
- [ ] The new `.zmb` program for this step exits with the expected code.
- [ ] The step's acceptance criteria (bottom of its `.md` file) are all met.
- [ ] README.md TODO checkboxes for the completed items are ticked.
