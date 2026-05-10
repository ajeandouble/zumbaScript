Implement the step described in `.claude/steps/$ARGUMENTS-*.md`.

1. Read the step file at `.claude/steps/$ARGUMENTS-*.md` (match by number prefix).
2. Read all source files relevant to the step (lexer, parser, ast_nodes, interpreter, and their test files).
3. Implement the changes described in the step file across all affected source files.
4. Write tests in the appropriate `*_tests.zig` file — at minimum one positive and one negative case per new behaviour.
5. Create any `.zmb` programs listed in the step's "integration program" section if they don't already exist in `test_programs/`.
6. Spawn a sub-agent to run verification:
   - `zig test src/lexer_tests.zig`
   - `zig test src/parser_tests.zig`
   - `zig test src/interpreter_tests.zig`
   - `zig build`
   - `cat test_programs/<new>.zmb | zig-out/bin/zumba && echo "exit: $?"`
   - Run every other `.zmb` in `test_programs/` through the binary to catch regressions.
   Report pass/fail per command and exit code per program.
7. Fix any failures found by the sub-agent, then re-run until all pass.
8. Tick the relevant checkboxes in `README.md` TODO section.
9. Report a one-paragraph summary: what changed, test results, any design decisions made.
