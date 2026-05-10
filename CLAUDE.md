# Zumbascript

A toy scripting language interpreter written in Zig. Implements a lexer, recursive-descent parser, AST, and tree-walking interpreter.

## Build & Run

```sh
zig build                           # build the zumba executable
cat  <file.zmb> | zig-out/bin/zumba # run a script
zig test src/lexer_tests.zig        # run lexer tests
zig test src/parser_tests.zig       # run parser tests
zig test src/interpreter_tests.zig  # run interpreter tests
```

## Project layout

```
src/
  main.zig              entry point
  lexer.zig             tokenizer
  lexer_tests.zig       lexer unit tests
  tokens.zig            token types
  lex_constants.zig     keyword/symbol tables
  parser.zig            recursive-descent parser → AST
  parser_tests.zig      parser unit tests
  ast_nodes.zig         AST node definitions
  interpreter.zig       tree-walking interpreter
  interpreter_tests.zig interpreter unit tests
  debug.zig             debug helpers
test_programs/          .zmb sample scripts
```

## Language grammar

See README.md for the full BNF grammar. Key constructs: functions, if/else, while (break/continue), integer arithmetic, string literals, arrays (WIP).

## Conventions

- Arena allocator for AST nodes; caller owns memory.
- Errors surface as Zig error unions — never use `unreachable` for user-facing errors.
- Test files mirror the module they test (e.g. `parser_tests.zig` tests `parser.zig`).
- `.zmb` programs in `test_programs/` serve as integration tests; keep them compiling.
