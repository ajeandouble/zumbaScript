# Zumbascript

<p align="center"><img src="https://github.com/user-attachments/assets/98dbd173-c2db-4e5b-a9c5-7be61cc9cba5" width="35%"></img></p>

As in **_`Zumba`_** + _`Script`_ [^1]

[^1]: Self-explanatory.

## Grammar (so far)

```
program:                            global_statements
global_statements:                  (assignment SEMI | function_decl)* function_decl

function_decl:                      FUNCTION ID LPAREN function_args RPAREN compound_statement
function_args:                      empty | function_args_list
function_args_list:                 ID (COMMA ID)*

compound_statement:                 LBRACE statements_list RBRACE
statements_list:                    (statement SEMI)*
statement:                          expr | assignment | if_block | return_statement

loop_compound_statement:            LBRACE loop_statements_list RBRACE
loop_statements_list:               ((statement | loop_statement) SEMI)*
loop_statements:					break | continue | statement

expr:                               arithmetic ((LT | LE | EQ | GE | GT) arithmetic)*
arithmetic:                         term ((PLUS | MINUS) term)*
term:                               factor ((MUL | DIV) factor)*
factor:                             PLUS factor
								    | MINUS factor
								    | INTEGER
								    | LPAREN expr RPAREN
                                    | variable
								    | function_call
                                    | array_subscript
								    | STRING


assignment:                         lvalue ASSIGN expr
lvalue:                             ID | array_subscript

if_block:                           IF LPAREN expr RPAREN compound_statement |  IF LPAREN expr RPAREN compound_statement else_block
else_block:                         ELSE compound_statement | ELSE if_block
while_block:                        WHILE LPAREN expr RPAREN loop_compound_statement
return_statements:                  RETURN expr

function_call:                      ID LPAREN call_args RPAREN
call_args:                          empty | call_args_list
call_args_list:                     expr (COMMA expr)*

variable:                           ID

array_literal:                      LBRACKET array_elements RBRACKET
array_elements:                     empty | expr (COMMA expr)
array_subscript:                    lvalue LBRACKET expr RBRACKET
```

## TODO

- [x] Parser

  - [x] Parse function declarations in global scope
  - [x] Parse control flow
  - [x] If block
  - [x] Else block
  - [x] While block
    - [x] Break statement
    - [x] Continue statement
  - [ ] Floats
  - [ ] Arrays
    - [ ] Array arithmetics
    - [ ] Subscripting
    - [ ] Slicing
  - [ ] Strings
    - [x] String declaration
    - [ ] String arithmetics
    - [ ] Subscripting
    - [ ] Slicing
  - [ ] Structs
  - [ ] Panic
  - [ ] Exceptions (Index error)

- [x] Interpreter
  - [x] Global statements
  - [x] Expressions
  - [x] Functions
  - [x] Global hoisted declarations
  - [x] Calls
  - [x] Control flow
  - [x] If block
  - [x] Else block
  - [x] While block
    - [x] Break statement
  - [ ] Strings
    - [x] String declaration
    - [ ] String arithmetics
    - [ ] Subscripting
    - [ ] Slicing
  - [ ] Arrays
    - [ ] Array arithmetics
    - [ ] Subscripting
    - [ ] Slicing

## Resources

### Basics

[Crafting interpreters - Robert Nystrom](https://craftinginterpreters.com)

[A simple interpreter from scratch in Python @Jaycon Rod's blog](https://web.archive.org/web/20130616090724/http://www.jayconrod.com/posts/40/a-simple-interpreter-from-scratch-in-python-part-4)

### Memory allocation

[Tip of the day #2 - A safer Arena allocator @Gaultier's blog](https://gaultier.github.io/blog/tip_of_the_day_2.html)

[Untangling Lifetimes - The Arena Allocator @rfleury's blog](https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator)

### Zig

[Runtime Polymorphism in Zig - Zig SHOWTIME - Alex Naskos @youtube](https://www.youtube.com/watch?v=AHc4x1uXBQE)

[HTML Parser from Scratch in Zig @youtube](https://www.youtube.com/watch?v=OrU_6VdItJA)
