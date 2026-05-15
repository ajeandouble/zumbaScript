# Zumbascript

<p align="center"><img src="https://github.com/user-attachments/assets/98dbd173-c2db-4e5b-a9c5-7be61cc9cba5" width="35%"></img></p>

As in **_`Zumba`_** + _`Script`_ [^1]

[^1]: Self-explanatory.

## Syntax

```zumbascript
// Global scope: constants and shared state live outside functions
MAX_ITER = 10;
PI = 3.14159;

struct Vec2 {
    x;
    y;
}

function dot(a, b) {
    return a.x * b.x + a.y * b.y;
}

function safe_div(a, b) {
    if (b == 0) {
        panic("division by zero");
    }
    return a / b;
}

function sum_slice(arr, lo, hi) {
    slice = arr[lo..hi];
    total = 0.0;
    i = 0;
    while (i < hi - lo) {
        total = total + slice[i];
        i = i + 1;
    }
    return total;
}

function main() {
    // Structs
    u = Vec2 { x: 3.0, y: 4.0 };
    v = Vec2 { x: 1.5, y: 2.5 };
    d = dot(u, v);

    // Float arithmetic
    circumference = 2.0 * PI * 5.0;

    // Arrays and slicing
    nums = [10, 20, 30, 40, 50];
    mid  = nums[1..4];       // [20, 30, 40]
    s    = sum_slice(nums, 1, 4);

    // Strings and subscript
    greeting = "hello";
    first    = greeting[0];  // "h"
    tail     = greeting[1..5];

    // Control flow
    i = 0;
    result = 0;
    while (i < MAX_ITER) {
        if (i == 5) {
            i = i + 1;
            continue;
        }
        result = result + i;
        i = i + 1;
    }

    // safe_div(d, 0) would call panic() and exit non-zero
    q = safe_div(d, 2.0);

    return 0;
}
```

## Learning Resources

<p align="center"><img src="logo/zumbascript_in_action_manning.jpeg" width="60%"></img></p>

## TODO

- [x] Parser

  - [x] Parse function declarations in global scope
  - [x] Parse control flow
  - [x] If block
  - [x] Else block
  - [x] While block
    - [x] Break statement
    - [x] Continue statement
  - [x] Integer arithmetic (`+` `-` `*` `/` `%`)
  - [x] Comparison operators (`<` `<=` `==` `!=` `>=` `>`)
  - [x] Logical operators (`&&` `||` `!`)
  - [x] Floats
  - [x] Strings
    - [x] String declaration
    - [x] String arithmetics (`+` concatenation)
    - [x] Subscripting (`s[i]`)
    - [x] Slicing (`s[lo..hi]`)
  - [x] Arrays
    - [x] Array literals
    - [x] Array concatenation (`+`)
    - [x] Subscripting (`a[i]`)
    - [x] Slicing (`a[lo..hi]`)
  - [x] Structs
  - [x] `panic()` builtin
  - [x] `//` line comments
  - [ ] Bitwise operators (`&` `|` `^` `~` `<<` `>>`)
  - [ ] `for` loop
  - [ ] Standard library (`open`, `read`, `write`)
  - [ ] Import system

- [x] Interpreter
  - [x] Global statements
  - [x] Expressions
  - [x] Integer arithmetic (`+` `-` `*` `/` `%`)
  - [x] Comparison operators (`<` `<=` `==` `!=` `>=` `>`)
  - [x] Logical operators (`&&` `||` `!`) with short-circuit evaluation
  - [x] Functions
  - [x] Global hoisted declarations
  - [x] Calls with argument passing
  - [x] Recursion (call depth limit: 1000)
  - [x] Control flow
    - [x] If / else if / else block
    - [x] While block with break and continue
  - [x] Typing rules
    - [x] Truthiness for all types (integer, float, string, array, object, void)
    - [x] Unified binary op dispatch (int/float/string/array/void)
    - [x] Int/float promotion in mixed arithmetic
    - [x] Unary `-` and `!` for all types
  - [x] Strings
    - [x] String equality (`==` / `!=`)
    - [x] String concatenation (`+`)
    - [x] Subscripting (`s[i]` → 1-char string, bounds-checked)
    - [x] Slicing (`s[lo..hi]` → substring, bounds-checked)
  - [x] Floats (`f64`, int/float promotion)
  - [x] Arrays
    - [x] Array literals (`[]`, `[e1, e2, ...]`)
    - [x] Array concatenation (`a + b` → new array)
    - [x] Subscripting (read `a[i]` + write `a[i] = x`, bounds-checked)
    - [x] Slicing (`a[lo..hi]` → new array, bounds-checked)
  - [x] Structs (declaration, literal, field read/write)
  - [x] Runtime error trace (error name + source line + call chain)
  - [x] `panic(msg)` builtin
  - [ ] Bitwise operators (`&` `|` `^` `~` `<<` `>>`)
  - [ ] `for` loop
  - [ ] Standard library
    - [ ] `print(val)` / `println(val)`
    - [ ] `len(s_or_arr)`
    - [ ] `int(x)` / `float(x)` / `str(x)` — type coercion
    - [ ] `open(path, mode)` / `read(fd)` / `write(fd, val)`

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
