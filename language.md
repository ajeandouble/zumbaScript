# Zumbascript Language Reference

Zumbascript is a dynamically-typed, expression-oriented scripting language.
Programs are a sequence of global variable assignments followed by one or more function declarations.
Execution starts at global scope (assignments run top-to-bottom) and then calls `main` if present.

---

## Literals

```zumba
x = 42;         # integer
y = 3.14;       # float  (planned)
s = "hello";    # string
a = [1, 2, 3];  # array   (planned)
empty = [];     # empty array (planned)
```

---

## Variables

Variables are declared on first assignment. There is no explicit type annotation.

```zumba
counter = 0;
name = "alice";
```

---

## Arithmetic

```zumba
function main() {
    a = 10 + 3;    # 13
    b = 10 - 3;    # 7
    c = 10 * 3;    # 30
    d = 10 / 3;    # 3  (integer division)
    e = 10 % 3;    # 1
    f = -a;        # unary minus
    return 0;
}
```

---

## Comparisons & logical operators

Comparison and logical operators return `1` (true) or `0` (false).

```zumba
function main() {
    a = 1 < 2;      # 1
    b = 2 <= 2;     # 1
    c = 3 == 3;     # 1
    d = 3 != 4;     # 1
    e = 4 >= 5;     # 0
    f = 4 > 5;      # 0
    g = 1 && 0;     # 0  (short-circuits)
    h = 0 || 1;     # 1  (short-circuits)
    i = !0;         # 1
    return 0;
}
```

---

## Functions

All function declarations are hoisted — order in the file does not matter.
Functions must be declared at the top level. Nested functions are not supported.

```zumba
function add(x, y) {
    return x + y;
}

function main() {
    result = add(10, 32);
    return result;    # exits with code 42
}
```

Variadic functions are not supported; all declared parameters must be passed.

---

## Control flow — if / else if / else

```zumba
function sign(n) {
    if (n < 0) {
        return -1;
    } else if (n == 0) {
        return 0;
    } else {
        return 1;
    }
}

function main() {
    return sign(-5);    # -1
}
```

---

## Control flow — while / break / continue

```zumba
function sum_to(n) {
    i = 0;
    acc = 0;
    while (i <= n) {
        acc = acc + i;
        i = i + 1;
    }
    return acc;
}

function first_even_above(threshold) {
    n = threshold + 1;
    while (1) {
        if (n % 2 == 0) {
            break;
        }
        n = n + 1;
    }
    return n;
}

function main() {
    return sum_to(10);    # 55
}
```

---

## Strings _(declaration implemented; operations planned)_

```zumba
function main() {
    greeting = "hello";
    name = "world";

    # Planned:
    # message = greeting + ", " + name;   # concatenation
    # ch = greeting[0];                   # subscript → "h"
    # sub = greeting[1..3];               # slice     → "el"

    return 0;
}
```

---

## Arrays _(planned)_

```zumba
function main() {
    nums = [10, 20, 30];

    # Planned:
    # first = nums[0];        # subscript read  → 10
    # nums[1] = 99;           # subscript write
    # sub = nums[0..2];       # slice → [10, 99]
    # merged = nums + [40];   # concat → [10, 99, 30, 40]

    return 0;
}
```

---

## Global statements

Assignments at file scope run before any function is called.

```zumba
PI = 3;          # global constant (integer approximation)
TWO_PI = PI * 2;

function circle_area(r) {
    return PI * r * r;
}

function main() {
    return circle_area(5);    # 75
}
```

---

## Operator precedence (high → low)

| Level | Operators                       | Associativity |
| ----- | ------------------------------- | ------------- |
| 1     | unary `+` `-` `!`               | right         |
| 2     | `*` `/` `%`                     | left          |
| 3     | `+` `-`                         | left          |
| 4     | `<` `<=` `==` `!=` `>=` `>`     | left          |
| 5     | `&&`                            | left          |
| 6     | `\|\|`                          | left          |
| 7     | `=`                             | right         |
