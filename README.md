# PiG

Interpreter for a simple language.

## Usage  

* Run `stack exec -- PiG` to start interpreter
* With `stack exec -- PiG -f|--file <file path>` to start interpreter with <file> loaded.
* With `stack exec -- PiG --help` to see more info.

## Language

In PiG, everything is an expression. Allowed are:

* literal values: 
    * `null`
    * `true`/`false`
    * numbers (integral and real)
    * characters like `'a'` and strings like `"abcd"`
    * functions (lambdas) in form `(arg1,...,argn) => expr`
    * lists in form `[expr1,...,exprn]`
* assignments: `<name> = <expr>`, where name consists of alphanumeric characters, or `name(number) = expr` to assign value to a specific element of a list. Value of assignment is the value of expression on the right
* sequence of expressions, separated and optionally ended by `;`. When sequence is enclosed by braces (`{...}`), it is treated as a single expression (sequences can be nested, like `{ expr1; {expr2;expr3};}`). Value of a sequence is the value of the last expression in it
* while loop: `while expr1 do expr2`. Both `expr1` and `expr2` must be a single expressions, but they don't have to be enclosed in braces. `expr2` will be executed as long as `expr1` is a `true`, non-zero number, non-empty list or a non-empty string. Value of a while expression is a list of values of `expr2` (for example `x = 3; while x > 0 do x = x - 1` will result in `[2,1,0]`)
* if: `if expr1 then expr2 else expr3` (`else` is optional). Value of "if" is `expr2` if `expr1` is a `true`, non-zero number, non-empty list or a non-empty string, otherwise `expr3` or `null` if no `else` is specified.
* a variable name, as in assignment
* function application, in form `<name>(arg1,...,argn)`
* expressions with build-in operators

There are two build-in functions provided: `print(arg1,...,argn)` which prints all its arguments followed to stdout and starts a new line, and `read()`, which reads one literal value from stdin.

Build-in operators are:
* `+`, `-`, `*`, `/` and `^` for numbers
* `-`, `||`, `&&` for booleans
* `<>`, `-<`, `>-` for lists (`>-` removes and returns first element, `-<` the last one)

Interpreter also provides five directives:
* `:load "<file path>"` to load file
* `:exit` (or Ctrl+d) to leave the interpreter
* `:rm <variable name>` to remove binding of a variable
* `:clear` to remove all bindings
* `:help` to display information about directives

Directives can also be used in files.

See [examples](https://github.com/PKopel/PiG/tree/master/examples) for more info.

### Dependencies
* **[RIO](https://hackage.haskell.org/package/rio)**
* **[Parsec](https://hackage.haskell.org/package/parsec)**
* **[Haskeline](https://hackage.haskell.org/package/haskeline)**
* **[pretty-terminal](https://github.com/loganmac/pretty-terminal)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
