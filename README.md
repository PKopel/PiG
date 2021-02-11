# PiG

Interpreter for a simple language, build with [Alex](https://www.haskell.org/alex/) and [Happy](https://www.haskell.org/happy/).

## Usage  

* Run `stack exec pig` to start interpreter
* With `stack exec pig -- -f|--file <file path>` to start interpreter with <file> loaded.
* With `stack exec pig -- --help` to see more info.

## Language

In PiG, everything is an expression:

* literal values: 
    * `null`
    * `true`/`false`
    * numbers (integral and real)
    * characters like `'a'` and strings like `"abcd"`
    * functions (lambdas) in form `(<arg1>,...,<argn>) => <sequence>`
    * lists in form `[<expr1>,...,<exprn>]`
* assignments: `<name> = <expr>`, where name consists of alphanumeric characters, or `<name>(<number>) = <expr>` to assign value to a specific element of a list. Value of assignment is the value of expression on the right
* sequence of expressions, separated and optionally ended by `;`. When sequence is enclosed by braces (`{...}`), it is treated as a single expression (sequences can be nested, like `{ <expr1>; { <expr2>; <expr3> }; }`). Value of a sequence is the value of the last expression in it
* while loop: `while <expr1> do <expr2>`. Both `<expr1>` and `<expr2>` must be a single expressions, but they don't have to be enclosed in braces. `<expr2>` will be executed as long as `<expr1>` is a `true`, non-zero number, non-empty list or a non-empty string. Value of a while expression is a list of values of `<expr2>` (for example `x = 3; while x > 0 do x = x - 1` will result in `[2,1,0]`)
* if: `if <expr> do <expr> elif <expr> do <expr> ... else <expr>` (`elif` and `else` are optional). Value of "if" is the value of expression after ifrst condition evaluated to `true`, non-zero number, non-empty list or a non-empty string, or value of expresion after `else` (`null` if no `else` is specified).
* a variable name, as in assignment. If the variable is bound to a list, `<name>(<num1>,...,<numn>)` syntax can be used to get the value at specified index or a list of them.
* function application, in form `<name>(<arg1>,...,<argn>)`
* expressions with build-in operators

There are four build-in functions provided: `print(arg1,...,argn)` which prints all its arguments to stdout, `read()`, which reads string from stdin, `load(<file name 1>, ... , <file name n>)` which loads other files and `exit()` which closes interpreter.

Build-in operators are:
* `+`, `-`, `*`, `/` and `^` for numbers
* `-`, `||`, `&&` for booleans
* `<>`, `-<`, `>-` for lists (`>-` removes and returns first element, `-<` the last one, `<>` concatenates second argument to the end of the first one, works with any type but results always in a list)
* `><` for strings (attaches second argument at the end of the firs one, works with any type but results always in a string)

Interpreter also provides five directives:
* `:load | :l "<file path>"` to load file
* `:exit | :e` (or Ctrl+d) to leave the interpreter
* `:rm <variable name>` to remove binding of a variable
* `:clear | :c` to remove all bindings
* `:help | :h` to display information about directives

See [examples](https://github.com/PKopel/PiG/tree/master/examples) for more info.

### Dependencies
* **[RIO](https://hackage.haskell.org/package/rio)**
* **[Haskeline](https://hackage.haskell.org/package/haskeline)**
* **[pretty-terminal](https://github.com/loganmac/pretty-terminal)**
* **[hspec](https://hspec.github.io)**
* **[QuickCheck](https://hackage.haskell.org/package/QuickCheck)**
* **[array](https://gitlab.haskell.org/ghc/packages/array)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
