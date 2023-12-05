# PiG

![tests](https://github.com/PKopel/PiG/actions/workflows/test.yaml/badge.svg)
![release](https://github.com/PKopel/PiG/actions/workflows/release.yaml/badge.svg)

Interpreter for a simple language, build with [Alex](https://www.haskell.org/alex/) and [Happy](https://www.haskell.org/happy/).

## Usage  

Use

* `stack exec pig` to start interpreter
* `stack install` to install `pig` executable on your machine

### Options

* `--version` show version of interpreter
* `--help`: show options with short description
* `-l|--load FILE`: start interpreter with `FILE` loaded

Command line arguments for scripts are stored in a list of strings named `args`. To distinguish script arguments from intepreter's arguments use `--`, for example with command `pig -l script.pig -- help` string `help` will be available in the script as `args(0)`.

PiG interpreter can also be used with shebang with `#!<path>/<to>/<pig> -l`, see [example](./examples/hello.pig).

## Language

In PiG, everything is an expression:

* literal values:
  * `null`
  * `true`/`false`
  * numbers (integral and real)
  * characters like `'a'` and strings like `"abcd"`
  * functions (lambdas) in form `(<arg1>,...,<argn>) => <sequence>`. Return value of a function is either the value of its last expression or value explicitly marked with the `return` keyword.
  * lists in form `[<expr1>,...,<exprn>]`
  * dictionaries (maps) in form `[<key1>: <value1>,...,<keyn>: <valuen]`
* assignments: `<name> = <expr>`, where name consists of alphanumeric characters, `<name>(<number>) = <expr>` to assign value to a specific element of a list, or `<name>(<expr>) = <expr>` to add key-value pair to a dictionary. Value of assignment is the value of expression on the right.
* sequence of expressions, separated and optionally ended by `;`. When sequence is enclosed by braces (`{...}`), it is treated as a single expression (sequences can be nested, like `{ <expr1>; { <expr2>; <expr3> }; }`). Value of a sequence is the value of the last expression in it.
* while loop: `while <expr1> : <expr2>`. Both `<expr1>` and `<expr2>` must be a single expressions, but they don't have to be enclosed in braces. `<expr2>` will be executed as long as `<expr1>` is a `true`, non-zero number, non-empty list or a non-empty string. Value of a while expression is a list of values of `<expr2>` (for example value of `x = 3; while x > 0 : x = x - 1` is `[2,1,0]`).
* if: `if <expr> : <expr> elif <expr> : <expr> ... else : <expr>` (`elif` and `else` are optional). Value of "if" is the value of expression after ifrst condition evaluated to `true`, non-zero number, non-empty list or a non-empty string, or value of expresion after `else` (`null` if no `else` is specified).
* a variable name, as in assignment. If the variable is bound to a list, `<name>(<num1>,...,<numn>)` syntax can be used to get the value at specified index or a list of them.
* function application, in form `<name>(<arg1>,...,<argn>)`. If the `<name>` is a list, arguments that can be evaluated to numbers will be rounded to integers and treated as zero-based indices, and the return value will be the value associated with that index or a list of values in case of more than one indices (for example a value of `l = [1,2,3]; l(0)` is `1`, value of `l = [1,2,3]; l(0,2)` is `[1,3]`). If the `<name>` is a dictionary, the return value will be the value associated with the argument or a list of values in case of more than one argument (for example a value of `m = ["a":1,"b":3]; m("a")` is `1`, value of `m = ["a":1,"b":3]; m("a","b")` is `[1,3]`).
* expressions with build-in keywords and operators

Build-in functions provided:

* `print(arg1,...,argn)` prints all its arguments to stdout
* `read()` reads string from stdin
* `open(<file path>[, <mode string>])` opens file and returns its handle
* `close(<handle>)` closes file
* `readFile(<handle>)` reads line form file
* `writeFile(<hande>,arg1,...,argn)` writes all its arguments except handle to file
* `strToNum(arg)` parses number from string
* `strToList(arg)` turns string to a list of chars
* `listToStr(arg)` turns a list of chars to string
* `isNum(arg)` checks if `arg` is a number
* `isBool(arg)` checks if `arg` is a boolean
* `isList(arg)` checks if `arg` is a list
* `isStr(arg)` checks if `arg` is a string
* `isFun(arg)` checks if `arg` is a function
* `exit()` closes interpreter

Keyword `load "<file name>"` executes code from other files.

Build-in operators are:

* `+`, `-`, `*`, `/`, `^` and `%` (modulo) for numbers
* `-`, `||`, `&&` for booleans
* `<>`, `-<`, `>-` for lists (`>-` removes and returns first element, `-<` the last one, `<>` concatenates second argument to the end of the first one, works with any type but results always in a list)
* `><` for strings (attaches second argument at the end of the firs one, works with any type but results always in a string)

Interpreter also provides four directives:

* `:exit | :e` (or Ctrl+d) to leave the interpreter
* `:rm <variable name>` to remove binding of a variable
* `:clear | :c` to remove all bindings
* `:help | :h` to display information about directives

See [examples](https://github.com/PKopel/PiG/tree/master/examples) for more info.

## Author

* **[Paweł Kopel](https://github.com/PKopel)**
