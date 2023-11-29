# Changelog for PiG

## Changes

### 0.1.8

* Remove unused dependency 'containers"

### 0.1.7

* Change `do` to `:` in `if`s and `while`s

### 0.1.6

* update ghc version to 9.6.3
* improve conversion between numbers and strings

### 0.1.5

* improve parser to handle chained function calls, e.g. `fun(0)()`
* improve directive's parser error message

### 0.1.4

* handle `return` keyword outside of function bodies
* update `exit` function to handle status codes

### 0.1.3

* add `return` keyword

### 0.1.1

* fix prompt on windows

### 0.1.0

* change versioning convention
* add `strToList` and `listToStr` bifs

### 0.0.20.0

* fix `open` and `readFile` functions

### 0.0.19.0

* add `open`, `close`, `readFile` and `writeFile` functions and file handle type

### 0.0.18.2

* treat non-lambda values as constant functions

### 0.0.18.1

* move contents of module Utils.Types.App to Utils.Types
* fix `catList` & `catStr`
* improve autocompletion

### 0.0.18.0

* remove Unary and Binary constructors from Expr
* remove classes UnaryOp and BinaryOp
* add bifs for operators

### 0.0.17.4

* rename Interp to REPL
* change bifs to `Map Text ([Val] -> Interp a Val)`

### 0.0.17.3

* replace ":print" with expresion in PiG
* add bifs `is<type>`

### 0.0.17.2

* fix Interp.Directives.Parser
* move BIF.hs from Interp to Lang

### 0.0.17.1

* rename runWithStore* and read/writeVar
* remove option `-v|--verbose`

### 0.0.17.0

* change lexer and parser to use Text.Lazy
* add Utils.IO to simplify IO
* remove redundant functions from Utils.Interp
* change directives parser to use Attoparsec

### 0.0.16.1

* replace bare IO with RIO App

### 0.0.16.0

* change `load` from function to keyword, remove `:load` directive
* move bifs to module Interp.BIF
* remove modulw Import

### 0.0.15.2

* add modulo operator (`%`)
* add `strToNum`

### 0.0.15.1

* implement code completion

### 0.0.15.0

* change order of monads in Interp to allow for auto completion

### 0.0.14.2

* change `--file` to `--load` to make it more consistent with language

### 0.0.14.1

* change implementation of read, print, load and exit

### 0.0.14.0

* change implementation of directives
* split interpretation of directives and programs

### 0.0.13.1

* remove Print & Read constructors, use FunApp "print" and FunApp "read" instead
* add one letter shortcuts for directives

### 0.0.13.0

* rewrite parser and lexer with Alex and Happy

### 0.0.12.2

* small refactorization

### 0.0.12.1

* `print` doesn't add '\n' at the end of line
* empty line in interpreter doesn't cause error

### 0.0.12.0

* interpreter doesn't print nulls

### 0.0.11.0

* `:exit` directive working properly in files

### 0.0.10.4

* fixed using directives in files

### 0.0.10.3

* simplified `read()`

### 0.0.10.2

* `read` expression for getting input

### 0.0.10.1

* chains of relations, like `1 < 2 > 3`, are now handled properly

### 0.0.10.0

* `==`, `!=`, `>` and `<` working for every type except for lambdas

### 0.0.9.0

* chars and strings

### 0.0.8.0

* reworked type system with more flexible operators and less boilerplate

### 0.0.7.1

* changed Store related functions to use more lenses

### 0.0.7.0

* changed Store type to simplify scoping

### 0.0.6.6

* replacing nth element in list by `list(n) = expr`

### 0.0.6.5

* assignment is now an expression
* `print` is back, now as a function, but plain expressions in interpreter are still shown after evaluation

### 0.0.6.4

* colors in interpreter
* fixed crash on loading file that can't be read

### 0.0.6.3

* list literals can now contain expressions, expressions can contain function application
* fixed variable scopes in functions

### 0.0.6.2

* new cli option `[-f|--file FILE]` for loading files on start of interpreter
* simplified types

### 0.0.6.1

* loading files from other files

### 0.0.6.0

* saving sequences to files and loading files to interpreter

### 0.0.5.0

* interpreter directives: `:help`,`:exit`,`:clear`,`:rm <var name>`,`:load "<file path>"`

### 0.0.4.4

* accesing list elements by index, ex: `list = [1,2]; list(0)`

### 0.0.4.3

* add `null` literal

### 0.0.4.2

* function declarations changed from `\<name>(<args>)` to `<name> = (<args>) =>`
* functions now return last expression (if it is the only part of body) or expression marked by `return` (must be last in body)

### 0.0.4.1

* fixed variable scopes in functions
* removed `print` keyword, interpreter now prints result of evaluation of expression written as statement, ex: `1 + 2` will result in printing `3.0`
