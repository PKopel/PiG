# Changelog for PiG

### 0.15.0
* change order of monads in Interp to allow for auto completion

### 0.14.2
* change `--file` to `--load` to make it more consistent with language

### 0.14.1
* change implementation of read, print, load and exit

### 0.14.0
* change implementation of directives
* split interpretation of directives and programs

### 0.13.1
* remove Print & Read constructors, use FunApp "print" and FunApp "read" instead
* add one letter shortcuts for directives

### 0.13.0
* rewrite parser and lexer with Alex and Happy

### 0.12.2
* small refactorization

### 0.12.1
* `print` doesn't add '\n' at the end of line
* empty line in interpreter doesn't cause error

### 0.12.0
* interpreter doesn't print nulls

### 0.11.0
* `:exit` directive working properly in files

### 0.10.4
* fixed using directives in files

### 0.10.3
* simplified `read()`

### 0.10.2
* `read` expression for getting input

### 0.10.1
* chains of relations, like `1 < 2 > 3`, are now handled properly

### 0.10.0
* `==`, `!=`, `>` and `<` working for every type except for lambdas

### 0.9.0
* chars and strings

### 0.8.0
* reworked type system with more flexible operators and less boilerplate

### 0.7.1
* changed Store related functions to use more lenses

### 0.7.0
* changed Store type to simplify scoping

### 0.6.6
* replacing nth element in list by `list(n) = expr`

### 0.6.5 
* assignment is now an expression
* `print` is back, now as a function, but plain expressions in interpreter are still shown after evaluation

### 0.6.4
* colors in interpreter
* fixed crash on loading file that can't be read

### 0.6.3
* list literals can now contain expressions, expressions can contain function application
* fixed variable scopes in functions

### 0.6.2
* new cli option `[-f|--file FILE]` for loading files on start of interpreter
* simplified types

### 0.6.1
* loading files from other files

### 0.6.0
* saving sequences to files and loading files to interpreter

### 0.5.0
* interpreter directives: `:help`,`:exit`,`:clear`,`:rm <var name>`,`:load "<file path>"`

### 0.4.4
* accesing list elements by index, ex: `list = [1,2]; list(0)`

### 0.4.3
* add `null` literal

### 0.4.2
* function declarations changed from `\<name>(<args>)` to `<name> = (<args>) =>`
* functions now return last expression (if it is the only part of body) or expression marked by `return` (must be last in body)

### 0.4.1
* fixed variable scopes in functions
* removed `print` keyword, interpreter now prints result of evaluation of expression written as statement, ex: `1 + 2` will result in printing `3.0`
