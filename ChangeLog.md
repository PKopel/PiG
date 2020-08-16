# Changelog for PiG

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
