# PiG

Interpreter for a simple language.

## Usage  

* Run `stack exec -- PiG` to start interpreter
* With `stack exec -- PiG -f|--file <file path>` to start interpreter with <file> loaded.
* With `stack exec -- PiG --help` to see more info.

Inside of the interpreter one can declare variables (true/false, numbers, lists and lambdas), like in the example:

```
x = true
y = 10.2
z = [x,y]
v = y + 2
t = (a) => a
s = (a,b) => { c = a + b; c}
```

One can also write statements like `when ... do ...`, `if ... then ... [else ...]` and sequences of statements separated by `;`, 
optionaly enclosed in braces. Writing single expression will effect in writing result of this expression to stdout. 

Supported operators are:
* `+`, `-`, `*`, `/` and `^` for numbers
* `-`, `||`, `&&` for booleans
* `<>`, `-<`, `>-` for lists (`>-` removes and returns first element, `-<` the last one)

It is also possible to load file via `:load "<file path>"`.

See [examples](https://github.com/PKopel/PiG/tree/master/examples) for more info.

### Dependencies
* **[RIO](https://hackage.haskell.org/package/rio)**
* **[Parsec](https://hackage.haskell.org/package/parsec)**
* **[Haskeline](https://hackage.haskell.org/package/haskeline)**
* **[pretty-terminal](https://github.com/loganmac/pretty-terminal)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
