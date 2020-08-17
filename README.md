# PiG

Interpreter for a very simple language.

## Usage  

* Run `stack exec -- PiG` to start interpreter
* With `stack exec -- PiG --verbose` to start interpreter with more logging.

Inside of the interpreter one can declare variables (true/false, numbers, lists and lambdas), like in the example:

```
x = true
y = 10.2
z = [x,y]
v = y + 2
t = (a) => a
s = (a,b) => { c = a + b; return c}
```

One can also write statements like `when ... do ...`, `if ... then ... [else ...]` and sequences of statements separated by `;`, 
optionaly enclosed in braces. Writing single expression will effect in writing result of this expression to stdout. 

Supported operators are:
* `+`, `-`, `*`, `/` and `^` for numbers
* `-`, `||`, `&&` for booleans
* `<>`, `-<`, `>-` for lists

### Dependencies
* **[RIO](https://hackage.haskell.org/package/rio)**
* **[Parsec](https://hackage.haskell.org/package/parsec)**
* **[Haskeline](https://hackage.haskell.org/package/haskeline)**
* **[pretty-terminal](https://github.com/loganmac/pretty-terminal)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
