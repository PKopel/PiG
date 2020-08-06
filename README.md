# PiG

Interpreter for very simple language. The grammar is defined as follows:
```
a  ::= x | n | a opa a | ( a )

opa ::= + | - | * | / | ^

S  ::= x = a | S1; S2 | while b do S done | print a | exit
```
## Usage  

* Run `stack exec -- PiG-exe` to start interpreter
* With `stack exec -- PiG-exe --verbose` to start interpreter with more logging.

### Dependencies
* **[Attoparsec](https://hackage.haskell.org/package/attoparsec)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
