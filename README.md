# PiG

Interpreter for a very simple language. The grammar is defined as follows:
```
A ::= x | n | -A | A opa A | ( A )

B ::= true | false | not B | B opb B | A opr A

opa ::= + | - | * | / | ^

opb ::= and | or

opr ::= > | <

S ::= x = A | { S; } S | ( S ) | while A do S | while B do S | if A then S [else S] | if B then S [else S] | print A | print B | exit
```
## Usage  

* Run `stack exec -- PiG` to start interpreter
* With `stack exec -- PiG --verbose` to start interpreter with more logging.

### Dependencies
* **[Attoparsec](https://hackage.haskell.org/package/attoparsec)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
