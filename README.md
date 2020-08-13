# PiG

Interpreter for a very simple language. The grammar is defined as follows:
```
A = x | n | "-" A | A opa A | "(" A ")"

B = "true" | "false" | "~" B | B opb B | A opr A

L = "[" [ {E ","} E] "]" | L opl E | "-<" L | "->" L

E = A | B | L

opa = "+" | "-" | "*" | "/" | "^"

opb = "&&" | "||"

opr = ">" | "<" | "=="

opl = "+>" | "+<"

S = x "=" A | S ";" S | "{" S "}" | "while" E "do" S | "if" E "then" S ["else" S] | "print" E 
```
## Usage  

* Run `stack exec -- PiG` to start interpreter
* With `stack exec -- PiG --verbose` to start interpreter with more logging.

### Dependencies
* **[RIO](https://hackage.haskell.org/package/rio)**
* **[Parsec](https://hackage.haskell.org/package/parsec)**
* **[Haskeline](https://hackage.haskell.org/package/haskeline)**

### Author

* **[Paweł Kopel](https://github.com/PKopel)**
