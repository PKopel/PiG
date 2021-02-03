module Lang.Tokens where

data TokenType = TIf
           | TElIf
           | TElse
           | TWhile
           | TDo
           | TPrint
           | TRead
           | TExit
           | THelp
           | TRM
           | TClear
           | TLoad
           | TPlus
           | TMinus
           | TStar
           | TSlash
           | TFatArr
           | TAssign
           | TDash
           | TLt
           | TGt
           | TEq
           | TNEq
           | TLtGt
           | TGtLt
           | TRFork
           | TLFork
           | TAnd
           | TOr
           | TRParen
           | TLParen
           | TRBrace
           | TLBrace
           | TRBracket
           | TLBracket
           | TTrue
           | TFalse
           | TNull
           | TNum Double
           | TStr String
           | TChar Char
           | TSym String
           | TComma
           | TSemi
           | TEOF

instance Show TokenType where
  show TIf       = "if"
  show TElIf     = "elif"
  show TElse     = "else"
  show TWhile    = "while"
  show TDo       = "do"
  show TPrint    = "print"
  show TRead     = "read"
  show TExit     = ":exit"
  show THelp     = ":help"
  show TRM       = ":rm"
  show TClear    = ":clear"
  show TLoad     = ":load"
  show TPlus     = "+"
  show TMinus    = "-"
  show TStar     = "*"
  show TSlash    = "/"
  show TFatArr   = "=>"
  show TAssign   = "="
  show TDash     = "^"
  show TLt       = "<"
  show TGt       = ">"
  show TEq       = "=="
  show TNEq      = "!="
  show TLtGt     = "<>"
  show TGtLt     = "><"
  show TRFork    = "-<"
  show TLFork    = ">-"
  show TAnd      = "&&"
  show TOr       = "||"
  show TRParen   = ")"
  show TLParen   = "("
  show TRBrace   = "}"
  show TLBrace   = "{"
  show TRBracket = "]"
  show TLBracket = "["
  show TTrue     = "true"
  show TFalse    = "false"
  show TNull     = "null"
  show (TNum  x) = show x
  show (TStr  s) = s
  show (TChar c) = [c]
  show (TSym  s) = s
  show TComma    = ","
  show TSemi     = ";"
  show TEOF      = "EOF"
