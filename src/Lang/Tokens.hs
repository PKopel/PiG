module Lang.Tokens where

data Token = TIf
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
           