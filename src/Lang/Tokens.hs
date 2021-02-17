{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Tokens where

import           RIO                            ( Show(..)
                                                , Char
                                                , Double
                                                )
import           Data.Text.Lazy                 ( Text
                                                , unpack
                                                )

data TokenType = TIf
           | TElIf
           | TElse
           | TWhile
           | TDo
           | TLoad
           | TPlus
           | TMinus
           | TStar
           | TSlash
           | TMod
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
           | TNot
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
           | TStr Text
           | TChar Char
           | TSym Text
           | TComma
           | TSemi
           | TEOF

instance Show TokenType where
  show TIf       = "if"
  show TElIf     = "elif"
  show TElse     = "else"
  show TWhile    = "while"
  show TDo       = "do"
  show TLoad     = "load"
  show TPlus     = "+"
  show TMinus    = "-"
  show TStar     = "*"
  show TSlash    = "/"
  show TMod      = "%"
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
  show TNot      = "~"
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
  show (TStr  s) = unpack s
  show (TChar c) = [c]
  show (TSym  s) = unpack s
  show TComma    = ","
  show TSemi     = ";"
  show TEOF      = "EOF"
