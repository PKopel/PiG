{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Lexer where

import           Control.Monad.Identity
import           Import
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token
                                                ( GenTokenParser )
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef { Token.commentStart    = "/*"
                       , Token.commentEnd      = "*/"
                       , Token.commentLine     = "//"
                       , Token.identStart      = letter
                       , Token.identLetter     = alphaNum
                       , Token.reservedNames   = reservedNms
                       , Token.reservedOpNames = reservedOps
                       }

reservedNms :: [String]
reservedNms = ["if", "then", "else", "while", "do", "skip", "true", "false"]

reservedOps :: [String]
reservedOps =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "\\"
  , "="
  , "^"
  , "<"
  , ">"
  , "=="
  , "#"
  , "-<"
  , ">-"
  , "&&"
  , "||"
  ]

lexer :: GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

identifier :: ParsecT String u Identity String
identifier = Token.identifier lexer -- parses an identifier

reserved :: String -> ParsecT String u Identity ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Token.reservedOp lexer -- parses an operator

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = Token.brackets lexer -- parses a sequence in brackets

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Token.parens lexer -- parses a sequence in parenthesis

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = Token.braces lexer -- parses a sequence in braces

double :: ParsecT String u Identity Double
double = Token.float lexer -- parses a double

integer :: ParsecT String u Identity Integer
integer = Token.integer lexer -- parses an integer

semi :: ParsecT String u Identity String
semi = Token.semi lexer -- parses a semicolon

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = Token.commaSep lexer -- parses a list separated by comma

whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

algOperators :: [[Operator Char st Expr]]
algOperators =
  [ [Prefix (reservedOp "-" >> return (Neg))]
  , [Infix (reservedOp "^" >> return (AlgBinary Power)) AssocLeft]
  , [ Infix (reservedOp "*" >> return (AlgBinary Multiply)) AssocLeft
    , Infix (reservedOp "/" >> return (AlgBinary Divide))   AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (AlgBinary Add))      AssocLeft
    , Infix (reservedOp "-" >> return (AlgBinary Subtract)) AssocLeft
    ]
  ]

boolOperators :: [[Operator Char st Expr]]
boolOperators =
  [ [Prefix (reservedOp "-" >> return (Neg))]
  , [ Infix (reservedOp "&&" >> return (BoolBinary And)) AssocLeft
    , Infix (reservedOp "||" >> return (BoolBinary Or))  AssocLeft
    ]
  ]

listOperators :: [[Operator Char st Expr]]
listOperators =
  [ [ Prefix (reservedOp ">-" >> return (ListUnary RmFirst))
    , Prefix (reservedOp "-<" >> return (ListUnary RmLast))
    ]
  , [Infix (reservedOp "#" >> return (ListBinary Concat)) AssocLeft]
  ]
