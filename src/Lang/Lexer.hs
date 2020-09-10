{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Lexer where

import           Import
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Expr
                                                ( Assoc(AssocLeft)
                                                , Operator(Infix, Prefix)
                                                )
import           Text.ParserCombinators.Parsec.Language
                                                ( GenLanguageDef
                                                , emptyDef
                                                )
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
reservedNms =
  [ "if"
  , "then"
  , "else"
  , "while"
  , "do"
  , "print"
  , "read"
  , "true"
  , "false"
  , "null"
  , ":exit"
  , ":help"
  , ":rm"
  , ":clear"
  , ":load"
  ]

reservedOps :: [String]
reservedOps =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "=>"
  , "="
  , "^"
  , "<"
  , ">"
  , "=="
  , "!="
  , "<>"
  , "><"
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
brackets p = Token.brackets lexer (skipMany endOfLine *> p) -- parses a sequence in brackets

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens p = Token.parens lexer (skipMany endOfLine *> p) -- parses a sequence in parenthesis

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces p = Token.braces lexer (skipMany endOfLine *> p) -- parses a sequence in braces

double :: ParsecT String u Identity Double
double = Token.float lexer -- parses a double

integer :: ParsecT String u Identity Integer
integer = Token.integer lexer -- parses an integer

stringLiteral :: ParsecT String u Identity String
stringLiteral = Token.stringLiteral lexer -- parses a string

charLiteral :: ParsecT String u Identity Char
charLiteral = Token.charLiteral lexer -- parses a char

semi :: ParsecT String u Identity String
semi = Token.semi lexer <* skipMany endOfLine -- parses a semicolon

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep p = Token.commaSep lexer (skipMany endOfLine *> p) -- parses a list separated by comma

whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

algOperators :: [[Operator Char st Expr]]
algOperators =
  [ [Prefix (reservedOp "-" >> return (Unary (negate :: Double -> Double)))]
  , [ Infix
        (reservedOp "^" >> return (Binary ((**) :: Double -> Double -> Double)))
        AssocLeft
    ]
  , [ Infix
      (reservedOp "*" >> return (Binary ((*) :: Double -> Double -> Double)))
      AssocLeft
    , Infix
      (reservedOp "/" >> return (Binary ((/) :: Double -> Double -> Double)))
      AssocLeft
    ]
  , [ Infix
      (reservedOp "+" >> return (Binary ((+) :: Double -> Double -> Double)))
      AssocLeft
    , Infix
      (reservedOp "-" >> return (Binary ((-) :: Double -> Double -> Double)))
      AssocLeft
    ]
  ]

boolOperators :: [[Operator Char st Expr]]
boolOperators =
  [ [Prefix (reservedOp "-" >> return (Unary not))]
  , [ Infix (reservedOp "&&" >> return (Binary (&&))) AssocLeft
    , Infix (reservedOp "||" >> return (Binary (||))) AssocLeft
    ]
  ]

listOperators :: [[Operator Char st Expr]]
listOperators =
  [ [ Prefix (reservedOp ">-" >> return (Unary (>-)))
    , Prefix (reservedOp "-<" >> return (Unary (-<)))
    ]
  , [ Infix
        (  reservedOp "<>"
        >> return (Binary ((<>) :: Seq Val -> Seq Val -> Seq Val))
        )
        AssocLeft
    ]
  ]

strOperators :: [[Operator Char st Expr]]
strOperators =
  [ [ Infix
        (reservedOp "><" >> return (Binary ((<>) :: String -> String -> String))
        )
        AssocLeft
    ]
  ]
