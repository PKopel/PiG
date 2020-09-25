{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Parser where

import           Data.Sequence                  ( fromList )
import           Import                  hiding ( many
                                                , optional
                                                , try
                                                , (<|>)
                                                )
import           Lang.Lexer
import           Text.Parsec
import           Text.Parsec.String             ( Parser
                                                , parseFromFile
                                                )
import           Text.ParserCombinators.Parsec.Expr
                                                ( buildExpressionParser )

parseProg :: String -> Either String Prog
parseProg p = case parse progParser "PiG" p of
  Left  err  -> Left $ show err
  Right prog -> Right prog

parseFile :: FilePath -> IO (Either String [Prog])
parseFile f = parseFromFile (many1 progParser <* eof) f >>= \case
  Left  err  -> return . Left $ show err
  Right prog -> return $ Right prog

parseLitVal :: String -> Either String Val
parseLitVal s = case parse litValParser "PiG" s of
  Left  err -> Left $ show err
  Right lit -> Right lit

progParser :: Parser Prog
progParser = whiteSpace *> (Drct <$> drctParser <|> Stmt <$> seqExprParser)

endParser :: ParsecT String u Identity ()
endParser =
  lookAhead
    $  whiteSpace
    <* (   skipMany1 semi
       <|> skipMany1 endOfLine
       <|> skipMany1 (char ')')
       <|> skipMany1 (char '}')
       <|> skipMany1 (char ']')
       <|> skipMany1 (char ',')
       <|> skipMany1 (reserved "elif")
       <|> skipMany1 (reserved "else")
       <|> skipMany1 (reserved "do")
       <|> eof
       )

last :: Parser a -> Parser a
last b = b <* endParser

drctParser :: Parser Drct
drctParser =
  try (last $ reserved ":clear" *> return Clear)
    <|> try (last $ reserved ":exit" *> return Exit)
    <|> try (last $ reserved ":help" *> return Help)
    <|> Rm
    <$> try (last $ reserved ":rm" *> identifier)
    <|> Load
    <$> try (last $ reserved ":load" *> stringLiteral)

exprParser :: Parser Expr
exprParser =
  try (last boolExprParser)
    <|> try (last relExprParser)
    <|> try (last algExprParser)
    <|> try (last funExprParser)
    <|> try (last strExprParser)
    <|> try (last listExprParser)
    <|> try (last assignExprParser)
    <|> try (last ifExprParser)
    <|> try (last whileExprParser)
    <|> try readExprParser

seqExprParser :: Parser Expr
seqExprParser = Seq <$> (sepEndBy1 singleExprParser (semi <|> many1 endOfLine))

singleExprParser :: Parser Expr
singleExprParser = braces seqExprParser <|> try printExprParser <|> exprParser

ifExprParser :: Parser Expr
ifExprParser =
  If
    <$> (reserved "if" *> exprParser)
    <*> (reserved "do" *> singleExprParser)
    <*> option (Val Null) (reserved "else" *> singleExprParser)
    <?> "if"

whileExprParser :: Parser Expr
whileExprParser =
  While
    <$> (reserved "while" *> exprParser)
    <*> (reserved "do" *> singleExprParser)
    <?> "while"

printExprParser :: Parser Expr
printExprParser =
  Print <$> (reserved "print" *> parens (commaSep exprParser)) <?> "print"

readExprParser :: Parser Expr
readExprParser = reserved "read" *> string "()" *> return Read

litValParser :: Parser Val
litValParser = algValParser <|> StrVal <$> many1 anyChar

valParser :: Parser Val
valParser =
  listValParser
    <|> algValParser
    <|> boolValParser
    <|> strValParser
    <|> charValParser

algValParser :: Parser Val
algValParser =
  AlgVal
    <$> (try double <|> fromInteger <$> integer)
    <|> (reserved "null" *> return Null)

boolValParser :: Parser Val
boolValParser =
  BoolVal
    <$> (   try (reserved "true" *> return True)
        <|> try (reserved "false" *> return False)
        )
    <|> (reserved "null" *> return Null)

funValParser :: Parser Val
funValParser =
  FunVal
    <$> parens (commaSep identifier)
    <*> (reservedOp "=>" *> singleExprParser)
    <?> "function definition"

listValParser :: Parser Val
listValParser =
  ListVal
    .   fromList
    <$> (brackets . commaSep) valParser
    <|> (reserved "null" *> return Null)

listLitParser :: Parser Expr
listLitParser =
  ListLiteral
    <$> (brackets . commaSep) exprParser
    <|> (reserved "null" *> return (Val Null))

charValParser :: Parser Val
charValParser = CharVal <$> charLiteral

strValParser :: Parser Val
strValParser = StrVal <$> stringLiteral

algExprParser :: Parser Expr
algExprParser = buildExpressionParser algOperators algTerm

boolExprParser :: Parser Expr
boolExprParser =
  buildExpressionParser boolOperators (boolTerm <|> relExprParser)

listExprParser :: Parser Expr
listExprParser = buildExpressionParser listOperators listTerm

strExprParser :: Parser Expr
strExprParser = buildExpressionParser strOperators strTerm

funExprParser :: Parser Expr
funExprParser = try funAppParser <|> Val <$> funValParser

funAppParser :: Parser Expr
funAppParser =
  FunApp
    <$> identifier
    <*> parens (commaSep exprParser)
    <?> "function application"

assignExprParser :: Parser Expr
assignExprParser =
  Assign
    <$> identifier
    <*> option (Val Null) (parens exprParser)
    <*> (reservedOp "=" *> exprParser)
    <?> "assignment"

relExprParser :: Parser Expr
relExprParser =
  try
      (do
        e1   <- relTerm
        rel  <- relation
        e2   <- try $ lookAhead relTerm
        next <- option (Val Null) (try $ last relExprParser)
        let cur = Binary rel e1 e2
        return $ case next of
          Val _ -> cur
          other -> Binary (&&) cur other
      )
    <|> relTerm

listTerm :: ParsecT String () Identity Expr
listTerm =
  parens exprParser
    <|> try funAppParser
    <|> listLitParser
    <|> Val
    <$> valParser
    <|> Var
    <$> identifier

strTerm :: ParsecT String () Identity Expr
strTerm =
  parens exprParser
    <|> try funAppParser
    <|> listLitParser
    <|> Val
    <$> valParser
    <|> Var
    <$> identifier

algTerm :: ParsecT String () Identity Expr
algTerm =
  parens algExprParser
    <|> try funAppParser
    <|> Val
    <$> algValParser
    <|> Var
    <$> identifier

boolTerm :: ParsecT String () Identity Expr
boolTerm =
  parens exprParser
    <|> try funAppParser
    <|> Val
    <$> boolValParser
    <|> Var
    <$> identifier

relTerm :: ParsecT String () Identity Expr
relTerm = try boolTerm <|> try algTerm <|> try strTerm <|> listTerm

relation :: ParsecT String u Identity (Val -> Val -> Bool)
relation =
  (reservedOp ">" *> return (>))
    <|> (reservedOp "<" *> return (<))
    <|> (reservedOp "==" *> return (==))
    <|> (reservedOp "!=" *> return (/=))
