{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Parser where

import           Control.Monad
import           Import                  hiding ( many
                                                , optional
                                                , try
                                                , (<|>)
                                                )
import           Lang.Lexer
import           Text.Parsec
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Expr

parseProg :: String -> Either String Prog
parseProg p = case parse progParser "PiG" p of
  Left  err  -> Left $ show err
  Right prog -> Right prog

parseFile :: FilePath -> IO (Either String [Prog])
parseFile f = parseFromFile (sepEndBy1 progParser semi <* eof) f >>= \case
  Left  err  -> return . Left $ show err
  Right prog -> return $ Right prog

progParser :: Parser Prog
progParser = whiteSpace >> (Stmt <$> seqExprParser <|> Drct <$> drctParser)

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
       <|> skipMany1 (reserved "then")
       <|> skipMany1 (reserved "else")
       <|> skipMany1 (reserved "do")
       <|> eof
       )

last :: Parser a -> Parser a
last b = b <* endParser

drctParser :: Parser Drct
drctParser =
  try (last $ reserved ":clear" >> return Clear)
    <|> try (last $ reserved ":exit" >> return Exit)
    <|> try (last $ reserved ":help" >> return Help)
    <|> Rm
    <$> try (last $ reserved ":rm" >> identifier)
    <|> Load
    <$> try (last $ reserved ":load" >> stringLiteral)

exprParser :: Parser Expr
exprParser =
  try (last boolExprParser)
    <|> try (last algExprParser)
    <|> try (last funExprParser)
    <|> try (last assignExprParser)
    <|> try (last ifExprParser)
    <|> try (last whileExprParser)
    <|> last listExprParser

seqExprParser :: Parser Expr
seqExprParser = Seq <$> (sepEndBy1 singleExprParser (semi <|> many1 endOfLine))

singleExprParser :: Parser Expr
singleExprParser = braces seqExprParser <|> try exprParser <|> printExprParser

ifExprParser :: Parser Expr
ifExprParser =
  (do
      cond  <- reserved "if" >> exprParser
      stmt1 <- reserved "then" >> singleExprParser
      stmt2 <- option (Val Null) (reserved "else" >> singleExprParser)
      return $ If cond stmt1 stmt2
    )
    <?> "if"

whileExprParser :: Parser Expr
whileExprParser =
  (do
      cond <- reserved "while" >> exprParser
      stmt <- reserved "do" >> singleExprParser
      return $ While cond stmt
    )
    <?> "while"

printExprParser :: Parser Expr
printExprParser =
  Print <$> (reserved "print" >> parens (commaSep exprParser)) <?> "print"

algValParser :: Parser Val
algValParser =
  AlgVal
    <$> (try double <|> fromInteger <$> integer)
    <|> (reserved "null" >> return Null)

boolValParser :: Parser Val
boolValParser =
  BoolVal
    <$> (   try (reserved "true" >> return True)
        <|> try (reserved "false" >> return False)
        )
    <|> (reserved "null" >> return Null)

funValParser :: Parser Val
funValParser =
  (do
      args <- parens (commaSep identifier)
      reservedOp "=>"
      body <- singleExprParser
      return $ FunVal args body
    )
    <?> "function definition"

listValParser :: Parser Expr
listValParser =
  ListLiteral
    <$> (brackets . commaSep) exprParser
    <|> (reserved "null" >> return (Val Null))

algExprParser :: Parser Expr
algExprParser = buildExpressionParser algOperators algTerm

boolExprParser :: Parser Expr
boolExprParser = buildExpressionParser boolOperators boolTerm

listExprParser :: Parser Expr
listExprParser = buildExpressionParser listOperators listTerm

funExprParser :: Parser Expr
funExprParser = try funAppParser <|> Val <$> funValParser

funAppParser :: Parser Expr
funAppParser =
  (do
      name <- identifier
      args <- parens (commaSep exprParser)
      return $ FunApp name args
    )
    <?> "function application"

assignExprParser :: Parser Expr
assignExprParser =
  (do
      var   <- identifier
      index <- option (Val Null) (parens exprParser)
      expr  <- reservedOp "=" >> exprParser
      return $ Assign var index expr
    )
    <?> "assignment"

algTerm :: ParsecT String () Identity Expr
algTerm =
  parens algExprParser
    <|> try funAppParser
    <|> Var
    <$> identifier
    <|> Val
    <$> algValParser

boolTerm :: ParsecT String () Identity Expr
boolTerm =
  parens boolExprParser
    <|> relExprParser
    <|> Val
    <$> boolValParser
    <|> try funAppParser
    <|> Var
    <$> identifier

relExprParser :: ParsecT String () Identity Expr
relExprParser = do
  a1 <- algExprParser
  op <- relation
  a2 <- algExprParser
  return $ Binary op a1 a2

relation :: ParsecT String u Identity (Double -> Double -> Bool)
relation =
  (reservedOp ">" >> return (>))
    <|> (reservedOp "<" >> return (<))
    <|> (reservedOp "==" >> return (==))

listTerm :: ParsecT String () Identity Expr
listTerm =
  parens listExprParser
    <|> try funAppParser
    <|> Var
    <$> identifier
    <|> listValParser
    <|> (try algExprParser <|> boolExprParser)
