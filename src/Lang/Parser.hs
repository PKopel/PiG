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
progParser = whiteSpace >> (Stmt <$> stmtParser <|> Drct <$> drctParser)

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

stmtParser :: Parser Stmt
stmtParser = do
  list <- (sepEndBy1 singleStmtParser (semi <|> many1 endOfLine))
  return $ case list of
    [stmt] -> stmt
    _      -> Seq list

singleStmtParser :: Parser Stmt
singleStmtParser =
  braces stmtParser
    <|> try ifStmtParser
    <|> try whileStmtParser
    <|> try skipStmtParser
    <|> try assignStmtParser
    <|> printStmtParser

ifStmtParser :: Parser Stmt
ifStmtParser =
  (do
      cond  <- reserved "if" >> exprParser
      stmt1 <- reserved "then" >> singleStmtParser
      stmt2 <- option Skip (reserved "else" >> singleStmtParser)
      return $ If cond stmt1 stmt2
    )
    <?> "if"

whileStmtParser :: Parser Stmt
whileStmtParser =
  (do
      cond <- reserved "while" >> exprParser
      stmt <- reserved "do" >> singleStmtParser
      return $ While cond stmt
    )
    <?> "while"

assignStmtParser :: Parser Stmt
assignStmtParser =
  (do
      var  <- identifier
      expr <- reservedOp "=" >> exprParser
      return $ Assign var expr
    )
    <?> "assignment"

printStmtParser :: Parser Stmt
printStmtParser = Print <$> exprParser <?> "print"

skipStmtParser :: Parser Stmt
skipStmtParser = (reserved "skip" >> return Skip) <?> "skip"

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
      try
          (braces $ do
            body <- stmtParser
            ret  <- reserved "return" >> exprParser <* semi
            return $ FunVal args body ret
          )
        <|> (do
              ret <- exprParser
              return $ FunVal args Skip ret
            )
    )
    <?> "function definition"

listValParser :: Parser Expr
listValParser =
  ListLiteral
    <$> (brackets . commaSep) exprParser
    <|> (reserved "null" >> return (Val Null))

exprParser :: Parser Expr
exprParser =
  try (last boolExprParser)
    <|> try (last algExprParser)
    <|> try (last funExprParser)
    <|> last listExprParser

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
  return $ RelBinary op a1 a2

relation :: ParsecT String u Identity RelBinOp
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
