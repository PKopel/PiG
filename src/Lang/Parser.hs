{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Parser where

import Control.Monad
import Import hiding
  ( optional,
    try,
    (<|>),
  )
import Lang.Lexer
import Text.Parsec
import Text.ParserCombinators.Parsec hiding
  ( try,
  )
import Text.ParserCombinators.Parsec.Expr

parseProg :: String -> Either String Prog
parseProg "exit" = Left "exit"
parseProg p = case parse progParser "PiG" p of
  Left err -> Left $ show err
  Right prog -> Right prog

progParser :: Parser Prog
progParser = whiteSpace >> stmtParser

stmtParser :: Parser Stmt
stmtParser = parens stmtParser <|> sequenceOfStmt
  where
    sequenceOfStmt = do
      list <- (sepEndBy1 singleStmtParser semi)
      -- If there's only one statement return it without using Seq.
      return $ case list of
        [stmt] -> stmt
        _ -> Seq list

singleStmtParser :: Parser Stmt
singleStmtParser =
  try (parens stmtParser)
    <|> try printStmtParser
    <|> try ifStmtParser
    <|> try whileStmtParser
    <|> try skipStmtParser
    <|> assignStmtParser

ifStmtParser :: Parser Stmt
ifStmtParser = do
  cond <- reserved "if" >> exprParser
  stmt1 <- reserved "then" >> singleStmtParser
  stmt2 <- option Skip (reserved "else" >> singleStmtParser)
  return $ If cond stmt1 stmt2

whileStmtParser :: Parser Stmt
whileStmtParser = do
  cond <- reserved "while" >> exprParser
  stmt <- reserved "do" >> singleStmtParser
  return $ While cond stmt

assignStmtParser :: Parser Stmt
assignStmtParser = do
  var <- identifier
  expr <- reservedOp "=" >> exprParser
  return $ var := expr

printStmtParser :: Parser Stmt
printStmtParser = do
  expr <- reserved "print" >> exprParser
  return $ Print expr

skipStmtParser :: Parser Stmt
skipStmtParser = reserved "skip" >> return Skip

listParser :: Parser
exprParser :: Parser Expr
exprParser = B <$> try boolExprParser <|> A <$> algExprParser

algExprParser :: Parser AlgExpr
algExprParser = buildExpressionParser algOperators algTerm

boolExprParser :: Parser BoolExpr
boolExprParser = buildExpressionParser boolOperators boolTerm

algTerm :: ParsecT String () Identity AlgExpr
algTerm =
  parens algExprParser
    <|> AlgVar
    <$> identifier
    <|> AlgConst
    <$> Just
    <$> try double
    <|> AlgConst
    <$> Just
    <$> fromInteger
    <$> integer

boolTerm :: ParsecT String () Identity BoolExpr
boolTerm =
  parens boolExprParser
    <|> try (reserved "true" >> return (BoolConst (Just True)))
    <|> try (reserved "false" >> return (BoolConst (Just False)))
    <|> relExprParser

relExprParser :: ParsecT String () Identity BoolExpr
relExprParser = do
  a1 <- algExprParser
  op <- relation
  a2 <- algExprParser
  return $ RelBinary op a1 a2

relation :: ParsecT String u Identity RelBinOp
relation =
  (reservedOp ">" >> return Greater) <|> (reservedOp "<" >> return Less)
