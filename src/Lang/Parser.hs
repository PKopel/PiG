{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Parser where

import           Control.Monad
import           Import                  hiding ( optional
                                                , try
                                                , (<|>)
                                                )
import           Lang.Lexer
import           Text.Parsec
import           Text.ParserCombinators.Parsec
                                         hiding ( try )
import           Text.ParserCombinators.Parsec.Expr

parseProg :: String -> Either String Prog
parseProg "exit" = Left "exit"
parseProg p      = case parse progParser "PiG" p of
  Left  err  -> Left $ show err
  Right prog -> Right prog

progParser :: Parser Prog
progParser = whiteSpace >> stmtParser

stmtParser :: Parser Stmt
stmtParser = braces stmtParser <|> sequenceOfStmt
 where
  sequenceOfStmt = do
    list <- (sepEndBy1 singleStmtParser semi)
    -- If there's only one statement return it without using Seq.
    return $ case list of
      [stmt] -> stmt
      _      -> Seq list

singleStmtParser :: Parser Stmt
singleStmtParser =
  try (braces stmtParser)
    <|> try printStmtParser
    <|> try ifStmtParser
    <|> try whileStmtParser
    <|> try skipStmtParser
    <|> assignStmtParser

ifStmtParser :: Parser Stmt
ifStmtParser = do
  cond  <- reserved "if" >> exprParser
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
  var  <- identifier
  expr <- reservedOp "=" >> exprParser
  return $ var := expr

printStmtParser :: Parser Stmt
printStmtParser = do
  expr <- reserved "print" >> exprParser
  return $ Print expr

skipStmtParser :: Parser Stmt
skipStmtParser = reserved "skip" >> return Skip

algValParser :: Parser Val
algValParser = AlgVal <$> (try double <|> fromInteger <$> integer)

boolValParser :: Parser Val
boolValParser =
  BoolVal
    <$> (   try (reserved "true" >> return True)
        <|> try (reserved "false" >> return False)
        )

listValParser :: Parser Val
listValParser = ListVal
  <$> (brackets . commaSep) (listValParser <|> boolValParser <|> algValParser)

exprParser :: Parser Expr
exprParser = try boolExprParser <|> try listExprParser <|> algExprParser

algExprParser :: Parser Expr
algExprParser = buildExpressionParser algOperators algTerm

boolExprParser :: Parser Expr
boolExprParser = buildExpressionParser boolOperators boolTerm

algTerm :: ParsecT String () Identity Expr
algTerm = parens algExprParser <|> Var <$> identifier <|> Val <$> algValParser

boolTerm :: ParsecT String () Identity Expr
boolTerm =
  parens boolExprParser
    <|> Var
    <$> identifier
    <|> Val
    <$> boolValParser
    <|> relExprParser

relExprParser :: ParsecT String () Identity Expr
relExprParser = do
  a1 <- algExprParser
  op <- relation
  a2 <- algExprParser
  return $ RelBinary op a1 a2

relation :: ParsecT String u Identity RelBinOp
relation =
  (reservedOp ">" >> return Greater)
    <|> (reservedOp "<" >> return Less)
    <|> (reservedOp "==" >> return Equal)

listExprParser :: Parser Expr
listExprParser =
  parens listExprParser
    <|> try
          (do
            a1 <- Val <$> listValParser <|> Var <$> identifier
            op <- listBinAction
            a2 <- exprParser
            return $ ListBinary op a1 a2
          )
    <|> try
          (do
            op <- listUnAction
            a  <- exprParser
            return $ ListUnary op a
          )
    <|> Val
    <$> listValParser

listBinAction :: ParsecT String u Identity ListBinOp
listBinAction =
  (reservedOp "+>" >> return AddFirst) <|> (reservedOp "+<" >> return AddLast)

listUnAction :: ParsecT String u Identity ListUnOp
listUnAction =
  (reservedOp "->" >> return RmFirst) <|> (reservedOp "-<" >> return RmLast)
