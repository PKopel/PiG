{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Either
import Import
import Util
import qualified RIO.Text as T

endParser :: Parser ()
endParser = skipSpace *> (endOfLine <|> skip (== ';'))

expParser :: Parser Exp
expParser =
  parseRPN . infixToRPN <$> takeTill (== ';')
    <?> "espresion"

varParser :: Parser Var
varParser = takeTill (notInClass "a-z") <* skipSpace <?> "variable"

stmtParser :: Parser Stmt
stmtParser =
  Print <$> (skipSpace *> string "print" *> skipSpace *> varParser <* endParser <?> "print")
    <|> While <$> (skipSpace *> string "while" *> expParser <* endParser <?> "while")
    <*> (skipSpace *> string "do" *> skipSpace *> stmtParser <* endParser <?> "do")
    <|> Seq <$> (skipSpace *> char '{' *> skipSpace *> many stmtParser <* char '}' <?> "sequence")
    <|> (:=) <$> (skipSpace *> varParser)
    <*> (char '=' *> skipSpace *> expParser <* endParser)
    <?> "statement"

runParser :: Text -> Either String Stmt
runParser "exit" = Left "exit"
runParser text = parseOnly stmtParser (text <> ";")

infixToRPN :: Text -> Text
infixToRPN = T.unwords . shunt ([], []) . T.words

shunt :: ([Text], [Operator]) -> [Text] -> [Text]
shunt (out, ops) [] = reverse out ++ ops
shunt (out, ops) (x : rest)
  | isOperator x = shunt (shuntOp (out, ops) x) rest
  | otherwise = shunt (x : out, ops) rest

shuntOp :: ([Text], [Operator]) -> Operator -> ([Text], [Operator])
shuntOp (out, "(" : ops) ")" = (out, ops)
shuntOp (out, op : ops) ")" = shuntOp (op : out, ops) ")"
shuntOp (out, op : ops) x
  | op ->- x || (op -=- x && isLeftAssociative x) =
    shuntOp (op : out, ops) x
  | otherwise =
    (out, x : op : ops)
shuntOp (out, ops) op = (out, op : ops)

parseRPN :: Text -> Exp
parseRPN text = case (foldM parseFun [] . T.words) text of
  Right [fun] -> fun

parseFun :: [Exp] -> Text -> Either String [Exp]
parseFun fs x
  | isOperator x && length fs > 1 = Right $ parseOp fs x
  | inClass ['a' .. 'z'] $ T.index x 0 = parseOnly (V <$> varParser) x >>= return . (: fs)
  | otherwise = case parseOnly double x of
    Right n -> Right $ C n : fs
    Left msg -> Left msg

parseOp :: [Exp] -> Text -> [Exp]
parseOp (fx : gx : hs) "*" = (fx :*: gx) : hs
parseOp (fx : gx : hs) "+" = (fx :+: gx) : hs
parseOp (fx : gx : hs) "-" = (gx :-: fx) : hs
parseOp (fx : gx : hs) "/" = (gx :/: fx) : hs
parseOp (fx : gx : hs) "^" = (gx :^: fx) : hs