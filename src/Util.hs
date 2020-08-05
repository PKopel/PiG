{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( (->-),
    (-=-),
    isLeftAssociative,
    isOperator,
  )
where

import RIO
import Types

(->-) :: Operator -> Operator -> Bool
op1 ->- op2 = comparePrecedence op1 op2 == GT

(-=-) :: Operator -> Operator -> Bool
op1 -=- op2 = comparePrecedence op1 op2 == EQ

comparePrecedence :: Operator -> Operator -> Ordering
comparePrecedence "(" _ = LT
comparePrecedence _ "(" = LT
comparePrecedence "^" _ = GT
comparePrecedence _ "^" = LT
comparePrecedence "*" "/" = EQ
comparePrecedence "/" "*" = EQ
comparePrecedence "*" _ = GT
comparePrecedence _ "*" = LT
comparePrecedence "/" _ = GT
comparePrecedence _ "/" = LT
comparePrecedence "+" "-" = EQ
comparePrecedence "-" "+" = EQ
comparePrecedence "+" _ = GT
comparePrecedence _ "+" = LT
comparePrecedence "-" _ = GT
comparePrecedence _ "-" = LT
comparePrecedence _ _ = EQ

isLeftAssociative :: Operator -> Bool
isLeftAssociative = (`elem` ["+", "-", "*", "/"])

isOperator :: Text -> Bool
isOperator = (`elem` ["+", "-", "*", "/", "^", "(", ")"])