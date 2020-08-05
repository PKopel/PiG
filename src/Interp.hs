{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp
  ( runProg,
  )
where

import Control.Monad
import Data.Map as Map
import Import
import RIO.Text

eval :: Exp -> Interp Val
eval (C n) = return n
eval (V x) = rd x
eval (e1 :+: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (e1 :-: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 - v2)
eval (e1 :*: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)
eval (e1 :^: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 ** v2)
eval (e1 :/: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  if v2 == 0 then fail "division by zero" else return (v1 / v2)

rd :: Var -> Interp Val
rd x = Interp $ \r -> case Map.lookup x r of
  Nothing -> Left ("unbound variable " <> x)
  Just v -> Right (v, r)

wr :: Var -> Val -> Interp ()
wr x v = Interp $ \r -> Right ((), Map.insert x v r)

pt :: Var -> Interp ()
pt x = Interp $ \r -> case Map.lookup x r of
  Nothing -> Left ("unbound variable " <> x)
  Just v -> Left $ x <> ":=" <> (pack . show) v

exec :: Stmt -> Interp ()
exec (x := e) = eval e >>= wr x
exec (Seq []) = return ()
exec (Seq (s : ss)) = exec s >> exec (Seq ss)
exec (Print e) = pt e
exec (While e s) = do
  v <- eval e
  when (v /= 0) (exec (Seq [s, While e s]))

runProg :: Store -> Prog -> Either String Store
runProg r p = case runInterp (exec p) r of
  Left msg -> Left $ unpack msg
  Right (_, r') -> Right r'
