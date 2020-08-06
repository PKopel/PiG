{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp
  ( runProg
  )
where

import           Control.Monad
import           Data.Map                      as Map
import           Import

eval :: Exp -> Interp Val
eval (C n      ) = return n
eval (V x      ) = rd x
eval (e1 :+: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return ((+) <$> v1 <*> v2)
eval (e1 :-: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return ((-) <$> v1 <*> v2)
eval (e1 :*: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return ((*) <$> v1 <*> v2)
eval (e1 :^: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return ((**) <$> v1 <*> v2)
eval (e1 :/: e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ if v2 == Just 0 then Nothing else ((/) <$> v1 <*> v2)

rd :: Var -> Interp Val
rd x = do
  store <- getStore
  case Map.lookup x store of
    Nothing -> print ("unbound variable " <> x) >> return Nothing
    Just v  -> return v

wr :: Var -> Val -> Interp ()
wr x v = do
  store <- getStore
  putStore $ Map.insert x v store

pt :: Val -> Interp ()
pt x = case x of
  Nothing -> print ("can't print that" :: Text)
  Just v  -> print v

exec :: Stmt -> Interp ()
exec (x := e        ) = eval e >>= wr x
exec (Seq   []      ) = return ()
exec (Seq   (s : ss)) = exec s >> exec (Seq ss)
exec (Print e       ) = eval e >>= pt
exec (If e s        ) = eval e >>= \case
  Just v  -> when (v /= 0) (exec s)
  Nothing -> return ()
exec (While e s) = eval e >>= \case
  Just v  -> when (v /= 0) (exec (Seq [s, While e s]))
  Nothing -> return ()

runProg :: Store -> Prog -> RIO App ((), Store)
runProg r p = runInterp (exec p) r
