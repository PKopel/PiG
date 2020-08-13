{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Exec where

import Control.Monad
import Import
import Interp.Eval

exec :: Stmt -> Interp ()
exec Skip = return ()
exec (x := e) = eval e >>= writeVar x
exec (Seq []) = return ()
exec (Seq (s : ss)) = exec s >> exec (Seq ss)
exec (Print e) = eval e >>= print
exec (If e s1 s2) =
  eval e >>= \case
    AlgVal v -> if v /= 0 then exec s1 else exec s2
    BoolVal v -> if v then exec s1 else exec s2
    ListVal v -> if length v > 0 then exec s1 else exec s2
    Empty -> return ()
exec (While e s) =
  eval e >>= \case
    AlgVal v -> when (v /= 0) (exec (Seq [s, While e s]))
    BoolVal v -> when v (exec (Seq [s, While e s]))
    ListVal v -> when (length v > 0) (exec (Seq [s, While e s]))
    Empty -> return ()

runProg :: Store -> Prog -> RIO App ((), Store)
runProg r p = runWithStore (exec p) r
