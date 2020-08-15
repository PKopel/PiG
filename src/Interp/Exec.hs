{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Exec where

import           Control.Monad
import qualified Data.Map                      as Map
import           Import
import           Interp.Eval
import           System.Console.Haskeline

exec :: Stmt -> Interp ()
exec Skip             = return ()
exec (Ign e         ) = eval e >> return ()
exec (x := e        ) = eval e >>= writeVar x
exec (Seq   []      ) = return ()
exec (Seq   (s : ss)) = exec s >> exec (Seq ss)
exec (Print e       ) = eval e >>= print
exec (FunDef n f    ) = writeFun n f
exec (FunApp n vs   ) = do
  fun <- readFun n
  case fun of
    None        -> return ()
    Fun as stmt -> do
      newLocals <-
        Map.fromList <$> zipWithM (\a v -> eval v >>= return . (,) a) as vs
      oldLocals <- getStore >>= return . getLocals
      withStore $ setLocals (Map.union newLocals)
      exec stmt
      withStore $ setLocals
        ((flip Map.difference) (Map.difference newLocals oldLocals))
exec (If e s1 s2) = eval e >>= \case
  AlgVal  v -> if v /= 0 then exec s1 else exec s2
  BoolVal v -> if v then exec s1 else exec s2
  ListVal v -> if length v > 0 then exec s1 else exec s2
  Empty     -> return ()
exec (While e s) = eval e >>= \case
  AlgVal  v -> when (v /= 0) (exec (Seq [s, While e s]))
  BoolVal v -> when v (exec (Seq [s, While e s]))
  ListVal v -> when (length v > 0) (exec (Seq [s, While e s]))
  Empty     -> return ()

runProg :: Store -> Prog -> InputT IO ((), Store)
runProg r p = runWithStore (exec p) r
