{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Statements where

import           Control.Monad
import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Import

eval :: Expr -> Interp Val
eval (Val n          ) = return n
eval (Var x          ) = readVar x
eval (Binary op e1 e2) = (appBin op) <$> eval e1 <*> eval e2
eval (Unary op e     ) = (appUn op) <$> eval e >>= \case
  ListVal (h :<| t) ->
    let (iv, x) = isVar e
    in  when iv (writeVar x (ListVal t) >> return ()) >> return h
  other -> return other
eval (ListLiteral es) = ListVal . Seq.fromList <$> mapM eval es
eval (FunApp n vs   ) = readVar n >>= evalFunApp vs
eval (Print e) =
  mapM eval e >>= mapM printVal >> printString "\n" >> return Null
eval (Seq []      ) = return Null
eval (Seq [s     ]) = eval s
eval (Seq (s : ss)) = eval s >> eval (Seq ss)
eval (If c e1 e2  ) = eval c >>= \case
  AlgVal  v -> if v /= 0 then eval e1 else eval e2
  BoolVal v -> if v then eval e1 else eval e2
  ListVal v -> if not (Seq.null v) then eval e1 else eval e2
  _         -> return Null
eval (While e s   ) = ListVal <$> evalWhile e s Empty
eval (Assign x i e) = do
  v <- eval e
  readVar x >>= \case
    ListVal l -> eval i >>= \case
      AlgVal ind -> writeVar x . ListVal $ Seq.update (round ind) v l
      _          -> writeVar x v
    _ -> writeVar x v

evalFunApp :: [Expr] -> Val -> Interp Val
-- execute function of name n with arguments from vs
evalFunApp vs (FunVal as body) = do
  newLocals <-
    Map.fromList <$> zipWithM (\a v -> eval v >>= return . (,) a) as vs
  oldLocals <- getStore >>= return . view (scope localL)
  oldScope  <- getScope
  withStore $ (over . scope) localL (const newLocals) -- introducing local variables
  setScope localL -- from now variables are declared in local scope
  retVal <- eval body
  setScope oldScope -- end of local scope
  withStore $ (over . scope) localL (const oldLocals) -- removing local variables
  return retVal
-- get elements from list of name n
evalFunApp vs (ListVal l) = do
  evs <- foldM
    (\acc v -> eval v >>= \case
      AlgVal av -> return (av : acc)
      _         -> return acc
    )
    []
    vs
  case getElems l evs of
    v  :<|    Empty -> return v -- one argument in vs, result is a single value
    v@(_ :<| _)     -> return $ ListVal v -- more than one argument in vs, result is a list
    _               -> return Null
evalFunApp _ _ = return Null

evalWhile :: Expr -> Expr -> Seq Val -> Interp (Seq Val)
evalWhile e s acc = eval e >>= \case
  AlgVal  v -> while $ v /= 0
  BoolVal v -> while v
  ListVal v -> while $ not (Seq.null v)
  _         -> return acc
 where
  while cond =
    if cond then eval s >>= evalWhile e s . (acc Seq.|>) else return acc
