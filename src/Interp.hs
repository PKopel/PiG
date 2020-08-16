{-# LANGUAGE LambdaCase #-}

module Interp where

import           Control.Monad
import qualified Data.Map                      as Map
import           Import
import           System.Console.Haskeline

eval :: Expr -> Interp Val
eval (Val n) = return n
eval (Var x) = readVar x
eval (Neg e) = eval e >>= \case
  AlgVal  v -> return . AlgVal . negate $ v
  BoolVal v -> return . BoolVal . not $ v
  _         -> return Null
eval (AlgBinary  op e1 e2) = evalAlgBin op e1 e2
eval (BoolBinary op e1 e2) = evalBoolBin op e1 e2
eval (RelBinary  op e1 e2) = evalRelBin op e1 e2
eval (ListBinary op e1 e2) = evalListBin op e1 e2
eval (ListUnary op e     ) = evalListUn op e
eval (FunApp    n  vs    ) = readVar n >>= evalFunApp vs

evalAlg :: Expr -> Interp (Maybe Double)
evalAlg e = eval e >>= return . algValToMaybe

evalBool :: Expr -> Interp (Maybe Bool)
evalBool e = eval e >>= return . boolValToMaybe

evalList :: Expr -> Interp (Maybe [Val])
evalList e = eval e >>= return . listValToMaybe

evalAlgBin :: AlgBinOp -> Expr -> Expr -> Interp Val
evalAlgBin Add e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ (+) <$> v1 <*> v2
evalAlgBin Subtract e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ (-) <$> v1 <*> v2
evalAlgBin Multiply e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ (*) <$> v1 <*> v2
evalAlgBin Power e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ (**) <$> v1 <*> v2
evalAlgBin Divide e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ if v2 == Just 0 then Nothing else (/) <$> v1 <*> v2

evalBoolBin :: BoolBinOp -> Expr -> Expr -> Interp Val
evalBoolBin And e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return . maybeToBoolVal $ (&&) <$> v1 <*> v2
evalBoolBin Or e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return . maybeToBoolVal $ (||) <$> v1 <*> v2

evalRelBin :: RelBinOp -> Expr -> Expr -> Interp Val
evalRelBin Greater e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ (>) <$> v1 <*> v2
evalRelBin Less e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ (<) <$> v1 <*> v2
evalRelBin Equal e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ (==) <$> v1 <*> v2

evalListBin :: ListBinOp -> Expr -> Expr -> Interp Val
evalListBin Concat e1 e2 = do
  l <- eval e1
  v <- eval e2
  return . maybeToListVal $ (++) <$> toMaybeList l <*> toMaybeList v
  where toMaybeList = Just . valToList

evalListUn :: ListUnOp -> Expr -> Interp Val
evalListUn RmFirst e = do
  let (iv, x) = isVar e
  evalList e >>= \case
    Just (h : t) -> when iv (writeVar x (ListVal t)) >> return h
    _            -> return Null -- not a list or an empty list
evalListUn RmLast e = do
  let (iv, x) = isVar e
  evalList e >>= \case
    Just l@(_ : _) ->
      when iv (writeVar x (ListVal $ init l)) >> return (last l)
    _ -> return Null -- not a list or an empty list

evalFunApp :: [Expr] -> Val -> Interp Val
evalFunApp vs (FunVal as stmt ret) = do
  -- execute function of name n with arguments from vs
  newLocals <-
    Map.fromList <$> zipWithM (\a v -> eval v >>= return . (,) a) as vs
  oldLocals <- getStore >>= return . getLocals
  withStore $ setLocals (Map.union newLocals) -- introducing local variables
  exec stmt
  retVal <- eval ret -- value returned from function (must be evaluated before removing local variables)
  withStore
    $ setLocals ((flip Map.difference) (Map.difference newLocals oldLocals)) -- removing local variables
  return retVal
evalFunApp vs (ListVal l) = do
  -- get elements from list of name n
  evs <- foldM (\acc v -> evalAlg v >>= return . (: acc)) [] vs
  case getElems l evs of
    [  v    ] -> return v -- one argument in vs, result is a single value
    v@(_ : _) -> return $ ListVal v -- more than one argument in vs, result is a list
    _         -> return Null
evalFunApp _ _ = return Null

exec :: Stmt -> Interp ()
exec Skip             = return ()
exec (x := e        ) = eval e >>= writeVar x
exec (Seq   []      ) = return ()
exec (Seq   (s : ss)) = exec s >> exec (Seq ss)
exec (Print e       ) = eval e >>= printVal
exec (If e s1 s2    ) = eval e >>= \case
  AlgVal  v -> if v /= 0 then exec s1 else exec s2
  BoolVal v -> if v then exec s1 else exec s2
  ListVal v -> if length v > 0 then exec s1 else exec s2
  _         -> return ()
exec (While e s) = eval e >>= \case
  AlgVal  v -> when (v /= 0) (exec (Seq [s, While e s]))
  BoolVal v -> when v (exec (Seq [s, While e s]))
  ListVal v -> when (length v > 0) (exec (Seq [s, While e s]))
  _         -> return ()

runProg :: Store -> Prog -> InputT IO ((), Store)
runProg r p = runWithStore (exec p) r
