{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Statements where

import           Control.Monad
import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Import

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
eval (ListLiteral es     ) = ListVal . Seq.fromList <$> mapM eval es
eval (FunApp n vs        ) = readVar n >>= evalFunApp vs
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
eval (While e s         ) = ListVal <$> evalWhile e s Empty
eval (Assign x Nothing e) = do
  v        <- eval e
  writeVar <- getWriteFun
  writeVar x v
  return v
eval (Assign x (Just i) e) = do
  v        <- eval e
  writeVar <- getWriteFun
  readVar x >>= \case
    ListVal l -> evalAlg i >>= \case
      Nothing  -> writeVar x v
      Just ind -> writeVar x . ListVal $ Seq.update (round ind) v l
    _ -> writeVar x v
  return v

evalAlg :: Expr -> Interp (Maybe Double)
evalAlg e = eval e >>= return . algValToMaybe

evalBool :: Expr -> Interp (Maybe Bool)
evalBool e = eval e >>= return . boolValToMaybe

evalList :: Expr -> Interp (Maybe (Seq Val))
evalList e = eval e >>= return . listValToMaybe

evalAlgBin :: AlgBinOp -> Expr -> Expr -> Interp Val
evalAlgBin op e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ op <$> v1 <*> v2

evalBoolBin :: BoolBinOp -> Expr -> Expr -> Interp Val
evalBoolBin op e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return . maybeToBoolVal $ op <$> v1 <*> v2

evalRelBin :: RelBinOp -> Expr -> Expr -> Interp Val
evalRelBin op e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ op <$> v1 <*> v2

evalListBin :: ListBinOp -> Expr -> Expr -> Interp Val
evalListBin op e1 e2 = do
  l <- eval e1
  v <- eval e2
  return . maybeToListVal $ op <$> toMaybeList l <*> toMaybeList v
  where toMaybeList = Just . valToList

evalListUn :: ListUnOp -> Expr -> Interp Val
evalListUn op e = do
  writeVar <- getWriteFun
  let (iv, x) = isVar e
  evalList e >>= return . fmap op >>= \case
    Just (h, t) -> when iv (writeVar x (ListVal t)) >> return h
    _           -> return Null

evalFunApp :: [Expr] -> Val -> Interp Val
evalFunApp vs (FunVal as body) = do
  -- execute function of name n with arguments from vs
  newLocals <-
    Map.fromList <$> zipWithM (\a v -> eval v >>= return . (,) a) as vs
  oldLocals   <- getStore >>= return . getLocals
  oldWriteVar <- getWriteFun
  withStore $ setLocals (const newLocals) -- introducing local variables
  putWriteFun writeLocVar -- from now variables are declared in local scope
  retVal <- eval body
  putWriteFun oldWriteVar -- end of local scope
  withStore $ setLocals (const oldLocals) -- removing local variables
  return retVal
evalFunApp vs (ListVal l) = do
  -- get elements from list of name n
  evs <- foldM (\acc v -> evalAlg v >>= return . (: acc)) [] vs
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
