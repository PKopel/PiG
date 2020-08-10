{-# LANGUAGE LambdaCase #-}

module Interp.Eval where

import Import

eval :: Expr -> Interp Val
eval (Val n) = return n
eval (Var x) = readVar x
eval (Neg e) =
  eval e >>= \case
    AlgVal v -> return . AlgVal . negate $ v
    BoolVal v -> return . BoolVal . not $ v
    _ -> return Empty
eval (AlgBinary op e1 e2) = evalAlgBin op e1 e2
eval (BoolBinary op e1 e2) = evalBoolBin op e1 e2
eval (RelBinary op e1 e2) = evalRelBin op e1 e2

evalAlg :: Expr -> Interp (Maybe Double)
evalAlg e = eval e >>= return . algValToMaybe

evalBool :: Expr -> Interp (Maybe Bool)
evalBool e = eval e >>= return . boolValToMaybe

evalAlgBin :: AlgBinOp -> Expr -> Expr -> Interp Val
evalAlgBin Add e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ ((+) <$> v1 <*> v2)
evalAlgBin Subtract e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ ((-) <$> v1 <*> v2)
evalAlgBin Multiply e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ ((*) <$> v1 <*> v2)
evalAlgBin Power e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ ((**) <$> v1 <*> v2)
evalAlgBin Divide e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToAlgVal $ if v2 == Just 0 then Nothing else ((/) <$> v1 <*> v2)

evalBoolBin :: BoolBinOp -> Expr -> Expr -> Interp Val
evalBoolBin And e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return . maybeToBoolVal $ ((&&) <$> v1 <*> v2)
evalBoolBin Or e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return . maybeToBoolVal $ ((||) <$> v1 <*> v2)

evalRelBin :: RelBinOp -> Expr -> Expr -> Interp Val
evalRelBin Greater e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ ((>) <$> v1 <*> v2)
evalRelBin Less e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ ((<) <$> v1 <*> v2)
evalRelBin Equal e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return . maybeToBoolVal $ ((==) <$> v1 <*> v2)
