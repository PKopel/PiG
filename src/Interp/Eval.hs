
{-# LANGUAGE LambdaCase #-}
module Interp.Eval where

import           Import

eval :: Expr -> Interp Val
eval (A expr) = algVal <$> evalAlg expr
eval (B expr) = boolVal <$> evalBool expr

evalAlg :: AlgExpr -> Interp (Maybe Double)
evalAlg (AlgConst n) = return n
evalAlg (AlgVar   x) = readVar x >>= \case
  AlgVal v -> return $ Just v
  _        -> return Nothing
evalAlg (Neg e) = do
  v <- evalAlg e
  return (negate <$> v)
evalAlg (AlgBinary op e1 e2) = evalAlgBin op e1 e2

evalAlgBin :: AlgBinOp -> AlgExpr -> AlgExpr -> Interp (Maybe Double)
evalAlgBin Add e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return ((+) <$> v1 <*> v2)
evalAlgBin Subtract e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return ((-) <$> v1 <*> v2)
evalAlgBin Multiply e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return ((*) <$> v1 <*> v2)
evalAlgBin Power e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return ((**) <$> v1 <*> v2)
evalAlgBin Divide e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return $ if v2 == Just 0 then Nothing else ((/) <$> v1 <*> v2)

evalBool :: BoolExpr -> Interp (Maybe Bool)
evalBool (BoolConst n) = return n
evalBool (BoolVar   x) = readVar x >>= \case
  BoolVal v -> return $ Just v
  _         -> return Nothing
evalBool (Not e) = do
  v <- evalBool e
  return (not <$> v)
evalBool (BoolBinary op e1 e2) = evalBoolBin op e1 e2
evalBool (RelBinary  op e1 e2) = evalRelBin op e1 e2

evalBoolBin :: BoolBinOp -> BoolExpr -> BoolExpr -> Interp (Maybe Bool)
evalBoolBin And e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return ((&&) <$> v1 <*> v2)
evalBoolBin Or e1 e2 = do
  v1 <- evalBool e1
  v2 <- evalBool e2
  return ((||) <$> v1 <*> v2)

evalRelBin :: RelBinOp -> AlgExpr -> AlgExpr -> Interp (Maybe Bool)
evalRelBin Greater e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return ((>) <$> v1 <*> v2)
evalRelBin Less e1 e2 = do
  v1 <- evalAlg e1
  v2 <- evalAlg e2
  return ((<) <$> v1 <*> v2)
