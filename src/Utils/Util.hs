{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Utils.Util
  ( print,
    readVar,
    writeVar,
    printExpr,
    maybeToAlgVal,
    maybeToBoolVal,
    algValToMaybe,
    boolValToMaybe,
    module Utils.Types,
  )
where

import Data.Map as Map
import RIO
import Utils.Types

maybeToAlgVal :: Maybe Double -> Val
maybeToAlgVal Nothing = Empty
maybeToAlgVal (Just v) = AlgVal v

maybeToBoolVal :: Maybe Bool -> Val
maybeToBoolVal Nothing = Empty
maybeToBoolVal (Just v) = BoolVal v

algValToMaybe :: Val -> Maybe Double
algValToMaybe (AlgVal n) = Just n
algValToMaybe _ = Nothing

boolValToMaybe :: Val -> Maybe Bool
boolValToMaybe (BoolVal b) = Just b
boolValToMaybe _ = Nothing

readVar :: Var -> Interp Val
readVar x = do
  store <- getStore
  case Map.lookup x store of
    Nothing -> print ("unbound variable " <> x) >> return Empty
    Just v -> return v

writeVar :: Var -> Val -> Interp ()
writeVar x v = do
  store <- getStore
  putStore $ Map.insert x v store

printExpr :: Val -> Interp ()
printExpr = \case
  AlgVal v -> print v
  BoolVal v -> print v
  ListVal v -> print v
  Empty -> print ("can't print that")

print :: Show a => a -> Interp ()
print = lift . logInfo . fromString . show
