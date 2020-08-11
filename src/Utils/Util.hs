{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( print
  , isVar
  , readVar
  , writeVar
  , valToList
  , maybeToAlgVal
  , maybeToBoolVal
  , maybeToListVal
  , algValToMaybe
  , boolValToMaybe
  , listValToMaybe
  , module Utils.Types
  )
where

import           Data.Map                      as Map
import           RIO
import           Utils.Types

valToList :: Val -> [Val]
valToList (ListVal vs) = vs
valToList v            = [v]

maybeToAlgVal :: Maybe Double -> Val
maybeToAlgVal Nothing  = Empty
maybeToAlgVal (Just v) = AlgVal v

maybeToBoolVal :: Maybe Bool -> Val
maybeToBoolVal Nothing  = Empty
maybeToBoolVal (Just v) = BoolVal v

maybeToListVal :: Maybe [Val] -> Val
maybeToListVal Nothing  = Empty
maybeToListVal (Just v) = ListVal v

algValToMaybe :: Val -> Maybe Double
algValToMaybe (AlgVal n) = Just n
algValToMaybe _          = Nothing

boolValToMaybe :: Val -> Maybe Bool
boolValToMaybe (BoolVal b) = Just b
boolValToMaybe _           = Nothing

listValToMaybe :: Val -> Maybe [Val]
listValToMaybe (ListVal n) = Just n
listValToMaybe _           = Nothing

isVar :: Expr -> (Bool, Var)
isVar (Var x) = (True, x)
isVar _       = (False, "")

readVar :: Var -> Interp Val
readVar x = do
  store <- getStore
  case Map.lookup x store of
    Nothing -> return Empty
    Just v  -> return v

writeVar :: Var -> Val -> Interp ()
writeVar x v = do
  store <- getStore
  putStore $ Map.insert x v store

print :: Show a => a -> Interp ()
print = lift . logInfo . fromString . show
