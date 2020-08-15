{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( print
  , isVar
  , readVar
  , readFun
  , writeVar
  , writeFun
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
import           System.Console.Haskeline
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
  case Map.lookup x (getLocals store) of
    Just v  -> return v
    Nothing -> case Map.lookup x (getGlobals store) of
      Just v  -> return v
      Nothing -> return Empty

writeVar :: Var -> Val -> Interp ()
writeVar x v = do
  store <- getStore
  putStore $ if Map.member x (getLocals store)
    then setLocals (Map.insert x v) store
    else setGlobals (Map.insert x v) store

readFun :: Var -> Interp Fun
readFun x = do
  store <- getStore
  case Map.lookup x (getFuns store) of
    Just v  -> return v
    Nothing -> return None

writeFun :: Var -> Fun -> Interp ()
writeFun f s = do
  store <- getStore
  putStore $ setFuns (Map.insert f s) store

print :: Show a => a -> Interp ()
print = Interp . lift . outputStrLn . fromString . show
