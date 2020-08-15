{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( printVal
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
import           System.Console.Haskeline
import           Utils.Types

valToList :: Val -> [Val]
valToList (ListVal vs) = vs
valToList v            = [v]

maybeToAlgVal :: Maybe Double -> Val
maybeToAlgVal Nothing  = Null
maybeToAlgVal (Just v) = AlgVal v

maybeToBoolVal :: Maybe Bool -> Val
maybeToBoolVal Nothing  = Null
maybeToBoolVal (Just v) = BoolVal v

maybeToListVal :: Maybe [Val] -> Val
maybeToListVal Nothing  = Null
maybeToListVal (Just v) = ListVal v

algValToMaybe :: Val -> Maybe Double
algValToMaybe (AlgVal n) = Just n
algValToMaybe _          = Nothing

boolValToMaybe :: Val -> Maybe Bool
boolValToMaybe (BoolVal b) = Just b
boolValToMaybe (AlgVal  a) = Just (a /= 0)
boolValToMaybe (ListVal l) = Just (length l > 0)
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
      Nothing -> return Null

writeVar :: Var -> Val -> Interp ()
writeVar x v = do
  store <- getStore
  putStore $ if Map.member x (getLocals store)
    then setLocals (Map.insert x v) store
    else setGlobals (Map.insert x v) store

printVal :: Show a => a -> Interp ()
printVal = Interp . lift . outputStrLn . fromString . show
