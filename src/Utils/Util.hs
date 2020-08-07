{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Utils.Util
  ( print
  , readVar
  , writeVar
  , printExpr
  , module Utils.Types
  )
where

import           Data.Map                      as Map
import           RIO
import           Utils.Types

readVar :: Var -> Interp Val
readVar x = do
  store <- getStore
  case Map.lookup x store of
    Nothing -> print ("unbound variable " <> x) >> return Empty
    Just v  -> return v

writeVar :: Var -> Val -> Interp ()
writeVar x v = do
  store <- getStore
  putStore $ Map.insert x v store

printExpr :: Val -> Interp ()
printExpr = \case
  AlgVal  v -> print v
  BoolVal v -> print v
  Empty     -> print ("can't print that" :: Text)

print :: Show a => a -> Interp ()
print = lift . logInfo . displayShow
