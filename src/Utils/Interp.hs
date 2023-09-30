{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Interp
  ( getStore
  , getScope
  , putStore
  , setScope
  , withScopes
  , withStore
  , getVar
  , putVar
  , runWithStore
  , interpWithStore
  ) where

import           Control.Monad.State            ( StateT(runStateT)
                                                , get
                                                , put
                                                )
import qualified Data.Map                      as Map
import           RIO
import           Utils.Types

getStore :: Interp a Store
getStore = get <&> snd

getScope :: Interp a Scope
getScope = get <&> fst

putStore :: Store -> Interp a ()
putStore s' = do
  (w, _) <- get
  put (w, s')

setScope :: Scope -> Interp a ()
setScope w' = do
  (_, s) <- get
  put (w', s)

withScopes :: (Scopes -> Scopes) -> Interp a ()
withScopes f = getStore >>= \case
  Right store -> putStore . Right . f $ store
  _           -> return ()

withStore :: (Store -> Interp a Store) -> Interp a ()
withStore fun = getStore >>= fun >>= putStore

interpWithStore :: Interp a v -> Interp a ()
interpWithStore interp = withStore $ Interp . lift . runWithStore interp

runWithStore :: Interp a v -> Store -> RIO a Store
runWithStore interp store@(Right _) =
  (runStateT . runInterp) interp (globalL, store) <&> snd . snd
runWithStore _ left = return left

getVar :: Var -> Interp a Val
getVar x = getStore >>= \case
  Right store -> case Map.lookup x (view (scope localL) store) of
    Just v  -> return v
    Nothing -> case Map.lookup x (view (scope globalL) store) of
      Just v  -> return v
      Nothing -> return Null
  _ -> return Null

putVar :: Var -> Val -> Interp a Val
putVar x v = do
  s <- getScope
  withScopes $ (over $ scope s) (Map.insert x v)
  return v
