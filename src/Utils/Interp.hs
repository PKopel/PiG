{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Interp
  ( readVal
  , printVal
  , printString
  , getStore
  , getScope
  , putStore
  , setScope
  , withScopes
  , withStore
  , readVar
  , writeVar
  , runWithStore
  , runWithStoreIO
  )
where

import           Control.Monad.State            ( StateT(runStateT)
                                                , get
                                                , put
                                                )
import qualified Data.Map                      as Map
import           System.IO                      ( getLine
                                                , putStr
                                                )
import           RIO
import           Utils.Types
import           Utils.Types.App                ( Interp(..) )

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

runWithStore :: Interp a v -> Interp a ()
runWithStore interp = withStore $ Interp . lift . runWithStoreIO interp

runWithStoreIO :: Interp a v -> Store -> RIO a Store
runWithStoreIO interp store@(Right _) =
  (runStateT . runInterp) interp (globalL, store) <&> snd . snd
runWithStoreIO _ _ = return (Left ())

readVar :: Var -> Interp a Val
readVar x = getStore >>= \case
  Right store -> case Map.lookup x (view (scope localL) store) of
    Just v  -> return v
    Nothing -> case Map.lookup x (view (scope globalL) store) of
      Just v  -> return v
      Nothing -> return Null
  _ -> return Null

writeVar :: Var -> Val -> Interp a Val
writeVar x v = do
  s <- getScope
  withScopes $ (over $ scope s) (Map.insert x v)
  return v

readVal :: Interp a String
readVal = Interp . lift . liftIO $ getLine

printVal :: Show v => v -> Interp a ()
printVal = Interp . lift . liftIO . putStr . show

printString :: String -> Interp a ()
printString = Interp . lift . liftIO . putStr
