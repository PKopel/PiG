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

getStore :: Interp Store
getStore = get <&> snd

getScope :: Interp Scope
getScope = get <&> fst

putStore :: Store -> Interp ()
putStore s' = do
  (w, _) <- get
  put (w, s')

setScope :: Scope -> Interp ()
setScope w' = do
  (_, s) <- get
  put (w', s)

withStore :: (Scopes -> Scopes) -> Interp ()
withStore f = getStore >>= \case
  Right store -> putStore . Right . f $ store
  _           -> return ()

runWithStore :: Interp a -> Interp ()
runWithStore interp =
  getStore >>= Interp . lift . runWithStoreIO interp >>= putStore

runWithStoreIO :: Interp a -> Store -> IO Store
runWithStoreIO interp store@(Right _) =
  (runStateT . runInterp) interp (globalL, store) <&> snd . snd
runWithStoreIO _ _ = return (Left ())

readVar :: Var -> Interp Val
readVar x = getStore >>= \case
  Right store -> case Map.lookup x (view (scope localL) store) of
    Just v  -> return v
    Nothing -> case Map.lookup x (view (scope globalL) store) of
      Just v  -> return v
      Nothing -> return Null
  _ -> return Null

writeVar :: Var -> Val -> Interp Val
writeVar x v = do
  s <- getScope
  withStore $ (over $ scope s) (Map.insert x v)
  return v

readVal :: Interp String
readVal = Interp . lift $ getLine

printVal :: Show a => a -> Interp ()
printVal = Interp . lift . putStr . show

printString :: String -> Interp ()
printString = Interp . lift . putStr
