{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( readVal
  , printVal
  , printString
  , isVar
  , readVar
  , writeVar
  , runWithStore
  , runWithStoreIO
  , getElems
  , getStore
  , getScope
  , putStore
  , setScope
  , withStore
  , (>-)
  , (-<)
  , module Utils.Types
  )
where

import           Control.Monad.State            ( StateT(runStateT)
                                                , get
                                                , put
                                                )
import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import           RIO
import           System.IO                      ( getLine
                                                , putStr
                                                )
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

getElems :: (Foldable t, Container s, Monoid (s a)) => s a -> t Double -> s a
getElems list = foldl'
  (\acc v -> fromMaybe acc ((<|) <$> list !? round v <*> pure acc))
  mempty

isVar :: Expr -> (Bool, Var)
isVar (Var x) = (True, x)
isVar _       = (False, "")

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

(>-) :: Seq Val -> (Val, Seq Val)
(>-) (h :<| t) = (h, t)
(>-) l         = (Null, l)

(-<) :: Seq Val -> (Val, Seq Val)
(-<) (i :|> l) = (l, i)
(-<) l         = (Null, l)
