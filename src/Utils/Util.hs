{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( printVal
  , printString
  , isVar
  , readVar
  , writeVar
  , runWithStore
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

import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           RIO
import           System.Console.Haskeline
import           Utils.Types

getStore :: Interp Store
getStore = get >>= return . snd

getScope :: Interp Scope
getScope = get >>= return . fst

putStore :: Store -> Interp ()
putStore s' = do
  (w, _) <- get
  put (w, s')

setScope :: Scope -> Interp ()
setScope w' = do
  (_, s) <- get
  put (w', s)

withStore :: (Store -> Store) -> Interp ()
withStore f = getStore >>= putStore . f

runWithStore :: Interp a -> Store -> InputT IO Store
runWithStore interp store =
  (runStateT . runInterp) interp (globalL, store) >>= return . snd . snd

getElems :: (Foldable t) => (Seq Val) -> t Double -> (Seq Val)
getElems list = foldl'
  (\acc v -> case (Seq.<|) <$> ((list Seq.!?) $ round v) <*> pure acc of
    Nothing   -> acc
    Just acc' -> acc'
  )
  Seq.empty

isVar :: Expr -> (Bool, Var)
isVar (Var x) = (True, x)
isVar _       = (False, "")

readVar :: Var -> Interp Val
readVar x = do
  store <- getStore
  case Map.lookup x (view (scope localL) store) of
    Just v  -> return v
    Nothing -> case Map.lookup x (view (scope globalL) store) of
      Just v  -> return v
      Nothing -> return Null

writeVar :: Var -> Val -> Interp Val
writeVar x v = do
  s <- getScope
  withStore $ (over $ scope s) (Map.insert x v)
  return v

printVal :: Show a => a -> Interp ()
printVal = Interp . lift . outputStr . show

printString :: String -> Interp ()
printString = Interp . lift . outputStr

(>-) :: Seq Val -> (Val, Seq Val)
(>-) (h :<| t) = (h, t)
(>-) l         = (Null, l)

(-<) :: Seq Val -> (Val, Seq Val)
(-<) (i :|> l) = (l, i)
(-<) l         = (Null, l)
