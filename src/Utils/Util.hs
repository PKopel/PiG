{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( bifs
  , isVar
  , getElems
  , (>-)
  , (-<)
  )
where

import           Data.Sequence                  ( Seq(..) )
import           RIO
import           Utils.Types


bifs :: [String]
bifs = ["read", "print", "load", "exit", ":print"]

getElems :: (Foldable t, Container s, Monoid (s a)) => s a -> t Double -> s a
getElems list = foldl'
  (\acc v -> fromMaybe acc ((<|) <$> list !? round v <*> pure acc))
  mempty

isVar :: Expr -> (Bool, Var)
isVar (Var x) = (True, x)
isVar _       = (False, "")

(>-) :: Seq Val -> (Val, Seq Val)
(>-) (h :<| t) = (h, t)
(>-) l         = (Null, l)

(-<) :: Seq Val -> (Val, Seq Val)
(-<) (i :|> l) = (l, i)
(-<) l         = (Null, l)
