{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( isVar
  , getElems
  , (>-)
  , (-<)
  ) where

import           Data.List                      ( (!!) )
import           RIO
import           RIO.Seq                        ( (!?)
                                                , (<|)
                                                , Seq(..)
                                                )
import           Utils.Types

class Container c where
  (!!?) :: c a -> Int -> Maybe a
  (<:) :: a -> c a -> c a

instance Container Seq where
  (!!?) = (!?)
  (<:)  = (<|)

instance Container [] where
  l !!? i | length l < i = Nothing
          | otherwise    = Just (l !! i)
  (<:) = (:)

getElems :: (Foldable t, Container s, Monoid (s a)) => s a -> t Double -> s a
getElems list = foldl'
  (\acc v -> fromMaybe acc ((<:) <$> list !!? round v <*> pure acc))
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
