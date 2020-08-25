{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec
  ( spec
  )
where

import           Control.Monad.State
import           Import                  hiding ( assert )
import           System.Console.Haskeline
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           TypesSpec                      ( )

interpToProp :: Interp a -> (Scope, Store) -> PropertyM IO a
interpToProp i s =
  run . runInputT defaultSettings $ (runStateT . runInterp) i s >>= return . fst

prop_getStore :: (Scope, Store) -> Property
prop_getStore ss@(_, st) = monadicIO $ do
  st' <- interpToProp getStore ss
  assert (st == st')

prop_getScope :: (Scope, Store) -> Property
prop_getScope ss@(sc, st) = monadicIO $ do
  sc' <- interpToProp getScope ss
  let v  = view (scope sc) st
      v' = view (scope sc') st
  assert (v == v')

prop_putStore :: (Scope, Store) -> Store -> Property
prop_putStore ss st = monadicIO $ do
  st' <- interpToProp (putStore st >> getStore) ss
  assert (st == st')

prop_setScope :: (Scope, Store) -> Scope -> Property
prop_setScope ss@(_, st) sc = monadicIO $ do
  sc' <- interpToProp (setScope sc >> getScope) ss
  let v  = view (scope sc) st
      v' = view (scope sc') st
  assert (v == v')

prop_withStore :: (Scope, Store) -> Property
prop_withStore ss@(_, st) = monadicIO $ do
  st' <- interpToProp (withStore id >> getStore) ss
  assert (st == st')

prop_getElems :: [Integer] -> Seq Val -> Bool
prop_getElems ids seqv =
  getElems seqv ids'
    == getElems seqv ids''
    && getElems seqv ids'
    == getElems seqv ids'''
 where
  ids'   = fromInteger <$> ids
  ids''  = ((+ 0.49) . fromInteger) <$> ids
  ids''' = ((subtract 0.49) . fromInteger) <$> ids

spec :: Spec
spec = do
  describe "Utils.Util" $ do
    modifyMaxSuccess (const 1) $ do
      it "Utils.Util.getStore" $ property prop_getStore
      it "Utils.Util.getScope" $ property prop_getScope
      it "Utils.Util.putStore" $ property prop_putStore
      it "Utils.Util.setScope" $ property prop_setScope
      it "Utils.Util.withStore" $ property prop_withStore
      it "Utils.Util.getElems" $ property prop_getElems
