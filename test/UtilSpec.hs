{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec
  ( spec
  )
where

import           Control.Monad.State            ( StateT(runStateT) )
import           Import                  hiding ( assert )
import           System.Console.Haskeline       ( defaultSettings
                                                , runInputT
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.QuickCheck                ( Property
                                                , Testable(property)
                                                )
import           Test.QuickCheck.Monadic        ( PropertyM
                                                , assert
                                                , monadicIO
                                                , run
                                                )
import           TypesSpec                      ( )

interpToProp :: Interp a -> (Scope, Store) -> PropertyM IO a
interpToProp i s =
  run . runInputT defaultSettings $ (runStateT . runInterp) i s <&> fst

prop_getStore :: (Scope, Store) -> Property
prop_getStore ss@(_, st) = monadicIO $ do
  st' <- interpToProp getStore ss
  assert (st == st')

prop_getScope :: (Scope, Store) -> Property
prop_getScope ss@(sc, Right st) = monadicIO $ do
  sc' <- interpToProp getScope ss
  let v  = view (scope sc) st
      v' = view (scope sc') st
  assert (v == v')
prop_getScope _ = property True

prop_putStore :: (Scope, Store) -> Store -> Property
prop_putStore ss st = monadicIO $ do
  st' <- interpToProp (putStore st >> getStore) ss
  assert (st == st')

prop_setScope :: (Scope, Store) -> Scope -> Property
prop_setScope ss@(_, Right st) sc = monadicIO $ do
  sc' <- interpToProp (setScope sc >> getScope) ss
  let v  = view (scope sc) st
      v' = view (scope sc') st
  assert (v == v')
prop_setScope _ _ = property True

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
  ids''  = (+ 0.49) . fromInteger <$> ids
  ids''' = subtract 0.49 . fromInteger <$> ids

spec :: Spec
spec = describe "Utils.Util" $ do
  modifyMaxSuccess (const 1) $ do
    it "Utils.Util.getStore" $ property prop_getStore
    it "Utils.Util.getScope" $ property prop_getScope
    it "Utils.Util.putStore" $ property prop_putStore
    it "Utils.Util.setScope" $ property prop_setScope
    it "Utils.Util.withStore" $ property prop_withStore
    it "Utils.Util.getElems" $ property prop_getElems
