{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec (spec) where

import Import hiding (assert)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TypesSpec ()

prop_putGetStore :: Store -> Gen (Interp Property)
prop_putGetStore s = monadic' $ do
  s' <- run (putStore s >> getStore)
  assert (s == s')

prop_setGetScope :: Scope -> Store -> Gen (Interp Property)
prop_setGetScope sc st = monadic' $ do
  sc' <- run (setScope sc >> getScope)
  let v = view (scope sc) st
      v' = view (scope sc') st
  assert (v == v')

prop_withStore :: Store -> Gen (Interp Property)
prop_withStore s = monadic' $ do
  s' <- run (putStore s >> withStore id >> getStore)
  assert (s == s')

spec :: Spec
spec = undefined {- do
                   describe "plus2" $ do
                     it "basic check" $ plus2 0 `shouldBe` 2
                     it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
                     prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i
                 -}