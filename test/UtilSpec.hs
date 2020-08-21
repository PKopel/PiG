{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Utils.Util

spec :: Spec
spec = undefined {- do
                   describe "plus2" $ do
                     it "basic check" $ plus2 0 `shouldBe` 2
                     it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
                     prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i
                 -}