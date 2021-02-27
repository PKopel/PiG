{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TypesSpec
  ( spec
  )
where

import           Utils.Types
import           RIO
import           Data.Text.Lazy
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Expr where
  arbitrary = oneof
    [ Var <$> arbitrary
    , Val <$> arbitrary
    , Assign <$> arbitrary <*> arbitrary <*> arbitrary
    , ListLiteral <$> arbitrary
    , FunApp <$> arbitrary <*> arbitrary
    , While <$> arbitrary <*> arbitrary
    , If <$> arbitrary <*> arbitrary
    , Seq <$> arbitrary
    ]

instance Arbitrary Val where
  arbitrary = oneof
    [ return Null
    , AlgVal <$> arbitrary
    , BoolVal <$> arbitrary
    , CharVal <$> arbitrary
    , StrVal <$> arbitrary
    , ListVal <$> arbitrary
    , FunVal <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Scopes where
  arbitrary = do
    g <- arbitrary
    l <- arbitrary
    return Scopes { globalS = g, localS = l }

instance Arbitrary Scope where
  arbitrary = elements [globalL, localL]

instance Arbitrary Data.Text.Lazy.Text where
  arbitrary = pack <$> arbitrary

spec :: Spec
spec = do
  describe "Utils.Types" $ do
    prop_emptyStore

prop_emptyStore :: SpecWith ()
prop_emptyStore = describe "Utils.Types.emptyStore" $ do
  it "returns empty store" $ do
    let store = emptyStore
    RIO.null (globalS store) `shouldBe` True
    RIO.null (localS store) `shouldBe` True
