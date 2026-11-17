{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypesSpec
  ( spec
  )
where

import           Utils.Types
import           RIO
import qualified RIO.Map                       as Map
import           RIO.Seq                        ( fromList )
import           Data.Text.Lazy
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Expr where
  arbitrary = oneof
    [ Var <$> arbitrary
    , Val <$> arbitrary
    , Assign <$> arbitrary <*> arbitrary <*> arbitrary
    , ListLiteral <$> arbitrary
    , MapLiteral <$> arbitrary
    , FunApp <$> arbitrary <*> arbitrary
    , Load <$> arbitrary
    , While <$> arbitrary <*> arbitrary
    , If <$> arbitrary <*> arbitrary
    , Seq <$> arbitrary
    , Return <$> arbitrary
    ]

instance Arbitrary Val where
  arbitrary = oneof
    [ return Null
    , AlgVal <$> arbitrary
    , BoolVal <$> arbitrary
    , CharVal <$> arbitrary
    , StrVal <$> arbitrary
    , ListVal <$> arbitrary
    , MapVal . Map.fromList <$> arbitrary
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
spec = describe "Utils.Types" $ do
  prop_emptyStore
  prop_argsStore

prop_emptyStore :: SpecWith ()
prop_emptyStore = describe "emptyStore" $ do
  it "starts with no global or local bindings" $ do
    let store = emptyStore
    RIO.null (globalS store) `shouldBe` True
    RIO.null (localS store) `shouldBe` True

prop_argsStore :: SpecWith ()
prop_argsStore = describe "argsStore" $ do
  it "stores script arguments as a list in the global scope" $ do
    let store = argsStore ["first", "second"]
        expected = ListVal (fromList [StrVal "first", StrVal "second"])
    Map.lookup "args" (globalS store) `shouldBe` Just expected
    RIO.null (localS store) `shouldBe` True
