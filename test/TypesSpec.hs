{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TypesSpec
  ( spec,
  )
where

import Import
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Expr where
  arbitrary =
    oneof
      [ return Read,
        Var <$> arbitrary,
        Val <$> arbitrary,
        Assign <$> arbitrary <*> arbitrary <*> arbitrary,
        ListLiteral <$> arbitrary,
        FunApp <$> arbitrary <*> arbitrary,
        While <$> arbitrary <*> arbitrary,
        If <$> arbitrary <*> arbitrary <*> arbitrary,
        Seq <$> arbitrary,
        Print <$> arbitrary
      ]

instance Arbitrary Val where
  arbitrary =
    oneof
      [ return Null,
        AlgVal <$> arbitrary,
        BoolVal <$> arbitrary,
        CharVal <$> arbitrary,
        StrVal <$> arbitrary,
        ListVal <$> arbitrary,
        FunVal <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Store where
  arbitrary = do
    g <- arbitrary
    l <- arbitrary
    return Store {globalS = g, localS = l}

instance Arbitrary Scope where
  arbitrary = elements [globalL, localL]

spec :: SpecWith ()
spec = describe "Utils.Types" $ do
  prop_emptyStore
  prop_appUn
  prop_appBin

prop_emptyStore :: SpecWith ()
prop_emptyStore =
  describe "Utils.Types.emptyStore" $ do
    it "returns empty store" $ do
      let store = emptyStore
      null (globalS store) `shouldBe` True
      null (localS store) `shouldBe` True

prop_appUn :: SpecWith ()
prop_appUn = describe "Utils.Types.UnAppToVal" $ do
  it "Double -> Double instance" $
    property prop_appUnDoubleDouble
  it "Bool -> Bool instance" $
    property prop_appUnBoolBool

prop_appUnDoubleDouble :: Val -> Bool
prop_appUnDoubleDouble x =
  let result = case x of
        AlgVal a -> AlgVal (negate a)
        _ -> Null
   in appUn (negate :: Double -> Double) x == result

prop_appUnBoolBool :: Val -> Bool
prop_appUnBoolBool x =
  let result = case x of
        BoolVal a -> BoolVal (not a)
        _ -> Null
   in appUn (not :: Bool -> Bool) x == result

prop_appBin :: SpecWith ()
prop_appBin = describe "Utils.Types.BinAppToVal" $ do
  it "Double -> Double -> Double instance" $
    property prop_appBinDoubleDoubleDouble
  it "Bool -> Bool -> Bool instance" $
    property prop_appBinBoolBoolBool
  it "String -> String -> String instance" $
    property prop_appBinStrStrStr
  it "Seq Val -> Seq Val -> Seq Val instance" $
    property prop_appBinSeqSeqSeq

prop_appBinBoolBoolBool :: Val -> Val -> Bool
prop_appBinBoolBoolBool a b = case appBin (||) a b of
  BoolVal _ -> True
  _ -> False

prop_appBinSeqSeqSeq :: Val -> Val -> Bool
prop_appBinSeqSeqSeq a b = case appBin ((<>) :: Seq Val -> Seq Val -> Seq Val) a b of
  ListVal _ -> True
  _ -> False

prop_appBinStrStrStr :: Val -> Val -> Bool
prop_appBinStrStrStr a b = case appBin ((<>) :: String -> String -> String) a b of
  StrVal _ -> True
  _ -> False

prop_appBinDoubleDoubleDouble :: Val -> Val -> Bool
prop_appBinDoubleDoubleDouble a b =
  let result = case (a, b) of
        (AlgVal x, AlgVal y) -> AlgVal (x + y)
        _ -> Null
   in appBin ((+) :: Double -> Double -> Double) a b == result