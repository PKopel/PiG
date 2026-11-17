{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UtilSpec
  ( spec
  )
where

import           Utils.Types
import           Utils.Interp                   ( getScope
                                                , getStore
                                                , getVar
                                                , putStore
                                                , putVar
                                                , runWithStore
                                                , interpWithStore
                                                , setScope
                                                , withScopes
                                                )
import           Utils.Util                     ( getElems
                                                , isVar
                                                , (-<)
                                                , (>-)
                                                )
import           Control.Monad.State            ( StateT(runStateT) )
import           RIO                     hiding ( assert )
import qualified RIO.Map                       as Map
import           RIO.Seq                        ( fromList )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                , expectationFailure
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

interpToProp :: Interp () v -> (Scope, Store) -> PropertyM IO v
interpToProp i s = run . runRIO () $ (runStateT . runInterp) i s <&> fst

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
  st' <- interpToProp (withScopes id >> getStore) ss
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
    it "getStore returns the original interpreter store" $ property prop_getStore
    it "getScope identifies the original active scope" $ property prop_getScope
    it "putStore replaces the interpreter store" $ property prop_putStore
    it "setScope changes the active scope" $ property prop_setScope
    it "withScopes preserves the store for an identity transformation" $ property prop_withStore
    it "getElems rounds indices consistently" $ property prop_getElems

  it "getVar prefers local bindings over global bindings" $ do
    let store = Right (Scopes
          { globalS = Map.singleton "x" (AlgVal 1)
          , localS = Map.singleton "x" (AlgVal 2)
          })
    (_, _, val) <- runRIO () $ do
      (val, (_, finalStore)) <- (runStateT . runInterp) (getVar "x") (globalL, store)
      return (store, finalStore, val)
    val `shouldBe` AlgVal 2

  it "getVar returns Null for an unbound name" $ do
    (val, _) <- runRIO () $ (runStateT . runInterp) (getVar "missing")
      (globalL, Right emptyStore)
    val `shouldBe` Null

  it "putVar adds a value to the selected scope" $ do
    (_, (_, stateStore)) <- runRIO () $ (runStateT . runInterp)
      (setScope localL >> putVar "answer" (AlgVal 42) >> getStore)
      (globalL, Right emptyStore)
    case stateStore of
      Right scopes -> Map.lookup "answer" (localS scopes) `shouldBe` Just (AlgVal 42)
      Left code -> expectationFailure $ "unexpected exit code: " <> show code

  it "runWithStore executes an interpreter action and returns its updated store" $ do
    finalStore <- runRIO () $ runWithStore
      (putVar "answer" (AlgVal 42))
      (Right emptyStore)
    case finalStore of
      Right scopes -> Map.lookup "answer" (globalS scopes) `shouldBe` Just (AlgVal 42)
      Left code -> expectationFailure $ "unexpected exit code: " <> show code

  it "interpWithStore applies its nested interpreter to the surrounding store" $ do
    (_, (_, finalStore)) <- runRIO () $ (runStateT . runInterp)
      (interpWithStore (putVar "answer" (AlgVal 42)))
      (globalL, Right emptyStore)
    case finalStore of
      Right scopes -> Map.lookup "answer" (globalS scopes) `shouldBe` Just (AlgVal 42)
      Left code -> expectationFailure $ "unexpected exit code: " <> show code

  it "isVar recognizes only variable expressions" $ do
    isVar (Var "x") `shouldBe` (True, "x")
    isVar (Val Null) `shouldBe` (False, "")

  it "first and last removal utilities return the removed element and remainder" $ do
    let values = fromList [AlgVal 1, AlgVal 2, AlgVal 3]
    (>-) values `shouldBe` (AlgVal 1, fromList [AlgVal 2, AlgVal 3])
    (-<) values `shouldBe` (AlgVal 3, fromList [AlgVal 1, AlgVal 2])
