{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LanguageSpec
  ( spec
  )
where

import           Control.Monad.State            ( runStateT )
import qualified Data.Text.Lazy                as Lazy
import           Lang.Parser                    ( parseProg )
import           REPL.Eval                      ( eval )
import           RIO                     hiding ( assert )
import qualified RIO.Map                       as Map
import           RIO.Seq                        ( fromList )
import           Test.Hspec
import           Utils.Types

runPiG :: String -> IO (Either Lazy.Text Val)
runPiG source =
  case parseProg (Lazy.pack source) of
    Left err -> return (Left err)
    Right expr -> runRIO () $ do
      (val, _) <- (runStateT . runInterp) (eval expr)
        (globalL, Right emptyStore)
      return (Right val)

expectPiG :: String -> Val -> Expectation
expectPiG source expected = do
  result <- runPiG source
  case result of
    Left err -> expectationFailure (Lazy.unpack err)
    Right val -> val `shouldBe` expected

spec :: Spec
spec = do
  describe "PiG language parser" $ do
    it "accepts literals and comments" $ do
      let sources :: [String]
          sources =
            [ "null"
            , "true"
            , "false"
            , "42"
            , "3.5"
            , "'a'"
            , "\"hello\""
            , "[1, 2, 3]"
            , "[\"a\": 1, \"b\": 2]"
            , "# comment\n1"
            ]
      for_ sources $ \source ->
        parseProg (Lazy.pack source) `shouldSatisfy` isRight

    it "accepts sequences, assignments, control flow, and functions" $ do
      let sources :: [String]
          sources =
            [ "x = 1; x + 2"
            , "{ x = 1; x + 2 }"
            , "while false: 1"
            , "if true: 1 elif false: 2 else: 3"
            , "(x) => { x + 1 }"
            , "add = (x, y) => { x + y }; add(2, 3)"
            ]
      for_ sources $ \source ->
        parseProg (Lazy.pack source) `shouldSatisfy` isRight

  describe "PiG expression evaluation" $ do
    it "evaluates literal values" $ do
      expectPiG "null" Null
      expectPiG "true" (BoolVal True)
      expectPiG "false" (BoolVal False)
      expectPiG "42" (AlgVal 42)
      expectPiG "3.5" (AlgVal 3.5)
      expectPiG "'a'" (CharVal 'a')
      expectPiG "\"hello\"" (StrVal "hello")

    it "evaluates sequences and assignments" $ do
      expectPiG "x = 3; x + 2" (AlgVal 5)
      expectPiG "{ x = 2; y = x + 3; y * 2 }" (AlgVal 10)

    it "evaluates arithmetic and boolean operators" $ do
      expectPiG "1 + 2 * 3" (AlgVal 7)
      expectPiG "(8 - 2) / 3" (AlgVal 2)
      expectPiG "2 ^ 3" (AlgVal 8)
      expectPiG "11 % 4" (AlgVal 3)
      expectPiG "-4" (AlgVal (-4))
      expectPiG "true && false" (BoolVal False)
      expectPiG "false || true" (BoolVal True)
      expectPiG "~false" (BoolVal True)
      expectPiG "2 > 1" (BoolVal True)
      expectPiG "2 == 2" (BoolVal True)
      expectPiG "2 != 3" (BoolVal True)

    it "evaluates if expressions and while loops" $ do
      expectPiG "if false: 1 elif true: 2 else: 3" (AlgVal 2)
      expectPiG "if 0: 1 else: 2" (AlgVal 2)
      expectPiG "if \"yes\": 1 else: 2" (AlgVal 1)
      expectPiG "x = 3; r = 0; while x > 0: { r = r + x; x = x - 1 }; r"
        (AlgVal 6)

    it "evaluates list literals, indexing, and assignment" $ do
      expectPiG "[1,2,3]" (ListVal (fromList [AlgVal 1, AlgVal 2, AlgVal 3]))
      expectPiG "l = [10,20,30]; l(1)" (AlgVal 20)
      expectPiG "l = [10,20,30]; l(0,2)"
        (ListVal (fromList [AlgVal 10, AlgVal 30]))
      expectPiG "l = [10,20,30]; l(1) = 99; l(1)" (AlgVal 99)

    it "evaluates map literals and keyed assignment" $ do
      let m = MapVal (Map.singleton (StrVal "a") (AlgVal 1))
      expectPiG "[\"a\":1]" m
      expectPiG "m = [\"a\":1]; m(\"a\")" (AlgVal 1)
      expectPiG "m = [\"a\":1]; m(\"b\") = 2; m(\"b\")" (AlgVal 2)

    it "evaluates functions, closures, and recursion" $ do
      expectPiG "double = (x) => { x * 2 }; double(6)" (AlgVal 12)
      expectPiG "add = (x,y) => { x + y }; add(4,7)" (AlgVal 11)
      expectPiG "fact = (n) => { if n == 0: 1 else: n * fact(n - 1) }; fact(5)"
        (AlgVal 120)
      expectPiG "increment = (x) => { return x + 1 }; increment(4)" (AlgVal 5)

  describe "PiG built-ins and operators" $ do
    it "performs string conversions and type predicates" $ do
      expectPiG "strToNum(\"12\") + 1" (AlgVal 13)
      expectPiG "listToStr(strToList(\"pig\"))" (StrVal "pig")
      expectPiG "isNum(1)" (BoolVal True)
      expectPiG "isBool(true)" (BoolVal True)
      expectPiG "isList([1])" (BoolVal True)
      expectPiG "isStr(\"x\")" (BoolVal True)
      expectPiG "isFun((x)=>{x})" (BoolVal True)

    it "reports container lengths and string/list operations" $ do
      expectPiG "length([1,2,3])" (AlgVal 3)
      expectPiG "length(\"hello\")" (AlgVal 5)
      expectPiG "length([\"a\":1,\"b\":2])" (AlgVal 2)
      expectPiG "\"ab\" >< \"cd\"" (StrVal "abcd")
      expectPiG "[1,2] <> 3" (ListVal (fromList [AlgVal 1, AlgVal 2, AlgVal 3]))

    it "provides list first and last removal operations" $ do
      expectPiG "l = [1,2,3]; >- l" (AlgVal 1)
      expectPiG "l = [1,2,3]; -< l" (AlgVal 3)
