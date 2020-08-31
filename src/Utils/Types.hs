{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Types where

import           Control.Monad.State
import           Data.List
import qualified Data.Sequence                 as Seq
import           Data.Version                   ( Version )
import           RIO
import           RIO.Process
import           System.Console.Haskeline

data Options = Options
  { optionsVerbose :: !Bool,
    optionsLoad :: !String
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appSettings :: !(Settings IO),
    appVersion :: !Version
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

class UnAppToVal op where
  appUn :: op -> Val -> Val

instance UnAppToVal (Double -> Double) where
  appUn op (AlgVal a) = AlgVal $ op a
  appUn _  _          = Null

instance UnAppToVal (Bool -> Bool) where
  appUn op (BoolVal a) = BoolVal $ op a
  appUn _  _           = Null

instance UnAppToVal (Seq Val -> (Val, Seq Val)) where
  appUn op (ListVal a) =
    let (v, l) = op a in ListVal $ Seq.fromList [v, ListVal l]
  appUn _ _ = Null

class BinAppToVal op where
  appBin :: op -> Val -> Val -> Val

instance BinAppToVal (Val -> Val -> Bool) where
  appBin op a b = BoolVal $ op a b

instance BinAppToVal (Bool -> Bool -> Bool) where
  appBin op a b = BoolVal $ op (toBool a) (toBool b)
   where
    toBool (BoolVal v) = v
    toBool (AlgVal  v) = v /= 0
    toBool (ListVal v) = not (null v)
    toBool (StrVal  v) = not (null v)
    toBool Null        = False
    toBool _           = True

instance BinAppToVal (Double -> Double -> Double) where
  appBin op (AlgVal a) (AlgVal b) = AlgVal $ op a b
  appBin _  _          _          = Null

instance BinAppToVal (String -> String -> String) where
  appBin op a b = StrVal $ op (toStr a) (toStr b)
   where
    toStr (FunVal _ _) = ""
    toStr v            = show v

instance BinAppToVal (Seq.Seq Val -> Seq.Seq Val -> Seq.Seq Val) where
  appBin op a b = ListVal $ op (toSeq a) (toSeq b)
   where
    toSeq (ListVal v) = v
    toSeq v           = Seq.singleton v

data Prog = Stmt Expr | Drct Drct

data Drct
  = Exit
  | Clear
  | Help
  | Rm Var
  | Load FilePath

data Expr
  = Var Var
  | Val Val
  | Assign Var Expr Expr
  | ListLiteral [Expr]
  | FunApp Var [Expr]
  | While Expr Expr
  | If Expr Expr Expr
  | Seq [Expr]
  | Print [Expr]
  | Read
  | forall op. (BinAppToVal op) => Binary op Expr Expr
  | forall op. (UnAppToVal op) => Unary op Expr

instance Eq Expr where
  (Assign a b c ) == (Assign d e f ) = a == d && b == e && c == f
  (If     a b c ) == (If     d e f ) = a == d && b == e && c == f
  (FunApp a b   ) == (FunApp d e   ) = a == d && b == e
  (While  a b   ) == (While  d e   ) = a == d && b == e
  (Binary _ a b ) == (Binary _ d e ) = a == d && b == e
  (Unary _ a    ) == (Unary _ d    ) = a == d
  (Var         a) == (Var         d) = a == d
  (Val         a) == (Val         d) = a == d
  (Seq         a) == (Seq         d) = a == d
  (Print       a) == (Print       d) = a == d
  (ListLiteral a) == (ListLiteral d) = a == d
  Read            == Read            = True
  _               == _               = False

instance Ord Expr where
  (Var v1) <= (Var v2) = v1 <= v2
  (Val v1) <= (Val v2) = v1 <= v2
  _        <= _        = False

instance Show Expr where
  show (Var v       ) = "Var " <> v
  show (Val v       ) = "Val " <> show v
  show (Assign a b c) = intercalate " " ["Assign", a, show b, show c]
  show (ListLiteral v) =
    "ListLiteral" ++ '[' : intercalate ", " (show <$> v) ++ "]"
  show (FunApp a b) =
    "FunApp " <> a ++ '(' : intercalate ", " (show <$> b) ++ ")"
  show (Print b  )    = "Print" ++ '(' : intercalate ", " (show <$> b) ++ ")"
  show (While a b)    = intercalate " " ["While", show a, show b]
  show (If a b c )    = intercalate " " ["If", show a, show b, show c]
  show (Seq v    )    = "Seq " ++ '{' : intercalate "; " (show <$> v) ++ "}"
  show Read           = "Read"
  show (Binary _ a b) = intercalate " " ["Binary", show a, show b]
  show (Unary _ a   ) = "Unary " <> show a

data Val
  = AlgVal Double
  | BoolVal Bool
  | CharVal Char
  | StrVal String
  | ListVal (Seq Val)
  | FunVal [Var] Expr
  | Null
  deriving (Eq, Ord)

instance Show Val where
  show (AlgVal  v    ) = show v
  show (CharVal v    ) = [v]
  show (BoolVal True ) = "true"
  show (BoolVal False) = "false"
  show (StrVal  v    ) = v
  show (ListVal v    ) = '[' : intercalate ", " (toList $ show <$> v) ++ "]"
  show (FunVal n _) = '(' : intercalate ", " (show <$> n) ++ ")" ++ " => func"
  show Null            = "null"

type Var = String

type Bindings = Map Var Val

newtype Scope = Scope {scope :: Lens' Store Bindings}

instance Show Scope where
  show _ = "scope"

data Store = Store {globalS :: Bindings, localS :: Bindings} deriving (Show)

instance Eq Store where
  s1 == s2 = (globalS s1 == globalS s2) && (localS s1 == localS s2)

emptyStore :: Store
emptyStore = Store { globalS = mempty, localS = mempty }

globalL :: Scope
globalL = Scope $ lens globalS (\x y -> x { globalS = y })

localL :: Scope
localL = Scope $ lens localS (\x y -> x { localS = y })

newtype Interp a = Interp {runInterp :: StateT (Scope, Store) (InputT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState (Scope, Store)
    )
