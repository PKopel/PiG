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

instance BinAppToVal (Double -> Double -> Bool) where
  appBin op (AlgVal a) (AlgVal b) = BoolVal $ op a b
  appBin _  _          _          = Null

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
  | forall op. (BinAppToVal op) => Binary op Expr Expr
  | forall op. (UnAppToVal op) => Unary op Expr

instance Eq Expr where
  (Var v1) == (Var v2) = v1 == v2
  (Val v1) == (Val v2) = v1 == v2
  _        == _        = False

data Val
  = AlgVal Double
  | BoolVal Bool
  | CharVal Char
  | StrVal String
  | ListVal (Seq Val)
  | FunVal [Var] Expr
  | Null
  deriving (Eq)

instance Show Val where
  show (AlgVal  v    ) = show v
  show (CharVal v    ) = [v]
  show (BoolVal True ) = "true"
  show (BoolVal False) = "false"
  show (StrVal  v    ) = v
  show (ListVal v    ) = '[' : intercalate ", " (toList $ show <$> v) ++ "]"
  show (FunVal _ _   ) = "function"
  show Null            = "null"

type Var = String

type Bindings = Map Var Val

newtype Scope = Scope {scope :: Lens' Store Bindings}

data Store = Store {globalS :: Bindings, localS :: Bindings} deriving (Show)

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
