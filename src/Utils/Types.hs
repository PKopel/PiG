{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Types where

import           Control.Monad.State
import           Data.List
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

data Expr
  = Var Var
  | Val Val
  | Neg Expr
  | Assign Var (Maybe Expr) Expr
  | ListLiteral [Expr]
  | FunApp Var [Expr]
  | While Expr Expr
  | If Expr Expr Expr
  | Seq [Expr]
  | Print [Expr]
  | BoolBinary BoolBinOp Expr Expr
  | RelBinary RelBinOp Expr Expr
  | AlgBinary AlgBinOp Expr Expr
  | ListUnary ListUnOp Expr
  | ListBinary ListBinOp Expr Expr

type BoolBinOp = Bool -> Bool -> Bool

type RelBinOp = Double -> Double -> Bool

type ListBinOp = Seq Val -> Seq Val -> Seq Val

type ListUnOp = Seq Val -> (Val, Seq Val)

type AlgBinOp = Double -> Double -> Double

data Val = AlgVal Double | BoolVal Bool | ListVal (Seq Val) | FunVal [Var] Expr | Null

instance Show Val where
  show (AlgVal  v    ) = show v
  show (BoolVal True ) = "true"
  show (BoolVal False) = "false"
  show (ListVal v    ) = '[' : intercalate ", " (toList $ show <$> v) ++ "]"
  show (FunVal _ _   ) = "function"
  show Null            = "null"

type Var = String

newtype Scope = Scope {scope :: Lens' Store Bindings}

type Bindings = Map Var Val

data Drct = Exit | Clear | Help | Rm Var | Load FilePath deriving (Show)

data Prog = Stmt Expr | Drct Drct

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
