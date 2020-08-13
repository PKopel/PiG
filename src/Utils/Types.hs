{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Types where

import Control.Monad.State
import Data.List
import Data.Map
import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x {appProcessContext = y})

infix 1 :=

data Expr
  = Var Var
  | Val Val
  | Neg Expr
  | BoolBinary BoolBinOp Expr Expr
  | RelBinary RelBinOp Expr Expr
  | AlgBinary AlgBinOp Expr Expr
  | ListUnary ListUnOp Expr
  | ListBinary ListBinOp Expr Expr
  deriving (Show)

data BoolBinOp = And | Or deriving (Show)

data RelBinOp = Greater | Less | Equal deriving (Show)

data ListBinOp = AddFirst | AddLast deriving (Show)

data ListUnOp = RmFirst | RmLast deriving (Show)

data AlgBinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Power
  deriving (Show)

data Stmt
  = Var := Expr
  | While Expr Stmt
  | If Expr Stmt Stmt
  | Seq [Stmt]
  | Print Expr
  | Skip
  deriving (Show)

data Val = AlgVal Double | BoolVal Bool | ListVal [Val] | Empty

instance Show Val where
  show (AlgVal v) = show v
  show (BoolVal True) = "true"
  show (BoolVal False) = "false"
  show (ListVal v) = '[' : intercalate ", " (show <$> v) ++ "]"
  show Empty = "null"

type Var = String

type Prog = Stmt

type Store = Map Var Val

newtype Interp a = Interp {runInterp :: StateT Store (RIO App) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState Store
    )

getStore :: Interp Store
getStore = get

putStore :: Store -> Interp ()
putStore = put

runWithStore :: Interp a -> Store -> RIO App (a, Store)
runWithStore = runStateT . runInterp
