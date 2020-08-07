{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils.Types where

import           Control.Monad.State
import           Data.Map
import           RIO
import           RIO.Process

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
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

infix 1 :=

data Expr
  = A AlgExpr
  | B BoolExpr
  deriving (Show)

data BoolExpr
  = BoolVar Var
  | BoolConst (Maybe Bool)
  | Not BoolExpr
  | BoolBinary BoolBinOp BoolExpr BoolExpr
  | RelBinary RelBinOp AlgExpr AlgExpr
  deriving (Show)

data BoolBinOp = And | Or deriving (Show)

data RelBinOp = Greater | Less deriving (Show)

data AlgExpr
  = AlgVar Var
  | AlgConst (Maybe Double)
  | Neg AlgExpr
  | AlgBinary AlgBinOp AlgExpr AlgExpr
  deriving (Show)

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

data Val = AlgVal Double | BoolVal Bool | Empty deriving (Show)

type Var = String

type Prog = Stmt

type Store = Map Var Val

type Interp = StateT Store (RIO App)

getStore :: Interp Store
getStore = get

putStore :: Store -> Interp ()
putStore = put

runInterp :: Interp a -> Store -> RIO App (a, Store)
runInterp = runStateT

algVal :: Maybe Double -> Val
algVal Nothing  = Empty
algVal (Just v) = AlgVal v

boolVal :: Maybe Bool -> Val
boolVal Nothing  = Empty
boolVal (Just v) = BoolVal v
