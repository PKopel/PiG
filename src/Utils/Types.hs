{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Types where

import           Control.Monad.State
import           Data.List
import           Data.Map
import           RIO
import           RIO.Process
import           System.Console.Haskeline

data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appSettings :: !(Settings IO)
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
  | BoolBinary BoolBinOp Expr Expr
  | RelBinary RelBinOp Expr Expr
  | AlgBinary AlgBinOp Expr Expr
  | ListUnary ListUnOp Expr
  | ListBinary ListBinOp Expr Expr
  | FunApp Var [Expr]
  deriving (Show)

data BoolBinOp = And | Or deriving (Show)

data RelBinOp = Greater | Less | Equal deriving (Show)

data ListBinOp = Concat deriving (Show)

data ListUnOp = RmFirst | RmLast deriving (Show)

data AlgBinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Power
  deriving (Show)

data Stmt
  = Assign Var Expr
  | While Expr Stmt
  | If Expr Stmt Stmt
  | Seq [Stmt]
  | Print Expr
  | Skip
  deriving (Show)

data Val = AlgVal Double | BoolVal Bool | ListVal [Val] | FunVal [Var] Stmt Expr | Null

instance Show Val where
  show (AlgVal  v    ) = show v
  show (BoolVal True ) = "true"
  show (BoolVal False) = "false"
  show (ListVal v    ) = '[' : intercalate ", " (show <$> v) ++ "]"
  show (FunVal _ _ _ ) = "function"
  show Null            = "null"

type Var = String

data Drct = Exit | Clear | Help | Rm Var | Load FilePath deriving (Show)

data Prog = Stmt Stmt | Drct Drct deriving (Show)

data Store = Store {gVars :: Map Var Val, lVars :: Map Var Val} deriving (Show)

emptyStore :: Store
emptyStore = Store { gVars = empty, lVars = empty }

globalL :: Lens' Store (Map Var Val)
globalL = lens gVars (\x y -> x { gVars = y })

localL :: Lens' Store (Map Var Val)
localL = lens lVars (\x y -> x { lVars = y })

getLocals :: Store -> Map Var Val
getLocals = view localL

getGlobals :: Store -> Map Var Val
getGlobals = view globalL

setLocals :: (Map Var Val -> Map Var Val) -> Store -> Store
setLocals = over localL

setGlobals :: (Map Var Val -> Map Var Val) -> Store -> Store
setGlobals = over globalL

newtype Interp a = Interp {runInterp :: StateT Store (InputT IO) a}
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

withStore :: (Store -> Store) -> Interp ()
withStore f = getStore >>= putStore . f

runWithStore :: Interp a -> Store -> InputT IO (a, Store)
runWithStore = runStateT . runInterp
