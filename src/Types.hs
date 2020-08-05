{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad
import Control.Monad.State
import Data.Map
import Control.Monad.Writer
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

infixl 6 :+:, :-:

infixl 7 :*:, :/:

infix 1 :=

data Exp
  = C Val
  | V Var
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Exp :/: Exp
  | Exp :^: Exp
  deriving (Show)

data Stmt
  = Var := Exp
  | While Exp Stmt
  | Seq [Stmt]
  | Print Var
  deriving (Show)

type Var = Text

type Operator = Text

type Prog = Stmt

type Val = Double

type Store = Map Var Val

newtype Interp a = Interp {runInterp :: WriterT [Text] (StateT Store (RIO App)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadWriter [Text],
      MonadState Store
    )

getStore :: Interp Store
getStore = get