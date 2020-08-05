{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Control.Monad
import Control.Monad.State
import Data.Map
import Control.Monad.Writer
import RIO
import RIO.Process
import RIO.Text

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

newtype Interp a = Interp {runInterp :: Store -> Either Text (a, Store)}

newtype Interp' a = Interp' {runInterp' :: WriterT [Text] StateT}

instance Functor Interp where
  fmap = liftM

instance Applicative Interp where
  pure = return
  (<*>) = ap

instance Monad Interp where
  return x = Interp $ \r -> Right (x, r)
  i >>= k = Interp $ \r -> case runInterp i r of
    Left msg -> Left msg
    Right (x, r') -> runInterp (k x) r'

instance MonadFail Interp where
  fail msg = Interp $ \_ -> Left $ pack msg