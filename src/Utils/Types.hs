{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Utils.Types where
import           Data.List                      ( intercalate )
import qualified Data.Text.Lazy                as Lazy
import           Data.Version                   ( Version )
import           RIO
import qualified RIO.Map                       as Map
import           RIO.Orphans                    ( )
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )
import           RIO.Seq                        ( fromList )
import           System.Console.Haskeline       ( Settings )

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                )
import           Control.Monad.State            ( MonadState
                                                , StateT(StateT)
                                                )

data Options = Options
  { optionsLoad :: String
  , scriptArgs  :: [String]
  }

data App = App
  { appLogFunc        :: LogFunc
  , appProcessContext :: ProcessContext
  , appOptions        :: Options
  , appSettings       :: Settings (Interp App)
  , appVersion        :: Version
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

newtype Interp app v = Interp {runInterp :: StateT (Scope, Store) (RIO app) v}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadState (Scope, Store)
    )

data Expr
  = Var Var
  | Val Val
  | Assign Var Expr Expr
  | ListLiteral [Expr]
  | MapLiteral [(Expr,Expr)]
  | FunApp Expr [Expr]
  | Load Expr
  | While Expr Expr
  | If [(Expr, Expr)] Expr
  | Seq [Expr]
  | Return Expr
  deriving(Eq, Ord)

instance Show Expr where
  show (Var v       ) = Lazy.unpack v
  show (Val v       ) = show v
  show (Assign a b c) = unwords [Lazy.unpack a, show b, " = ", show c]
  show (Load e      ) = "load " <> show e
  show (While a b   ) = unwords ["while", show a, show b]
  show (Seq    v    ) = '{' : intercalate "; " (show <$> v) <> "}"
  show (Return v    ) = "return" <> show v
  show (If a b) =
    unwords ["if", intercalate " elif " (show <$> a), "else", show b]
  show (ListLiteral v) = '[' : intercalate ", " (show <$> v) <> "]"
  show (MapLiteral  m) = '[' : intercalate ", " (showEntry <$> m) <> "]"
  show (FunApp a b   ) = show a <> "(" <> intercalate ", " (show <$> b) <> ")"

instance Ord Handle where
  _ <= _ = False

data Val
  = AlgVal Double
  | BoolVal Bool
  | CharVal Char
  | StrVal String
  | IOVal Handle
  | ListVal (Seq Val)
  | MapVal (Map Val Val)
  | FunVal [Var] Expr
  | Null
  deriving (Eq, Ord)

instance Show Val where
  show (AlgVal v) | v == fromInteger (round v) = show (round v :: Integer)
                  | otherwise                  = show v
  show (CharVal v    ) = [v]
  show (BoolVal True ) = "true"
  show (BoolVal False) = "false"
  show (StrVal  v    ) = v
  show (IOVal   v    ) = show v
  show (ListVal v    ) = '[' : intercalate ", " (toList $ show <$> v) <> "]"
  show (MapVal m) = '[' : intercalate ", " (showEntry <$> Map.toList m) <> "]"
  show (FunVal n _   ) = "fun(" <> intercalate ", " (show <$> n) <> ")"
  show Null            = "null"

showEntry :: (Show a1, Show a2) => (a1, a2) -> String
showEntry (k, v) = show k <> ": " <> show v

instance Exception Val

type Var = Lazy.Text

type Bindings = Map Var Val

newtype Scope = Scope {scope :: Lens' Scopes Bindings}

instance Show Scope where
  show _ = "scope"

data Scopes = Scopes
  { globalS :: Bindings
  , localS  :: Bindings
  }
  deriving Show

instance Eq Scopes where
  s1 == s2 = (globalS s1 == globalS s2) && (localS s1 == localS s2)

type Store = Either Int Scopes

emptyStore :: Scopes
emptyStore = Scopes { globalS = [], localS = [] }

argsStore :: [String] -> Scopes
argsStore args = Scopes { globalS = [("args", argsList)], localS = [] }
  where argsList = ListVal . fromList $ map StrVal args

globalL :: Scope
globalL = Scope $ lens globalS (\x y -> x { globalS = y })

localL :: Scope
localL = Scope $ lens localS (\x y -> x { localS = y })
