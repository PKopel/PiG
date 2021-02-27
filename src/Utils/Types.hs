{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Utils.Types where
import           Data.List                      ( intercalate )
import qualified Data.Text.Lazy                as Lazy
import           RIO

data Expr
  = Var Var
  | Val Val
  | Assign Var Expr Expr
  | ListLiteral [Expr]
  | FunApp Var [Expr]
  | Load Expr
  | While Expr Expr
  | If [(Expr, Expr)] Expr
  | Seq [Expr]
  deriving(Eq, Ord)

instance Show Expr where
  show (Var v       ) = Lazy.unpack v
  show (Val v       ) = show v
  show (Assign a b c) = unwords [Lazy.unpack a, show b, " = ", show c]
  show (Load e      ) = "load " <> show e
  show (While a b   ) = unwords ["while", show a, show b]
  show (Seq v       ) = '{' : intercalate "; " (show <$> v) <> "}"
  show (If a b) =
    unwords ["if", intercalate " elif " (show <$> a), "else", show b]
  show (ListLiteral v) = '[' : intercalate ", " (show <$> v) <> "]"
  show (FunApp a b) =
    Lazy.unpack a <> "(" <> intercalate ", " (show <$> b) <> ")"

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
  show (ListVal v    ) = '[' : intercalate ", " (toList $ show <$> v) <> "]"
  show (FunVal n _   ) = "fun(" <> intercalate ", " (show <$> n) <> ")"
  show Null            = "null"

type Var = Lazy.Text

type Bindings = Map Var Val

newtype Scope = Scope {scope :: Lens' Scopes Bindings}

instance Show Scope where
  show _ = "scope"

data Scopes = Scopes {globalS :: Bindings, localS :: Bindings} deriving (Show)

instance Eq Scopes where
  s1 == s2 = (globalS s1 == globalS s2) && (localS s1 == localS s2)

type Store = Either () Scopes

emptyStore :: Scopes
emptyStore = Scopes { globalS = [], localS = [] }

globalL :: Scope
globalL = Scope $ lens globalS (\x y -> x { globalS = y })

localL :: Scope
localL = Scope $ lens localS (\x y -> x { localS = y })
