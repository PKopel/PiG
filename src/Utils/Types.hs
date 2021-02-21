{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Utils.Types where
import           Data.List                      ( intercalate )
import qualified Data.Text.Lazy                as Lazy
import           RIO

class UnaryOp op where
  appUn :: op -> Val -> Val

instance UnaryOp (Double -> Double) where
  appUn op (AlgVal a) = AlgVal $ op a
  appUn _  _          = Null

instance UnaryOp (Bool -> Bool) where
  appUn op (BoolVal a) = BoolVal $ op a
  appUn _  _           = Null

instance UnaryOp (Seq Val -> (Val, Seq Val)) where
  appUn op (ListVal a) = let (v, l) = op a in ListVal [v, ListVal l]
  appUn _  _           = Null

class BinaryOp op where
  appBin :: op -> Val -> Val -> Val

instance BinaryOp (Val -> Val -> Bool) where
  appBin op a b = BoolVal $ op a b

instance BinaryOp (Bool -> Bool -> Bool) where
  appBin op a b = BoolVal $ op (toBool a) (toBool b)
   where
    toBool (BoolVal v) = v
    toBool (AlgVal  v) = v /= 0
    toBool (ListVal v) = not (null v)
    toBool (StrVal  v) = not (null v)
    toBool Null        = False
    toBool _           = True

instance BinaryOp (Double -> Double -> Double) where
  appBin op (AlgVal a) (AlgVal b) = AlgVal $ op a b
  appBin _  _          _          = Null

instance BinaryOp (Integer -> Integer -> Integer) where
  appBin op (AlgVal a) (AlgVal b) | isInt a && isInt b =
    AlgVal $ fromIntegral $ op (round a) (round b)
    where isInt x = (100000 * (x - fromInteger (round x))) == 0.0
  appBin _ _ _ = Null

instance BinaryOp (String -> String -> String) where
  appBin op a b = StrVal $ op (toStr a) (toStr b)
   where
    toStr (FunVal _ _) = ""
    toStr v            = show v

instance BinaryOp (Seq Val -> Seq Val -> Seq Val) where
  appBin op a b = ListVal $ op (toSeq a) (toSeq b)
   where
    toSeq (ListVal v) = v
    toSeq v           = [v]

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
  | forall op. (BinaryOp op) => Binary op Expr Expr
  | forall op. (UnaryOp op) => Unary op Expr

instance Eq Expr where
  (Assign a b c ) == (Assign d e f ) = a == d && b == e && c == f
  (If     a b   ) == (If     d e   ) = a == d && b == e
  (FunApp a b   ) == (FunApp d e   ) = a == d && b == e
  (While  a b   ) == (While  d e   ) = a == d && b == e
  (Binary _ a b ) == (Binary _ d e ) = a == d && b == e
  (Unary _ a    ) == (Unary _ d    ) = a == d
  (Var         a) == (Var         d) = a == d
  (Val         a) == (Val         d) = a == d
  (Seq         a) == (Seq         d) = a == d
  (ListLiteral a) == (ListLiteral d) = a == d
  _               == _               = False

instance Ord Expr where
  (Var v1) <= (Var v2) = v1 <= v2
  (Val v1) <= (Val v2) = v1 <= v2
  _        <= _        = False

instance Show Expr where
  show (Var v       ) = Lazy.unpack v
  show (Val v       ) = show v
  show (Assign a b c) = unwords [Lazy.unpack a, show b, " = ", show c]
  show (Load e      ) = "load " <> show e
  show (While a b   ) = unwords ["while", show a, show b]
  show (Seq v       ) = '{' : intercalate "; " (show <$> v) <> "}"
  show (Binary _ a b) = unwords ["binary op", show a, show b]
  show (Unary _ a   ) = "unary op " <> show a
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
