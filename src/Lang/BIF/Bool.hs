{-# LANGUAGE NoImplicitPrelude #-}

module Lang.BIF.Bool where

import           RIO
import           Utils.Types                    ( Val(..) )

and', or' :: [Val] -> Val
and' = boolBinOp (&&)
or' = boolBinOp (||)

boolBinOp :: (Bool -> Bool -> Bool) -> [Val] -> Val
boolBinOp _  [val@( BoolVal _)   ] = val
boolBinOp op (a : b :        vals) = boolBinOp op (BoolVal (op ba bb) : vals)
 where
  ba = toBool a
  bb = toBool b
boolBinOp _ _ = Null

not' :: [Val] -> Val
not' [val] = BoolVal . not $ toBool val
not' _     = Null

toBool :: Val -> Bool
toBool (BoolVal v) = v
toBool (AlgVal  v) = v /= 0
toBool (ListVal v) = not (null v)
toBool (StrVal  v) = not (null v)
toBool Null        = False
toBool _           = True

lt, gt, eq, neq :: [Val] -> Val
lt = valBinOp (<)
gt = valBinOp (>)
eq = valBinOp (==)
neq = valBinOp (/=)

valBinOp :: (Val -> Val -> Bool) -> [Val] -> Val
valBinOp op [a, b] = BoolVal $ op a b
valBinOp _  _      = Null
