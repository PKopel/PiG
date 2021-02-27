{-# LANGUAGE NoImplicitPrelude #-}

module Lang.BIF.Alg where

import           RIO
import           Utils.Types                    ( Val(..) )

add, sub, mul, div', pow :: [Val] -> Val
add = algBinOp (+)
sub = algBinOp (-)
mul = algBinOp (*)
div' = algBinOp (/)
pow = algBinOp (**)

algBinOp :: (Double -> Double -> Double) -> [Val] -> Val
algBinOp _ [val@(AlgVal _)] = val
algBinOp op (AlgVal a : AlgVal b : vals) = add (AlgVal (op a b) : vals)
algBinOp _  _ = Null

modulo :: [Val] -> Val
modulo [AlgVal a, AlgVal b] | isInt a && isInt b = AlgVal . fromInteger $ mod
  (round a)
  (round b)
  where isInt x = (100000 * (x - fromInteger (round x))) == 0.0
modulo _ = Null

neg :: [Val] -> Val
neg [AlgVal a] = AlgVal $ negate a
neg _          = Null
