{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
module Lang.BIF.List where

import           RIO
import           Data.Sequence                  ( Seq(..) )
import           Utils.Types                    ( Val(..) )

catl :: [Val] -> Val
catl = seqBinOp (<>)

cats :: [Val] -> Val
cats = strBinOp (<>)

strBinOp :: (String -> String -> String) -> [Val] -> Val
strBinOp _  [val@( StrVal _)   ] = val
strBinOp op (a : b :       vals) = strBinOp
  op
  (StrVal (op (toStr a) (toStr b)) : vals)
 where
  toStr (FunVal _ _) = ""
  toStr v            = show v
strBinOp _ _ = Null

seqBinOp :: (Seq Val -> Seq Val -> Seq Val) -> [Val] -> Val
seqBinOp _  [val@( ListVal _)   ] = val
seqBinOp op (a : b :        vals) = seqBinOp
  op
  (ListVal (op (toSeq a) (toSeq b)) : vals)
 where
  toSeq (ListVal v) = v
  toSeq v           = [v]
seqBinOp _ _ = Null

fst', lst' :: [Val] -> Val
fst' = unOp (>-)
lst' = unOp (-<)

(>-) :: Seq Val -> (Val, Seq Val)
(>-) (h :<| t) = (h, t)
(>-) l         = (Null, l)

(-<) :: Seq Val -> (Val, Seq Val)
(-<) (i :|> l) = (l, i)
(-<) l         = (Null, l)

unOp :: (Seq Val -> (Val, Seq Val)) -> [Val] -> Val
unOp op [ListVal a] = let (v, l) = op a in ListVal [v, ListVal l]
unOp _  _           = Null
