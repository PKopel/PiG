{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Lang.BIF.List where

import           RIO
import           Utils.Types                    ( Interp
                                                , Val(..)
                                                )

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

listToStr :: [Val] -> Interp a Val
listToStr ((ListVal l) : _) = return . StrVal $ foldr toStr [] l
 where
  toStr (CharVal c) s = c : s
  toStr _           s = s
listToStr _ = return Null
