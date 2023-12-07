{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Lang.BIF.List where

import           RIO
import qualified RIO.Map                       as Map
                                                ( size )
import qualified RIO.Seq                       as Seq
                                                ( length )
import           Utils.Types                    ( Interp
                                                , Val(..)
                                                )

catl :: [Val] -> Val
catl = seqBinOp (<>)

length :: [Val] -> Val
length ((ListVal l) : _) = AlgVal . fromIntegral $ Seq.length l
length ((StrVal  s) : _) = AlgVal . fromIntegral $ RIO.length s
length ((MapVal  m) : _) = AlgVal . fromIntegral $ Map.size m
length _                 = Null

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
