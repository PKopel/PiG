{-# LANGUAGE NoImplicitPrelude #-}

module Lang.BIF.Str where


import           RIO
import qualified RIO.Seq                       as Seq
import           Utils.Types                    ( Interp
                                                , Val(..)
                                                )

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


strToNum, strToList :: [Val] -> Interp a Val
strToNum ((StrVal str) : _) = return $ maybe Null AlgVal (readMaybe str)
strToNum _                  = return Null
strToList ((StrVal str) : _) =
  return . ListVal . Seq.fromList . map CharVal $ str
strToList _ = return Null
