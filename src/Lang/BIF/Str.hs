{-# LANGUAGE NoImplicitPrelude #-}

module Lang.BIF.Str where


import           RIO
import qualified RIO.Seq                       as Seq
import           Utils.Types                    ( Interp
                                                , Val(..)
                                                )


strToNum, strToList :: [Val] -> Interp a Val
strToNum ((StrVal str) : _) = return $ maybe Null AlgVal (readMaybe str)
strToNum _                  = return Null
strToList ((StrVal str) : _) =
  return . ListVal . Seq.fromList . map CharVal $ str
strToList _ = return Null
