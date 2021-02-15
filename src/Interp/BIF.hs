
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.BIF where

import           Utils.Types                    ( Interp
                                                , Val(Null, StrVal, AlgVal)
                                                )
import           Utils.Interp                   ( printVal
                                                , putStore
                                                , readVal
                                                )
import           RIO


bifs :: [String]
bifs = ["read", "print", "exit", "strToNum", ":print"]


evalBIF :: String -> [Val] -> Interp Val
evalBIF "read"   _           = StrVal <$> readVal
evalBIF "print"  (e    : es) = printVal e >> evalBIF "print" es
evalBIF ":print" (Null : _ ) = return Null
evalBIF ":print" (e    : es) = printVal e >> evalBIF ":print" es
evalBIF "exit"   _           = putStore (Left ()) >> return Null
evalBIF "strToNum" ((StrVal str) : _) =
  return $ maybe Null AlgVal (readMaybe str)
evalBIF _ _ = return Null
