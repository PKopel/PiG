{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.BIF
  ( bifs
  , evalBIF
  )
where

import           Utils.Types.App                ( Interp )
import           Utils.Types                    ( Val(Null, StrVal, AlgVal) )
import           Utils.Interp                   ( putStore )
import           RIO
import qualified Data.Text.Lazy                as Lazy
import           System.IO                      ( getLine
                                                , putStr
                                                )


bifs :: [Lazy.Text]
bifs = ["read", "print", "exit", "strToNum", ":print"]


evalBIF :: Lazy.Text -> [Val] -> Interp a Val
evalBIF "read"  _        = StrVal <$> liftIO getLine
evalBIF "print" (e : es) = liftIO (putStr (show e)) >> evalBIF "print" es
evalBIF "exit"  _        = putStore (Left ()) >> return Null
evalBIF "strToNum" ((StrVal str) : _) =
  return $ maybe Null AlgVal (readMaybe str)
evalBIF _ _ = return Null
