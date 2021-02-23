{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.BIF
  ( bifs
  , evalBIF
  )
where

import           Utils.Types.App                ( Interp )
import           Utils.Types                    ( Val(..) )
import           Utils.Interp                   ( putStore )
import           RIO
import qualified Data.Text.Lazy                as Lazy
import           System.IO                      ( getLine
                                                , putStr
                                                )


bifs :: [Lazy.Text]
bifs =
  [ "read"
  , "print"
  , "exit"
  , "strToNum"
  , "isNum"
  , "isStr"
  , "isBool"
  , "isList"
  , "isFun"
  ]


evalBIF :: Lazy.Text -> [Val] -> Interp a Val
evalBIF "isNum"  [AlgVal _]   = return $ BoolVal True
evalBIF "isNum"  _            = return $ BoolVal False
evalBIF "isStr"  [StrVal _]   = return $ BoolVal True
evalBIF "isStr"  _            = return $ BoolVal False
evalBIF "isBool" [BoolVal _]  = return $ BoolVal True
evalBIF "isBool" _            = return $ BoolVal False
evalBIF "isList" [ListVal _]  = return $ BoolVal True
evalBIF "isList" _            = return $ BoolVal False
evalBIF "isFun"  [FunVal _ _] = return $ BoolVal True
evalBIF "isFun"  _            = return $ BoolVal False
evalBIF "read"   _            = StrVal <$> liftIO getLine
evalBIF "print"  (e : es)     = liftIO (putStr (show e)) >> evalBIF "print" es
evalBIF "exit"   _            = putStore (Left ()) >> return Null
evalBIF "strToNum" ((StrVal str) : _) =
  return $ maybe Null AlgVal (readMaybe str)
evalBIF _ _ = return Null
