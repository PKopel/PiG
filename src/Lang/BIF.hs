{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lang.BIF
  ( bifs
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
import           Lang.BIF.Alg
import           Lang.BIF.Bool
import           Lang.BIF.List


bifs :: Map Lazy.Text ([Val] -> Interp a Val)
bifs =
  [ ("read"    , read)
  , ("print"   , print)
  , ("exit"    , exit)
  , ("strToNum", strToNum)
  , ("isNum"   , isNum)
  , ("isStr"   , isStr)
  , ("isBool"  , isBool)
  , ("isList"  , isList)
  , ("isFun"   , isFun)
  , ("add"     , return . add)
  , ("sub"     , return . sub)
  , ("mul"     , return . mul)
  , ("div"     , return . div')
  , ("mod"     , return . modulo)
  , ("pow"     , return . pow)
  , ("neg"     , return . neg)
  , ("not"     , return . not')
  , ("and"     , return . and')
  , ("or"      , return . or')
  , ("lt"      , return . lt)
  , ("gt"      , return . gt)
  , ("eq"      , return . eq)
  , ("neq"     , return . neq)
  , ("catl"    , return . catl)
  , ("cats"    , return . cats)
  , ("fst"     , return . fst')
  , ("lst"     , return . lst')
  ]


isNum, isStr, isBool, isList, isFun :: [Val] -> Interp a Val
isNum [AlgVal _] = return $ BoolVal True
isNum _          = return $ BoolVal False
isStr [StrVal _] = return $ BoolVal True
isStr _          = return $ BoolVal False
isBool [BoolVal _] = return $ BoolVal True
isBool _           = return $ BoolVal False
isList [ListVal _] = return $ BoolVal True
isList _           = return $ BoolVal False
isFun [FunVal _ _] = return $ BoolVal True
isFun _            = return $ BoolVal False

read, print :: [Val] -> Interp a Val
read _ = StrVal <$> liftIO getLine
print (e : es) = liftIO (putStr (show e)) >> print es
print _        = return Null

exit :: [Val] -> Interp a Val
exit _ = putStore (Left ()) >> return Null

strToNum :: [Val] -> Interp a Val
strToNum ((StrVal str) : _) = return $ maybe Null AlgVal (readMaybe str)
strToNum _                  = return Null
