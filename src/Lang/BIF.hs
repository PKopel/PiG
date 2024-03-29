{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lang.BIF
  ( bifs
  ) where

import qualified Data.Text.Lazy                as Lazy
import           Lang.BIF.Alg
import           Lang.BIF.Bool
import           Lang.BIF.List
import           Lang.BIF.Str
import           RIO                     hiding ( length
                                                , openFile
                                                )
import           System.IO                      ( getLine
                                                , hGetLine
                                                , hPutStr
                                                , openFile
                                                , putStr
                                                )
import           Utils.Interp                   ( putStore )
import           Utils.Types                    ( Interp
                                                , Val(..)
                                                )


bifs :: Map Lazy.Text ([Val] -> Interp a Val)
bifs =
  [ ("read"     , read)
  , ("print"    , print)
  , ("open"     , open)
  , ("close"    , close)
  , ("readFile" , readFile)
  , ("writeFile", writeFile)
  , ("exit"     , exit)
  , ("strToNum" , strToNum)
  , ("strToList", strToList)
  , ("listToStr", listToStr)
  , ("isNum"    , isNum)
  , ("isStr"    , isStr)
  , ("isBool"   , isBool)
  , ("isList"   , isList)
  , ("isFun"    , isFun)
  , ("add"      , return . add)
  , ("sub"      , return . sub)
  , ("mul"      , return . mul)
  , ("div"      , return . div')
  , ("mod"      , return . modulo)
  , ("pow"      , return . pow)
  , ("neg"      , return . neg)
  , ("not"      , return . not')
  , ("and"      , return . and')
  , ("or"       , return . or')
  , ("lt"       , return . lt)
  , ("gt"       , return . gt)
  , ("eq"       , return . eq)
  , ("neq"      , return . neq)
  , ("catList"  , return . catl)
  , ("catStr"   , return . cats)
  , ("length"   , return . length)
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
print (e : es) = liftIO (putStr $ show e) >> print es
print _        = return Null

open, close :: [Val] -> Interp a Val
open [StrVal p, StrVal m] = IOVal <$> liftIO (openFile p mode)
 where
  mode = case take 2 m of
    "rw"      -> ReadWriteMode
    ('r' : _) -> ReadMode
    ('w' : _) -> WriteMode
    _         -> AppendMode
open [StrVal p] = IOVal <$> liftIO (openFile p ReadWriteMode)
open _          = return Null
close [IOVal h] = liftIO (hClose h) $> Null
close _         = return Null

readFile, writeFile :: [Val] -> Interp a Val
readFile [IOVal h] =
  StrVal <$> liftIO (hIsEOF h >>= \eof -> if eof then return "" else hGetLine h)
readFile x = read x
writeFile (IOVal h : e : es) =
  liftIO (hPutStr h $ show e) >> writeFile (IOVal h : es)
writeFile [IOVal _] = return Null
writeFile x         = print x

exit :: [Val] -> Interp a Val
exit []                 = putStore (Left 0) >> return Null
exit ((AlgVal val) : _) = putStore (Left . round $ val) >> return Null
exit (_            : _) = putStore (Left 1) >> return Null
