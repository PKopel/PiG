{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Statements
  ( eval
  , evalFile
  )
where

import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Import
import           Lang.Parser                    ( parseFile )
import           System.IO.Error                ( tryIOError )
import qualified Data.Text                     as T
import           System.IO                      ( print
                                                , putStrLn
                                                )

eval :: Expr -> Interp Val
eval (Val n          ) = return n
eval (Var x          ) = readVar x
eval (Binary op e1 e2) = appBin op <$> eval e1 <*> eval e2
eval (Unary op e     ) = eval e <&> appUn op >>= \case
  ListVal (h :<| t :<| Empty) ->
    let (iv, x) = isVar e in when iv (void (writeVar x t)) >> return h
  other -> return other
eval (ListLiteral es) = ListVal . Seq.fromList <$> mapM eval es
eval (FunApp n vs) | n `elem` bifs = mapM eval vs >>= evalBIF n
                   | otherwise     = readVar n >>= evalFunApp vs
eval (Seq []             ) = return Null
eval (Seq [s     ]       ) = eval s
eval (Seq (s : ss)       ) = eval s >> eval (Seq ss)
eval (If []            e2) = eval e2
eval (If ((c, e1) : t) e2) = eval c >>= \case
  AlgVal  v -> if v /= 0 then eval e1 else eval (If t e2)
  BoolVal v -> if v then eval e1 else eval (If t e2)
  ListVal v -> if not (Seq.null v) then eval e1 else eval (If t e2)
  _         -> return Null
eval (While e s   ) = ListVal <$> evalWhile e s Empty
eval (Assign x i e) = do
  v <- eval e
  readVar x >>= \case
    ListVal l -> eval i >>= \case
      AlgVal ind -> writeVar x . ListVal $ Seq.update (round ind) v l
      _          -> writeVar x v
    _ -> writeVar x v

evalWhile :: Expr -> Expr -> Seq Val -> Interp (Seq Val)
evalWhile e s acc = eval e >>= \case
  AlgVal  v -> while $ v /= 0
  BoolVal v -> while v
  ListVal v -> while $ not (Seq.null v)
  _         -> return acc
 where
  while cond =
    if cond then eval s >>= evalWhile e s . (acc Seq.|>) else return acc

evalFunApp :: [Expr] -> Val -> Interp Val
-- execute function with arguments from args
evalFunApp args (FunVal as body) = getStore >>= \case
  Right s -> do
    let oldLocals = view (scope localL) s
    newLocals <- Map.fromList <$> zipWithM (\v a -> eval v <&> (,) a) args as
    oldScope  <- getScope
    withStore $ (over . scope) localL (const newLocals) -- introducing local variables
    setScope localL -- from now variables are declared in local scope
    retVal <- eval body
    setScope oldScope -- end of local scope
    withStore $ (over . scope) localL (const oldLocals) -- removing local variables
    return retVal
  _ -> return Null
-- get elements from list
evalFunApp args (ListVal l) = do
  evs <- evalDoubleList args
  case getElems l evs of
    v  :<|    Empty -> return v -- one argument in vs, result is a single value
    v@(_ :<| _)     -> return $ ListVal v -- more than one argument in vs, result is a list
    _               -> return Null
-- get chars from string
evalFunApp args (StrVal l) = do
  evs <- evalDoubleList args
  case getElems l evs of
    [  v    ] -> return $ CharVal v -- one argument in vs, result is a single char
    v@(_ : _) -> return $ StrVal v -- more than one argument in vs, result is a string
    _         -> return Null
evalFunApp _ _ = return Null

evalDoubleList :: [Expr] -> Interp [Double]
evalDoubleList = foldM
  (\acc v -> eval v >>= \case
    AlgVal av -> return (av : acc)
    _         -> return acc
  )
  []

evalBIF :: String -> [Val] -> Interp Val
evalBIF "read"   _           = StrVal <$> readVal
evalBIF "print"  (e    : es) = printVal e >> evalBIF "print" es
evalBIF ":print" (Null : _ ) = return Null
evalBIF ":print" (e    : es) = printVal e >> evalBIF ":print" es
evalBIF "exit"   _           = putStore (Left ()) >> return Null
evalBIF "load" ((StrVal file) : t) =
  getStore >>= Interp . lift . evalFile file >>= putStore >> evalBIF "load" t
evalBIF "strToNum" ((StrVal str) : _) =
  return $ maybe Null AlgVal (readMaybe str)
evalBIF _ _ = return Null


evalFile :: FilePath -> Store -> IO Store
evalFile file store = do
  contents <- tryIOError (parseFile file . T.unpack <$> readFileUtf8 file)
  case contents of
    Left  err          -> print err >> return store
    Right (Left  err ) -> putStrLn err >> return store
    Right (Right expr) -> runWithStoreIO (eval expr) store
