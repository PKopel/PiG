{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Statements
  ( eval
  )
where

import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           RIO
import           Utils.Interp
import           Utils.Types
import           Utils.Types.App                ( Interp(..) )
import           Utils.IO                       ( putStr
                                                , readFile
                                                )
import           Utils.Util                     ( getElems
                                                , isVar
                                                )
import           Interp.BIF                     ( evalBIF
                                                , bifs
                                                )
import           Lang.Parser                    ( parseFile )

eval :: Expr -> Interp a Val
eval (Val  n) = return n
eval (Var  x) = getVar x
eval (Load e) = eval e >>= \case
  StrVal file -> withStore (evalFile file) >> return Null
  _           -> return Null
eval (Binary op e1 e2) = appBin op <$> eval e1 <*> eval e2
eval (Unary op e     ) = eval e <&> appUn op >>= \case
  ListVal (h :<| t :<| Empty) ->
    let (iv, x) = isVar e in when iv (void (putVar x t)) >> return h
  other -> return other
eval (ListLiteral es) = ListVal . Seq.fromList <$> mapM eval es
eval (FunApp n vs) | n `elem` bifs = mapM eval vs >>= evalBIF n
                   | otherwise     = getVar n >>= evalFunApp vs
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
  getVar x >>= \case
    ListVal l -> eval i >>= \case
      AlgVal ind -> putVar x . ListVal $ Seq.update (round ind) v l
      _          -> putVar x v
    _ -> putVar x v

evalWhile :: Expr -> Expr -> Seq Val -> Interp a (Seq Val)
evalWhile e s acc = eval e >>= \case
  AlgVal  v -> while $ v /= 0
  BoolVal v -> while v
  ListVal v -> while $ not (Seq.null v)
  _         -> return acc
 where
  while cond =
    if cond then eval s >>= evalWhile e s . (acc Seq.|>) else return acc

evalFunApp :: [Expr] -> Val -> Interp a Val
-- execute function with arguments from args
evalFunApp args (FunVal as body) = getStore >>= \case
  Right s -> do
    let oldLocals = view (scope localL) s
    newLocals <- Map.fromList <$> zipWithM (\v a -> eval v <&> (,) a) args as
    oldScope  <- getScope
    withScopes $ (over . scope) localL (const newLocals) -- introducing local variables
    setScope localL -- from now variables are declared in local scope
    retVal <- eval body
    setScope oldScope -- end of local scope
    withScopes $ (over . scope) localL (const oldLocals) -- removing local variables
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

evalDoubleList :: [Expr] -> Interp a [Double]
evalDoubleList = foldM
  (\acc v -> eval v >>= \case
    AlgVal av -> return (av : acc)
    _         -> return acc
  )
  []


evalFile :: FilePath -> Store -> Interp a Store
evalFile file store = do
  contents <- parseFile file <$> readFile file
  case contents of
    Left  err  -> putStr err >> return store
    Right expr -> Interp . lift $ runWithStore (eval expr) store
