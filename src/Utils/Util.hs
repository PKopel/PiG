{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Util
  ( printVal
  , printString
  , isVar
  , readVar
  , writeGlobVar
  , writeLocVar
  , valToList
  , runWithStore
  , maybeToAlgVal
  , maybeToBoolVal
  , maybeToListVal
  , algValToMaybe
  , boolValToMaybe
  , listValToMaybe
  , getElems
  , getStore
  , getWriteFun
  , putStore
  , putWriteFun
  , withStore
  , (>-)
  , (-<)
  , module Utils.Types
  )
where

import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           RIO
import           System.Console.Haskeline
import           Utils.Types

getStore :: Interp Store
getStore = get >>= return . snd

getWriteFun :: Interp WriteFun
getWriteFun = get >>= return . fst

putStore :: Store -> Interp ()
putStore s' = do
  (w, _) <- get
  put (w, s')

putWriteFun :: WriteFun -> Interp ()
putWriteFun w' = do
  (_, s) <- get
  put (w', s)

withStore :: (Store -> Store) -> Interp ()
withStore f = getStore >>= putStore . f

runInterpWithStore
  :: Interp a -> (WriteFun, Store) -> InputT IO (a, (WriteFun, Store))
runInterpWithStore = runStateT . runInterp

runWithStore :: Interp a -> Store -> InputT IO (a, Store)
runWithStore interp store =
  runInterpWithStore interp (writeGlobVar, store)
    >>= return
    .   ((,) <$> fst <*> (snd . snd))

valToList :: Val -> Seq Val
valToList (ListVal vs) = vs
valToList v            = Seq.singleton v

maybeToAlgVal :: Maybe Double -> Val
maybeToAlgVal Nothing  = Null
maybeToAlgVal (Just v) = AlgVal v

maybeToBoolVal :: Maybe Bool -> Val
maybeToBoolVal Nothing  = Null
maybeToBoolVal (Just v) = BoolVal v

maybeToListVal :: Maybe (Seq Val) -> Val
maybeToListVal Nothing  = Null
maybeToListVal (Just v) = ListVal v

algValToMaybe :: Val -> Maybe Double
algValToMaybe (AlgVal n) = Just n
algValToMaybe _          = Nothing

boolValToMaybe :: Val -> Maybe Bool
boolValToMaybe (BoolVal b) = Just b
boolValToMaybe (AlgVal  a) = Just (a /= 0)
boolValToMaybe (ListVal l) = Just (length l > 0)
boolValToMaybe _           = Nothing

listValToMaybe :: Val -> Maybe (Seq Val)
listValToMaybe (ListVal n) = Just n
listValToMaybe _           = Nothing

getElems :: (Foldable t) => (Seq Val) -> t (Maybe Double) -> (Seq Val)
getElems list = foldl'
  (\acc v ->
    case (Seq.<|) <$> (join $ (list Seq.!?) . round <$> v) <*> pure acc of
      Nothing   -> acc
      Just acc' -> acc'
  )
  Seq.empty

isVar :: Expr -> (Bool, Var)
isVar (Var x) = (True, x)
isVar _       = (False, "")

readVar :: Var -> Interp Val
readVar x = do
  store <- getStore
  case Map.lookup x (getLocals store) of
    Just v  -> return v
    Nothing -> case Map.lookup x (getGlobals store) of
      Just v  -> return v
      Nothing -> return Null

writeGlobVar :: WriteFun
writeGlobVar x v = do
  store <- getStore
  putStore $ setGlobals (Map.insert x v) store

writeLocVar :: WriteFun
writeLocVar x v = do
  store <- getStore
  putStore $ setLocals (Map.insert x v) store

printVal :: Show a => a -> Interp ()
printVal = Interp . lift . outputStr . show

printString :: String -> Interp ()
printString = Interp . lift . outputStr

(>-) :: Seq Val -> (Val, Seq Val)
(>-) (h :<| t) = (h, t)
(>-) l         = (Null, l)

(-<) :: Seq Val -> (Val, Seq Val)
(-<) (i :|> l) = (l, i)
(-<) l         = (Null, l)
