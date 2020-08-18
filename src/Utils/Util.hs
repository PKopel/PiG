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
  , replaceAtIndex
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

valToList :: Val -> [Val]
valToList (ListVal vs) = vs
valToList v            = [v]

maybeToAlgVal :: Maybe Double -> Val
maybeToAlgVal Nothing  = Null
maybeToAlgVal (Just v) = AlgVal v

maybeToBoolVal :: Maybe Bool -> Val
maybeToBoolVal Nothing  = Null
maybeToBoolVal (Just v) = BoolVal v

maybeToListVal :: Maybe [Val] -> Val
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

listValToMaybe :: Val -> Maybe [Val]
listValToMaybe (ListVal n) = Just n
listValToMaybe _           = Nothing

getElems :: (Foldable t, RealFrac a) => [Val] -> t (Maybe a) -> [Val]
getElems list = foldl'
  (\acc v -> case v of
    Nothing -> acc
    Just d ->
      let i = round d
      in  if length list > i then (list !! i) : acc else Null : acc
  )
  []

replaceAtIndex :: a -> [a] -> Int -> [a]
replaceAtIndex _ [] _ = []
replaceAtIndex item ls n | n < length ls = a ++ (item : tail b)
                         | otherwise     = ls
  where (a, b) = splitAt n ls

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

(>-) :: [Val] -> (Val, [Val])
(>-) (h : t) = (h, t)
(>-) l       = (Null, l)

(-<) :: [Val] -> (Val, [Val])
(-<) l@(_ : _) = (last l, init l)
(-<) l         = (Null, l)
