{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Completion
  ( completion
  )
where

import           RIO
import qualified Data.Map                      as M
import           Data.List                      ( isPrefixOf )
import           System.Console.Haskeline
import           Utils.Types
import           Utils.Interp
import           Utils.Util


searchFunc :: Store -> String -> [Completion]
searchFunc (Right scopes) str = map simpleCompletion $ filter
  ((&&) <$> (str `isPrefixOf`) <*> (':' `notElem`))
  (M.keys (globalS scopes) ++ M.keys (localS scopes) ++ bifs)
searchFunc _ _ = []

completion :: CompletionFunc Interp
completion = completeWord Nothing " \t" $ \str -> do
  store <- getStore
  return $ searchFunc store str
