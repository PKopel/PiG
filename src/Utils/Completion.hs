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
import           System.Console.Haskeline       ( completeWord
                                                , simpleCompletion
                                                , Completion
                                                , CompletionFunc
                                                )
import           Utils.Types                    ( Interp
                                                , Store
                                                , Scopes(globalS, localS)
                                                )
import           Utils.Interp                   ( getStore )
import           Interp.BIF


searchFunc :: Store -> String -> [Completion]
searchFunc (Right scopes) str = map simpleCompletion $ filter
  ((&&) <$> (str `isPrefixOf`) <*> (':' `notElem`))
  (M.keys (globalS scopes) ++ M.keys (localS scopes) ++ bifs)
searchFunc _ _ = []

completion :: CompletionFunc Interp
completion = completeWord Nothing " \t" $ \str -> do
  store <- getStore
  return $ searchFunc store str
