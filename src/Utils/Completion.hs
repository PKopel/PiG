{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Completion
  ( completion
  )
where

import           RIO
import qualified Data.Map                      as Map
import qualified Data.Text.Lazy                as Lazy
import           Data.List                      ( isPrefixOf )
import           System.Console.Haskeline       ( completeWord
                                                , simpleCompletion
                                                , Completion
                                                , CompletionFunc
                                                )
import           Utils.Types                    ( Store
                                                , Scopes(globalS, localS)
                                                , Interp
                                                )
import           Utils.Interp                   ( getStore )
import           Lang.BIF                       ( bifs )


searchFunc :: Store -> String -> [Completion]
searchFunc (Right scopes) str =
  map simpleCompletion
    $  filter (str `isPrefixOf`)
    .  ("load" :)
    .  map ((++ "(") . Lazy.unpack)
    $  Map.keys (globalS scopes)
    ++ Map.keys (localS scopes)
    ++ Map.keys bifs
searchFunc _ _ = []

completion :: CompletionFunc (Interp a)
completion = completeWord Nothing "\t" $ \str -> do
  store <- getStore
  return $ searchFunc store str
