{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Completion
  ( completion
  ) where

import           Data.List                      ( isPrefixOf )
import qualified Data.Text.Lazy                as Lazy
import           Lang.BIF                       ( bifs )
import           RIO
import qualified RIO.Map                       as Map
import           System.Console.Haskeline       ( Completion
                                                , CompletionFunc
                                                , completeWord
                                                , simpleCompletion
                                                )
import           Utils.Interp                   ( getStore )
import           Utils.Types                    ( Interp
                                                , Scopes(globalS, localS)
                                                , Store
                                                )


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
