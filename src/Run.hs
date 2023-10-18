{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Run
  ( run
  ) where

import           Data.Version                   ( showVersion )
import           REPL.Console                   ( startREPL )
import           REPL.Eval                      ( evalWithCach )
import           RIO
import           Utils.IO                       ( putStrLn )
import           Utils.Interp                   ( runWithStore )
import           Utils.Types

run :: RIO App Int
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  store    <- view (to appOptions) >>= startStore
  case store of
    Left  val -> return val
    Right _   -> do
      putStrLn
        (  "We're inside the experimental PiG interpreter!\nversion: "
        <> fromString (showVersion version)
        <> "\ntype ':help' or ':h' for more information "
        )
      runWithStore (startREPL settings) store >>= \case
        Left  val -> return val
        Right _   -> return 0
 where
  startStore ops = case optionsLoad ops of
    []   -> return $ Right emptyStore
    file -> runWithStore (evalWithCach . Load . Val . StrVal $ file)
      $ Right emptyStore
