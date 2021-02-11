{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Version                   ( showVersion )
import           Import
import           REPL.Console                   ( startREPL )
import           REPL.Directives                ( exec )
import           System.Console.Haskeline       ( runInputT )

run :: RIO App ()
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  options  <- view $ to appOptions
  store    <- liftIO $ runInputT settings $ startStore options
  case store of
    Left  _ -> return ()
    Right _ -> do
      logInfo
        (  "We're inside the experimental PiG interpreter!\nversion: "
        <> fromString (showVersion version)
        <> "\ntype ':help' or ':h' for more information "
        )
      liftIO $ startREPL settings store
 where
  startStore ops = case optionsLoad ops of
    []   -> return $ Right emptyStore
    file -> runWithStore (exec (Load file)) $ Right emptyStore
