{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Version                   ( showVersion )
import           RIO
import           Utils.Interp                   ( runWithStore )
import           Utils.IO                       ( putStrLn )
import           Utils.Types
import           REPL.Console                   ( startREPL )
import           REPL.Eval                      ( eval )

run :: RIO App ()
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  store    <- view (to appOptions) >>= startStore
  case store of
    Left  _ -> return ()
    Right _ -> do
      putStrLn
        (  "We're inside the experimental PiG interpreter!\nversion: "
        <> fromString (showVersion version)
        <> "\ntype ':help' or ':h' for more information "
        )
      void $ runWithStore (startREPL settings) store
 where
  startStore ops = case optionsLoad ops of
    []   -> return $ Right emptyStore
    file -> runWithStore (eval . Load . Val . StrVal $ file) $ Right emptyStore
