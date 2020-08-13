{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Map                      as Map
import           Import
import           Interp.Exec
import           Lang.Parser
import           System.Console.Haskeline

run :: RIO App ()
run = do
  logInfo "We're inside the experimental PiG interpreter!\nhit Ctrl+D to exit"
  settings <- view $ to appSettings
  liftIO $ runInputT settings $ runLine Map.empty

runLine :: Store -> InputT IO ()
runLine store = do
  line <- getInputLine "PiG> "
  case parseProg <$> line of
    Nothing           -> return ()
    Just (Left  err ) -> outputStrLn err >> runLine store
    Just (Right prog) -> runProg store prog >>= runLine . snd
