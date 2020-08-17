{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Version
import           Import
import qualified Interp.Directives             as D
import qualified Interp.Statements             as S
import           Lang.Parser
import           System.Console.Haskeline

run :: RIO App ()
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  options  <- view $ to appOptions
  logInfo
    (  "We're inside the experimental PiG interpreter!\nversion: "
    <> fromString (showVersion version)
    <> "\ntype ':help' for more information "
    )
  liftIO $ runInputT settings $ store options >>= runLine
 where
  store ops = case optionsLoad ops of
    []   -> return emptyStore
    file -> runWithStore (D.exec (Load file)) emptyStore >>= return . snd

runLine :: Store -> InputT IO ()
runLine store = do
  line <- getInputLine "PiG> "
  case parseProg <$> line of
    Nothing           -> return ()
    Just (Left  err ) -> outputStrLn err >> runLine store
    Just (Right prog) -> runProg store prog

runProg :: Store -> Prog -> InputT IO ()
runProg store (Stmt p   ) = runWithStore (S.exec p) store >>= runLine . snd
runProg _     (Drct Exit) = return ()
runProg store (Drct p   ) = runWithStore (D.exec p) store >>= runLine . snd
