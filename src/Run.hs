{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Import
import qualified Interp.Directives             as D
import qualified Interp.Statements             as S
import           Lang.Parser
import           System.Console.Haskeline

run :: RIO App ()
run = do
  logInfo
    "We're inside the experimental PiG interpreter!\n\
    \type ':help' for more information "
  settings <- view $ to appSettings
  liftIO $ runInputT settings $ runLine emptyStore

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
