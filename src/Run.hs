{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Version
import           Import
import           Interp.Directives
import           Interp.Statements
import           Lang.Parser
import           System.Console.Haskeline
import           System.Console.Pretty

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
        <> "\ntype ':help' for more information "
        )
      liftIO $ runInputT settings $ runLine Green store
 where
  startStore ops = case optionsLoad ops of
    []   -> return $ Right emptyStore
    file -> runWithStore (exec (Load file)) $ Right emptyStore

runLine :: Color -> Store -> InputT IO ()
runLine colour store = do
  line <- getInputLine $ (style Faint . color colour) "PiG" <> "> "
  case parseProg <$> line of
    Nothing           -> return ()
    Just (Left  err ) -> outputStrLn err >> runLine Red store
    Just (Right prog) -> runProg store prog

runProg :: Store -> Prog -> InputT IO ()
runProg store (Stmt p) =
  let p' = case p of
        Seq [e] -> Print [e]
        other   -> other
  in  runWithStore (eval p') store >>= runLine Green
runProg _     (Drct Exit) = return ()
runProg store (Drct p   ) = runWithStore (exec p) store >>= runLine Green
