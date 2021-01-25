{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Version                   ( showVersion )
import           Import
import           Interp.Directives              ( exec )
import           Interp.Statements              ( eval )
import           Lang.Parser                    ( parseProg )
import           System.Console.Haskeline       ( InputT
                                                , getInputLine
                                                , outputStrLn
                                                , runInputT
                                                )
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                )

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
        e@(Seq _) -> Print [e]
        other     -> other
  in  runWithStore (eval p') store >>= runLine Green
runProg _     (Drct Exit) = return ()
runProg store (Drct p   ) = runWithStore (exec p) store >>= runLine Green
