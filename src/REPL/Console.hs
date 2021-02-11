{-# LANGUAGE NoImplicitPrelude #-}
module REPL.Console where

import           Import
import           Interp.Statements              ( eval )
import           REPL.Directives                ( isDirective
                                                , execute
                                                )
import           Lang.Parser                    ( parseProg )
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                )
import           System.Console.Haskeline       ( InputT
                                                , Settings
                                                , getInputLine
                                                , outputStrLn
                                                , runInputT
                                                )

startREPL :: Settings IO -> Either () Scopes -> IO ()
startREPL settings store = runInputT settings $ runLine Green store

runLine :: Color -> Store -> InputT IO ()
runLine _      (Left _) = return ()
runLine colour store    = do
  line <- getInputLine $ (style Faint . color colour) "PiG" <> "> "
  checkLine line
 where
  checkLine Nothing     = return ()
  checkLine (Just line) = if isDirective line
    then runWithStore (execute line) store >>= runLine Green
    else case parseProg line of
      Left  err  -> outputStrLn err >> runLine Red store
      Right prog -> runProg store prog

runProg :: Store -> Expr -> InputT IO ()
runProg store expr =
  let expr' = case expr of
        e@(Seq _) -> FunApp "print" [e]
        other     -> other
  in  runWithStore (eval expr') store >>= runLine Green
