{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Interp.Console
  ( startREPL
  )
where

import           Import
import           Interp.Statements              ( eval )
import           Interp.Directives              ( isDirective
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

startREPL :: Settings Interp -> Interp ()
startREPL settings = runInputT settings $ runLine Green

runLine :: Color -> InputT Interp ()
runLine colour = lift getStore >>= \case
  Left _ -> return ()
  _      -> do
    line <- getInputLine $ (style Faint . color colour) "PiG" <> "> "
    checkLine line

checkLine :: Maybe String -> InputT Interp ()
checkLine (Just line) = if isDirective line
  then lift (runWithStore (execute line)) >> runLine Green
  else case parseProg line of
    Left  err  -> outputStrLn err >> runLine Red
    Right prog -> runProg prog
checkLine _ = return ()

runProg :: Expr -> InputT Interp ()
runProg expr =
  let expr' = case expr of
    -- kind of a hack, but works
        e@(Seq _) -> FunApp ":print" [e, Val (StrVal "\n")]
        other     -> other
  in  lift (runWithStore (eval expr')) >> runLine Green
