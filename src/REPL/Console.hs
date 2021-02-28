{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Console
  ( startREPL
  )
where

import           RIO                     hiding ( Text )
import qualified Data.Text.Lazy                as Lazy
import           Utils.IO                       ( putStrLn )
import           Utils.Types                    ( Val(..)
                                                , Expr(..)
                                                , Interp
                                                )
import           Utils.Interp                   ( getStore
                                                , interpWithStore
                                                )
import           REPL.Eval                      ( eval )
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
                                                , runInputT
                                                )

startREPL :: Settings (Interp a) -> Interp a ()
startREPL settings = runInputT settings $ runLine Green

runLine :: Color -> InputT (Interp a) ()
runLine colour = lift getStore >>= \case
  Left _ -> return ()
  _      -> do
    line <- getInputLine $ (style Faint . color colour) "PiG" <> "> "
    checkLine $ Lazy.strip . fromString <$> line

checkLine :: Maybe Lazy.Text -> InputT (Interp a) ()
checkLine (Just line)
  | Lazy.null line = runLine Green
  | otherwise = if isDirective line
    then lift (interpWithStore (execute line)) >> runLine Green
    else case parseProg line of
      Left  err  -> putStrLn err >> runLine Red
      Right prog -> runProg prog
checkLine _ = return ()

runProg :: Expr -> InputT (Interp a) ()
runProg expr = lift (interpWithStore (eval expr')) >> runLine Green
 where
  expr'  = Seq [assign, If [(cond, print)] (Val Null)]
  assign = Assign "$$" (Val Null) expr
  cond   = FunApp "neq" [Var "$$", Val Null]
  print  = FunApp "print" [Var "$$", Val (StrVal "\n")]


