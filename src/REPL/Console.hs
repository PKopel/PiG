{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Console
  ( startREPL
  ) where

import qualified Data.Text.Lazy                as L
import           Lang.Parser                    ( parseProg )
import           Prettyprinter                  ( annotate
                                                , defaultLayoutOptions
                                                , layoutPretty
                                                )
import           Prettyprinter.Render.Terminal  ( Color(Green, Red)
                                                , colorDull
                                                , renderLazy
                                                )
import           REPL.Directives                ( execute
                                                , isDirective
                                                )
import           REPL.Eval                      ( eval )
import           RIO                     hiding ( Text )
import           System.Console.Haskeline       ( InputT
                                                , Settings
                                                , getInputLine
                                                , runInputT
                                                )
import           Utils.IO                       ( putStrLn )
import           Utils.Interp                   ( getStore
                                                , interpWithStore
                                                )
import           Utils.Types                    ( Expr(..)
                                                , Interp
                                                , Val(..)
                                                )

type REPL a = InputT (Interp a) ()

startREPL :: Settings (Interp a) -> Interp a ()
startREPL settings = runInputT settings $ runLine Green

runLine :: Color -> REPL a
runLine colour = lift getStore >>= \case
  Left _ -> return ()
  _right -> do
    let promptAnsi = annotate (colorDull colour) "PiG" <> "> "
        promptSDoc = layoutPretty defaultLayoutOptions promptAnsi
    line <- getInputLine . L.unpack . renderLazy $ promptSDoc
    checkLine $ L.strip . fromString <$> line

checkLine :: Maybe L.Text -> REPL a
checkLine (Just line)
  | L.null line = runLine Green
  | otherwise = if isDirective line
    then lift (interpWithStore (execute line)) >> runLine Green
    else case parseProg line of
      Left  err  -> putStrLn err >> runLine Red
      Right prog -> runProg prog
checkLine _ = return ()

runProg :: Expr -> REPL a
runProg expr = lift (interpWithStore (eval expr')) >> runLine Green
 where
  expr'  = Seq [assign, If [(cond, print)] (Val Null)]
  assign = Assign "$$" (Val Null) expr
  cond   = FunApp "neq" [Var "$$", Val Null]
  print  = FunApp "print" [Var "$$", Val (StrVal "\n")]


