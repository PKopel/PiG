{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module REPL.Console
  ( startREPL
  ) where

import qualified Data.Text.Lazy                as L
import           Lang.Parser                    ( parseProg )
import           REPL.Directives                ( execute
                                                , isDirective
                                                )
import           REPL.Eval                      ( evalWithCach )
import           RIO                     hiding ( Text
                                                , catch
                                                )
import           System.Console.Haskeline       ( InputT
                                                , Settings
                                                , getInputLine
                                                , runInputT
                                                )
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                )
import           Utils.IO                       ( putStrLn )
import           Utils.Interp                   ( getStore
                                                , interpWithStore
                                                
                                                )
import           Utils.Types                    ( Expr(..)
                                                , Interp
                                                , Val(..)
                                                )

type REPL a = InputT (Interp a) Int

startREPL :: Settings (Interp a) -> Interp a Int
startREPL settings = runInputT settings $ runLine Green

runLine :: Color -> REPL a
runLine colour = lift getStore >>= \case
  Left val -> return val
  _right   -> do
#ifndef mingw32_HOST_OS
    line <- getInputLine $ (style Faint . color colour) "PiG" <> "> "
#else
    line <- getInputLine $ "PiG" <> "> "
#endif
    checkLine $ L.strip . fromString <$> line

checkLine :: Maybe L.Text -> REPL a
checkLine (Just line)
  | L.null line = runLine Green
  | otherwise = if isDirective line
    then lift (interpWithStore (execute line)) >> runLine Green
    else case parseProg line of
      Left  err  -> putStrLn err >> runLine Red
      Right prog -> runProg prog
checkLine _ = return 1

runProg :: Expr -> REPL a
runProg expr = lift (interpWithStore $ evalWithCach expr') >> runLine Green
 where
  expr'  = Seq [assign, If [(cond, print)] (Val Null)]
  assign = Assign "$$" (Val Null) expr
  cond   = FunApp (Var "neq") [Var "$$", Val Null]
  print  = FunApp (Var "print") [Var "$$", Val (StrVal "\n")]

