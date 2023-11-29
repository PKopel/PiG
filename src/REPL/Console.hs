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
#ifndef mingw32_HOST_OS
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                )
#endif
import           Utils.IO                       ( putStrLn )
import           Utils.Interp                   ( getStore
                                                , interpWithStore
                                                )
import           Utils.Types                    ( Expr(..)
                                                , Interp
                                                , Val(..)
                                                )

type REPL a = InputT (Interp a) Int

#ifndef mingw32_HOST_OS
{- if not on Windows, use colors in terminal -}

startREPL :: Settings (Interp a) -> Interp a Int
startREPL settings = runInputT settings $ runLine Green

runLine :: Color -> REPL a
runLine colour = lift getStore >>= \case
  Left val -> return val
  _right   -> do
    line <- getInputLine $ (style Faint . color colour) "PiG" <> "> "
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

#else
{- no colors on Windows -}

data Previous = Ok | Err

startREPL :: Settings (Interp a) -> Interp a Int
startREPL settings = runInputT settings $ runLine Ok

runLine :: Previous -> REPL a
runLine previous = lift getStore >>= \case
  Left val -> return val
  _right   -> do
    line <- getInputLine $ prompt <> "> "
    checkLine $ L.strip . fromString <$> line
 where
  prompt = case previous of
    Err -> "Err"
    Ok  -> "PiG"

checkLine :: Maybe L.Text -> REPL a
checkLine (Just line)
  | L.null line = runLine Ok
  | otherwise = if isDirective line
    then lift (interpWithStore (execute line)) >> runLine Ok
    else case parseProg line of
      Left  err  -> putStrLn err >> runLine Err
      Right prog -> runProg prog
checkLine _ = return 1

#endif

runProg :: Expr -> REPL a
runProg expr = lift (interpWithStore $ evalWithCach expr') >> runNext
 where
  expr'   = Seq [assign, If [(cond, print)] (Val Null)]
  assign  = Assign "$$" (Val Null) expr
  cond    = FunApp (Var "neq") [Var "$$", Val Null]
  print   = FunApp (Var "print") [Var "$$", Val (StrVal "\n")]
#ifndef mingw32_HOST_OS
  runNext = runLine Green
#else
  runNext = runLine Ok
#endif

