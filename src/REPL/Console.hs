{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Console
  ( startREPL
  ) where

import qualified Data.Text.Lazy                as L
import           Lang.Parser                    ( parseProg )
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
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                , supportsPretty
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
startREPL settings = liftIO supportsPretty
  >>= \prettyPrompt -> runInputT settings $ runLine prettyPrompt Green

runLine :: Bool -> Color -> REPL a
runLine pretty colour = lift getStore >>= \case
  Left _ -> return ()
  _right -> do
    let prompt = if pretty then style Faint . color colour else id
    line <- getInputLine $ prompt "PiG" <> "> "
    checkLine (runLine pretty) $ L.strip . fromString <$> line

checkLine :: (Color -> REPL a) -> Maybe L.Text -> REPL a
checkLine runLine' (Just line)
  | L.null line = runLine' Green
  | otherwise = if isDirective line
    then lift (interpWithStore (execute line)) >> runLine' Green
    else case parseProg line of
      Left  err  -> putStrLn err >> runLine' Red
      Right prog -> runProg runLine' prog
checkLine _ _ = return ()

runProg :: (Color -> REPL a) -> Expr -> REPL a
runProg runLine' expr = lift (interpWithStore (eval expr')) >> runLine' Green
 where
  expr'  = Seq [assign, If [(cond, print)] (Val Null)]
  assign = Assign "$$" (Val Null) expr
  cond   = FunApp "neq" [Var "$$", Val Null]
  print  = FunApp "print" [Var "$$", Val (StrVal "\n")]


