{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Interp.Console
  ( startREPL
  )
where

import           RIO                     hiding ( Text )
import qualified Data.Text.Lazy                as Lazy
import           Utils.IO
import           Utils.Types.App                ( Interp )
import           Utils.Types                    ( Val(StrVal)
                                                , Expr(Val, Seq, FunApp)
                                                )
import           Utils.Interp                   ( getStore
                                                , interpWithStore
                                                )
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
runProg expr =
  let expr' = case expr of
    -- kind of a hack, but works
        e@(Seq _) -> FunApp ":print" [e, Val (StrVal "\n")]
        other     -> other
  in  lift (interpWithStore (eval expr')) >> runLine Green
