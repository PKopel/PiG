{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Directives
  ( isDirective
  , execute
  ) where

import           Data.Map                       ( delete
                                                , empty
                                                )
import qualified Data.Text.Lazy                as Lazy
import           RIO
import           Utils.IO                       ( putStr )
import           Utils.Interp                   ( putStore
                                                , withScopes
                                                )
import           Utils.Types                    ( Interp
                                                , Scope(scope)
                                                , Var
                                                , globalL
                                                )

data Drct
  = Exit
  | Clear
  | Help
  | Rm Var

parseDrct :: Lazy.Text -> Either Lazy.Text Drct
parseDrct s = case Lazy.words s of
  [dr, var] | dr `elem` [":rm", ":remove"] -> Right $ Rm var
  [dr] | dr `elem` [":c", ":clear"] -> Right Clear
  [dr] | dr `elem` [":e", ":exit"] -> Right Exit
  [dr] | dr `elem` [":h", ":help"] -> Right Help
  _ -> Left $ "Unrecognized directive '" <> s <> "', try ':h'\n"

isDirective :: Lazy.Text -> Bool
isDirective s = ":" `Lazy.isPrefixOf` s

execute :: Lazy.Text -> Interp a ()
execute str = case parseDrct str of
  Left  err  -> putStr err
  Right drct -> exec drct

exec :: Drct -> Interp a ()
exec Exit  = putStore $ Left 0
exec Clear = withScopes $ (over . scope) globalL (const empty)
exec Help =
  putStr
    "PiG interpreter directives: \n\
    \:help | :h - display this message\n\
    \:exit | :e or Ctrl+d - leave the interpreter\n\
    \:clear | :c - remove all variables\n\
    \:remove | :rm VAR - remove variable VAR\n"
exec (Rm var) = withScopes $ (over . scope) globalL (delete var)
