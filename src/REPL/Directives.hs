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
import           REPL.Directives.Parser         ( Drct(..)
                                                , parseDrct
                                                )
import           RIO
import           Utils.IO                       ( putStr )
import           Utils.Interp                   ( putStore
                                                , withScopes
                                                )
import           Utils.Types                    ( Interp
                                                , Scope(scope)
                                                , globalL
                                                )

isDirective :: Lazy.Text -> Bool
isDirective s = ":" `Lazy.isPrefixOf` s

execute :: Lazy.Text -> Interp a ()
execute str = case parseDrct str of
  Left  "string" -> putStr $ "no directive '" <> str <> "', try ':h'\n"
  Left  err      -> putStr $ fromString err <> "\n"
  Right drct     -> exec drct

exec :: Drct -> Interp a ()
exec Exit  = putStore $ Left 0
exec Clear = withScopes $ (over . scope) globalL (const empty)
exec Help =
  putStr
    "PiG interpreter directives: \n\
    \:help | :h - display this message\n\
    \:exit | :e or Ctrl+d - leave the interpreter\n\
    \:clear | :c - remove all variables\n\
    \:rm VAR - remove variable VAR\n"
exec (Rm var) = withScopes $ (over . scope) globalL (delete var)
