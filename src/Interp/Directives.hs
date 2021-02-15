{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Directives where

import           RIO
import           Utils.Types.App                ( Interp )
import           Utils.Types                    ( Scope(scope)
                                                , globalL
                                                )
import           Utils.Interp                   ( printString
                                                , putStore
                                                , withScopes
                                                )
import           Interp.Directives.Parser       ( parseDrct
                                                , Drct(..)
                                                )
import           Data.Map                       ( delete
                                                , empty
                                                )

isDirective :: String -> Bool
isDirective s = ':' `elem` s

execute :: String -> Interp a ()
execute str = case parseDrct str of
  Left  err  -> printString (err ++ "\n")
  Right drct -> exec drct

exec :: Drct -> Interp a ()
exec Exit  = putStore (Left ())
exec Clear = withScopes $ (over . scope) globalL (const empty)
exec Help =
  printString
    "PiG interpreter directives: \n\
    \:help | :h - display this message\n\
    \:exit | :e or Ctrl+d - leave the interpreter\n\
    \:clear | :c - remove all variables\n\
    \:rm <name> - remove variable <name>\n"
exec (Rm var) = withScopes $ (over . scope) globalL (delete var)
