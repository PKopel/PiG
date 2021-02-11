{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Directives where

import           Import
import           Interp.Statements              ( evalFile )
import           Interp.Directives.Parser       ( parseDrct )
import           Data.Map                       ( delete
                                                , empty
                                                )

isDirective :: String -> Bool
isDirective s = ':' `elem` s


execute :: String -> Interp ()
execute str = case parseDrct str of
  Left  err  -> printString err
  Right drct -> exec drct

exec :: Drct -> Interp ()
exec Exit  = putStore (Left ())
exec Clear = withStore $ (over . scope) globalL (const empty)
exec Help =
  printString
    "PiG interpreter directives: \n\
    \:help | :h - display this message\n\
    \:exit | :e or Ctrl+d - leave the interpreter\n\
    \:clear | :c - remove all variables\n\
    \:rm <name> - remove variable <name>\n\
    \:load | :l \"<file path>\"' - execute program from <file path>\n"
exec (Rm   var ) = withStore $ (over . scope) globalL (delete var)
exec (Load file) = getStore >>= Interp . lift . evalFile file >>= putStore
