{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Drct where

import           Data.Map
import           Import
import           System.Console.Haskeline

exec :: Drct -> Interp ()
exec Exit  = return ()
exec Clear = withStore $ setGlobals $ const empty
exec Help =
  Interp
    . lift
    . outputStrLn
    $ "PiG interpreter directives: \n\
      \':help' - display this message\n\
      \':exit' or Ctrl+d - leave the interpreter\n\
      \':clear' - remove all variables\n\
      \':rm <name>' - remove variable <name>\n\
      \':load \"<file path>\"' - execute program from <file path> (not implemented yet)"
exec (Rm   var ) = withStore $ setGlobals (delete var)
exec (Load file) = undefined
