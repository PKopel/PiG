{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Directives where

import           Data.Map
import           Import                  hiding ( try )
import           Interp.Statements
import           Lang.Parser
import           System.Console.Haskeline
import           System.IO.Error

execProg :: Store -> Prog -> InputT IO Store
execProg store (Stmt p) = runWithStore (eval p) store
execProg store (Drct p) = runWithStore (exec p) store

execFile :: FilePath -> Store -> InputT IO Store
execFile file store = (lift . tryIOError) (parseFile file) >>= \case
  Left  err          -> outputStrLn (show err) >> return store
  Right (Left  err ) -> outputStrLn err >> return store
  Right (Right prog) -> foldM (\str stmt -> execProg str stmt) store prog

exec :: Drct -> Interp ()
exec Exit  = return ()
exec Clear = withStore $ (over . scope) globalL (const empty)
exec Help =
  printString
    "PiG interpreter directives: \n\
    \':help' - display this message\n\
    \':exit' or Ctrl+d - leave the interpreter\n\
    \':clear' - remove all variables\n\
    \':rm <name>' - remove variable <name>\n\
    \':load \"<file path>\"' - execute program from <file path>\n"
exec (Rm   var ) = withStore $ (over . scope) globalL (delete var)
exec (Load file) = getStore >>= Interp . lift . execFile file >>= putStore
