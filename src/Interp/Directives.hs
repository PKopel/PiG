{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interp.Directives where

import           Data.Map
import           Import                  hiding ( try )
import qualified Interp.Statements             as S
import           Lang.Parser
import           System.Console.Haskeline
import           System.IO.Error

execProg :: Store -> Prog -> InputT IO ((), Store)
execProg store (Stmt p) = runWithStore (S.exec p) store
execProg store (Drct p) = runWithStore (exec p) store

execFile :: FilePath -> Store -> InputT IO Store
execFile file store = (lift . tryIOError) (parseFile file) >>= \case
  Left  err        -> outputStrLn (show err) >> return store
  Right (Left err) -> outputStrLn err >> return store
  Right (Right prog) ->
    foldM (\str stmt -> execProg str stmt >>= return . snd) store prog

exec :: Drct -> Interp ()
exec Exit  = return ()
exec Clear = withStore . setGlobals $ const empty
exec Help =
  Interp
    . lift
    . outputStrLn
    $ "PiG interpreter directives: \n\
      \':help' - display this message\n\
      \':exit' or Ctrl+d - leave the interpreter\n\
      \':clear' - remove all variables\n\
      \':rm <name>' - remove variable <name>\n\
      \':load \"<file path>\"' - execute program from <file path>"
exec (Rm   var ) = withStore . setGlobals $ delete var
exec (Load file) = getStore >>= Interp . lift . execFile file >>= putStore
