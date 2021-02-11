{-# LANGUAGE NoImplicitPrelude #-}

module REPL.Directives where

import           Import
import           Interp.Statements              ( eval )
import           Lang.Parser                    ( parseFile )
import           REPL.Directives.Parser         ( parseDrct )
import           System.Console.Haskeline       ( InputT
                                                , outputStrLn
                                                )
import           System.IO.Error                ( tryIOError )
import qualified Data.Text                     as T
import           Data.Map                       ( delete
                                                , empty
                                                )

isDirective :: String -> Bool
isDirective s = ':' `elem` s

execFile :: FilePath -> Store -> InputT IO Store
execFile file store = do
  contents <- (lift . tryIOError)
    (parseFile file . T.unpack <$> readFileUtf8 file)
  case contents of
    Left  err          -> outputStrLn (show err) >> return store
    Right (Left  err ) -> outputStrLn err >> return store
    Right (Right expr) -> runWithStore (eval expr) store

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
exec (Load file) = getStore >>= Interp . lift . execFile file >>= putStore
