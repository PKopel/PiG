{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_PiG
import RIO.Process
import Run
import System.Console.Haskeline

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_PiG.version)
      "PiG - interpreter for a simple imperative language"
      "start the interpreter"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "verbose output"
            )
          <*> strOption
            ( long "file"
                <> short 'f'
                <> metavar "FILE"
                <> value ""
                <> help "start interpreter with FILE loaded"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  let settings = Settings {complete = noCompletion, historyFile = Nothing, autoAddHistory = True}
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options,
              appSettings = settings,
              appVersion = Paths_PiG.version
            }
     in runRIO app run
