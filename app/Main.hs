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
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
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
              appSettings = settings
            }
     in runRIO app run
