{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  )
where

import           Utils.Completion               ( completion )
import           Utils.Types
import           Options.Applicative.Simple
import qualified Paths_PiG
import           RIO
import           RIO.Process                    ( mkDefaultProcessContext )
import           Run                            ( run )
import           System.Console.Haskeline

main :: IO ()
main = do
  options <- fst <$> pigOptions
  lo      <- logOptionsHandle stderr False
  pc      <- mkDefaultProcessContext
  let settings = Settings { complete       = completion
                          , historyFile    = Nothing
                          , autoAddHistory = True
                          }
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc        = lf
                  , appProcessContext = pc
                  , appOptions        = options
                  , appSettings       = settings
                  , appVersion        = Paths_PiG.version
                  }
    in  runRIO app run

pigOptions :: IO (Options, ())
pigOptions = simpleOptions
  $(simpleVersion Paths_PiG.version)
  "PiG - interpreter for a simple imperative language"
  "start the interpreter"
  (   Options <$> strOption
        (long "load" <> short 'l' <> metavar "FILE" <> value "" <> help
          "start interpreter with FILE loaded"
        )
  )
  empty
