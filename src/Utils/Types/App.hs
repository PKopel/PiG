{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Types.App where

import           Data.Version                   ( Version )
import           RIO                            ( Bool
                                                , String
                                                , lens
                                                , HasLogFunc(..)
                                                , LogFunc
                                                )
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )
import           System.Console.Haskeline       ( Settings )
import           Utils.Types                    ( Interp )

data Options = Options
  { optionsVerbose :: !Bool,
    optionsLoad :: !String
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appSettings :: !(Settings Interp),
    appVersion :: !Version
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })
