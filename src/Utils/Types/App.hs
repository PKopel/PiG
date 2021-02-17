{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils.Types.App
  ( Options(..)
  , App(..)
  , Interp(..)
  )
where

import           Utils.Types
import           Data.Version                   ( Version )
import           RIO
import           RIO.Orphans                    ( )
import           RIO.Process                    ( HasProcessContext(..)
                                                , ProcessContext
                                                )
import           System.Console.Haskeline       ( Settings )

import           Control.Monad.Catch            ( MonadMask
                                                , MonadCatch
                                                )
import           Control.Monad.State            ( MonadState
                                                , StateT(StateT)
                                                )

data Options = Options
  { optionsVerbose :: !Bool,
    optionsLoad :: !String
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appSettings :: !(Settings (Interp App)),
    appVersion :: !Version
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

newtype Interp app v = Interp {runInterp :: StateT (Scope, Store) (RIO app) v}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadState (Scope, Store)
    )
