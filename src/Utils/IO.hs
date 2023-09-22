{-# LANGUAGE NoImplicitPrelude #-}

module Utils.IO where

import           RIO                            ( FilePath
                                                , MonadIO(..)
                                                , (.)
                                                )
import qualified Data.Text.Lazy                as Lazy
import qualified Data.Text.Lazy.IO             as LazyIO

getLine :: MonadIO m => m Lazy.Text
getLine = liftIO LazyIO.getLine

putStr :: MonadIO m => Lazy.Text -> m ()
putStr = liftIO . LazyIO.putStr

putStrLn :: MonadIO m => Lazy.Text -> m ()
putStrLn = liftIO . LazyIO.putStrLn

readFile :: MonadIO m => FilePath -> m Lazy.Text
readFile = liftIO . LazyIO.readFile
