{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.IO where

import           RIO                            ( FilePath
                                                , MonadIO(..)
                                                , (.)
                                                )
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TLIO

getLine :: MonadIO m => m TL.Text
getLine = liftIO TLIO.getLine

putStr :: MonadIO m => TL.Text -> m ()
putStr = liftIO . TLIO.putStr

putStrLn :: MonadIO m => TL.Text -> m ()
putStrLn = liftIO . TLIO.putStrLn

readFile :: MonadIO m => FilePath -> m TL.Text
readFile = liftIO . TLIO.readFile
