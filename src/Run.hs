{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run,
  )
where

import Data.Map as Map
import Data.Text.IO as TIO
import Import
import Interp
import Parser

run :: RIO App ()
run = do
  logInfo "We're inside the experimental PiG interpreter!"
  runLine Map.empty

runLine :: Store -> RIO App ()
runLine store = do
  logSticky "PiG> "
  line <- liftIO TIO.getLine
  case runParser line of
    Left msg -> proceed msg
    Right prog -> do
      ((), store') <- runProg store prog
      runLine store'
  where
    proceed "exit" = return ()
    proceed msg = (logInfo . displayShow) (msg <> "\n") >> runLine store
