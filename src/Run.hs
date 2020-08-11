{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Map                      as Map
import           Import
import           Interp.Exec
import           Lang.Parser
import           System.IO

run :: RIO App ()
run = do
  logInfo "We're inside the experimental PiG interpreter!"
  runLine Map.empty

runLine :: Store -> RIO App ()
runLine store = do
  logSticky "PiG> "
  line <- liftIO getLine
  case parseProg line of
    Left  err  -> proceed err
    Right prog -> do
      ((), store') <- runProg store prog
      runLine store'
 where
  proceed "exit" = return ()
  proceed msg    = (logInfo . fromString) (msg <> "\n") >> runLine store
