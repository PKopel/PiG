{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Map                      as Map
import           Data.Text.IO                  as TIO
import           Import
import           Interp
import           Parser

run :: RIO App ()
run = do
  logInfo "We're inside the experimental PiG interpretter!\n"
  runLine Map.empty
  logInfo "exit\n"

runLine :: Store -> RIO App ()
runLine store = do
  logInfo "PiG> "
  line <- liftIO TIO.getLine
  case runParser line >>= runProg store of
    Left  msg    -> proceed msg
    Right store' -> runLine store'
 where
  proceed ":exit" = return ()
  proceed msg     = (logInfo . fromString) (msg ++ "\n") >> runLine store
