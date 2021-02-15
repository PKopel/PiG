{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run
  )
where

import           Data.Version                   ( showVersion )
import           RIO
import           Utils.Types.App                ( App
                                                  ( appVersion
                                                  , appSettings
                                                  , appOptions
                                                  )
                                                , Options(optionsLoad)
                                                )
import           Utils.Interp                   ( runWithStoreIO )
import           Utils.Types                    ( Val(StrVal)
                                                , Expr(Val, Load)
                                                , emptyStore
                                                )
import           Interp.Console                 ( startREPL )
import           Interp.Statements              ( eval )

run :: RIO App ()
run = do
  version  <- view $ to appVersion
  settings <- view $ to appSettings
  store    <- view (to appOptions) >>= startStore
  case store of
    Left  _ -> return ()
    Right _ -> do
      logInfo
        (  "We're inside the experimental PiG interpreter!\nversion: "
        <> fromString (showVersion version)
        <> "\ntype ':help' or ':h' for more information "
        )
      void $ runWithStoreIO (startREPL settings) store
 where
  startStore ops = case optionsLoad ops of
    [] -> return $ Right emptyStore
    file ->
      runWithStoreIO (eval . Load . Val . StrVal $ file) $ Right emptyStore
