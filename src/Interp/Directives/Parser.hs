{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Interp.Directives.Parser
  ( parseDrct
  , Drct(..)
  )
where

import           RIO                     hiding ( many
                                                , optional
                                                , try
                                                )
import qualified Data.Text.Lazy                as TL
import           Utils.Types                    ( Var )

import           Data.Attoparsec.Text.Lazy

data Drct
  = Exit
  | Clear
  | Help
  | Rm Var

parseDrct :: TL.Text -> Either String Drct
parseDrct s = case parse drctParser s of
  Fail _ _ err -> Left err
  Done _ drct  -> Right drct

drctParser :: Parser Drct
drctParser =
  many' (char ' ')
    *>  try (string ":e" $> Exit)
    <|> try (string ":h" $> Help)
    <|> try (string ":c" $> Clear)
    <|> try (string ":rm " *> (Rm <$> takeLazyText))
    <?> "PiG directives"
