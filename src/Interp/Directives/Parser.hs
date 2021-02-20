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
import qualified Data.Text.Lazy                as Lazy
import           Utils.Types                    ( Var )

import           Data.Attoparsec.Text.Lazy

data Drct
  = Exit
  | Clear
  | Help
  | Rm Var

parseDrct :: Lazy.Text -> Either String Drct
parseDrct s = case parse drctParser s of
  Fail _ _ err -> Left err
  Done _ drct  -> Right drct

drctParser :: Parser Drct
drctParser =
  try (drct ":exit" ":e" $> Exit)
    <|> try (drct ":help" ":h" $> Help)
    <|> try (drct ":clear" ":c" $> Clear)
    <|> try (string ":rm " *> (Rm <$> takeLazyText))
    <?> "PiG directives"
 where
  drct long short = (try (string long) <|> try (string short)) *> endOfInput
