{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL.Directives.Parser
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
  try (":rm " *> (Rm <$> takeLazyText))
    <|> try (drct ":clear" ":c" $> Clear)
    <|> try (drct ":help" ":h" $> Help)
    <|> try (drct ":exit" ":e" $> Exit)
    <?> "PiG directives"
  where drct long short = (try long <|> try short) *> endOfInput
