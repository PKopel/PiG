{-# LANGUAGE NoImplicitPrelude #-}
module Interp.Directives.Parser where

import           Import                  hiding ( many
                                                , optional
                                                , try
                                                , (<|>)
                                                )

import           Text.Parsec
import           Text.Parsec.String             ( Parser )

parseDrct :: String -> Either String Drct
parseDrct s = case parse drctParser "REPL directives" s of
  Left  err  -> Left $ show err
  Right drct -> Right drct

drctParser :: Parser Drct
drctParser =
  many (char ' ')
    *>  try (string ":e" $> Exit)
    <|> try (string ":h" $> Help)
    <|> try (string ":c" $> Clear)
    <|> try (string ":rm " *> (Rm <$> anyStr))
    <|> ((try (string ":l ") <|> string ":load ") *> (Load <$> quotes anyStr))
 where
  quotes = between (char '"') (char '"')
  anyStr = many anyToken
