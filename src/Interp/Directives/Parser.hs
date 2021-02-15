{-# LANGUAGE NoImplicitPrelude #-}
module Interp.Directives.Parser where

import           RIO                     hiding ( many
                                                , optional
                                                , try
                                                , (<|>)
                                                )
import           Utils.Types                    ( Var )
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

data Drct
  = Exit
  | Clear
  | Help
  | Rm Var

parseDrct :: String -> Either String Drct
parseDrct s = case parse drctParser "PiG REPL" s of
  Left  err  -> Left $ show err
  Right drct -> Right drct

drctParser :: Parser Drct
drctParser =
  many (char ' ')
    *>  try (string ":e" $> Exit)
    <|> try (string ":h" $> Help)
    <|> try (string ":c" $> Clear)
    <|> try (string ":rm " *> (Rm <$> many anyToken))
    <?> "PiG directives"
