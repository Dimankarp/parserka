module Main (main) where

import Options.Applicative as O
import Program

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument
      (metavar "FILENAME")

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  program opts
  where
    optsParserInfo =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "A parsec-like YAML parser build with parserka package." <> header "Parserka YAML")