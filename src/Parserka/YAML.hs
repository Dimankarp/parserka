{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parserka.YAML (yamlTokens) where

import Parserka.Parser (Parser, Position, char, curPos, excluding, getParserState, many0, optional, stop, string, try, (<?>), (<|>))

-- Lexer

data YAMLToken
  = FileStartToken Position
  | IntegerToken Position Integer
  | DoubleToken Position Double
  | BoolToken Position Bool
  | DoubleQuoteString Position String
  | KeyToken Position String
  | ItemToken Position
  deriving (Show)

fileStart :: Parser Char YAMLToken
fileStart =
  ( do
      state <- getParserState
      try (string "---") <?> "file starting ---"
      return (FileStartToken (curPos state))
  )

-- contentLine = keyValue <|> listItem

comment :: Parser Char [Char]
comment = do char '#'; many0 (excluding ['\n'])

nl :: Parser Char ()
nl = do char '\n' <?> "new line"; return ()

spaces :: Parser Char [Char]
spaces = many0 (char ' ') <?> "possible spaces"

lineEnd :: Parser Char ()
lineEnd = do spaces; optional comment; (nl <|> stop)

yamlTokens :: Parser Char YAMLToken
yamlTokens = do
  st <- fileStart
  lineEnd
  return st