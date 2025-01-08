{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parserka.YamlLexer (yamlTokens) where

import Parserka.Parser (Parser, Position, char, curPos, digit, excluding, getParserState, ignore, letter, many0, many1, manyAll, manyBreak, optional, stop, string, try, (<?>), (<|>))

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
      string "---" <?> "file starting ---"
      return (FileStartToken (curPos state))
  )

keyName :: Parser Char YAMLToken
keyName = do
  state <- getParserState
  name <- many1 (letter <|> digit)
  return (KeyToken (curPos state) name)

-- scalarBool =

scalar :: Parser Char YAMLToken
scalar = do
  state <- getParserState
  name <- many1 (letter <|> digit)
  return (DoubleQuoteString (curPos state) name) -- TODO:

keyValue :: Parser Char [YAMLToken]
keyValue = do
  key <- keyName
  char ':'
  spaces
  val <- optional (scalar)
  return
    ( [key] ++ case val of
        Just value -> [value]
        Nothing -> []
    )

listify :: Parser i a -> Parser i [a]
listify p = do
  res <- p
  return [res]

blockListItem :: Parser Char [YAMLToken]
blockListItem = do
  state <- getParserState
  char '-'
  spaces
  tokens <- (try keyValue <|> listify scalar)
  (return $ [ItemToken (curPos state)] ++ tokens)

fileStartLine :: Parser Char YAMLToken
fileStartLine = do
  token <- fileStart
  lineEnd
  return token

contentLine :: Parser Char [YAMLToken]
contentLine =
  do
    tokens <- (blockListItem <|> keyValue)
    lineEnd
    return tokens

comment :: Parser Char [Char]
comment = do char '#'; many0 (excluding ['\n'])

nl :: Parser Char ()
nl = do char '\n' <?> "new line"; return ()

spaces :: Parser Char [Char]
spaces = many0 (char ' ') <?> "possible spaces"

lineEnd :: Parser Char Bool
lineEnd = do spaces; optional comment; (try (do nl; return False) <|> (do stop; return True))

yamlTokens :: Parser Char [YAMLToken]
yamlTokens = do
  tokens <-
    manyAll
      ( do
          st <- fileStartLine
          contents <-
            many1
              (do spaces; contentLine <|> blank)
          return $ [st] ++ (concat contents)
      )
  return $ concat tokens
  where
    blank = do
      isStop <- lineEnd
      if isStop
        then manyBreak
        else return []