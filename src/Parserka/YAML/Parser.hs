{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parserka.YAML.Parser (YAMLValue(..), parseYamlFromString, runParserOnTokens) where

import Data.Map as Map
import Parserka.Parser
import Parserka.YAML.Lexer

data YAMLValue
  = StringValue String
  | BoolValue Bool
  | IntegerValue Integer
  | DoubleValue Double
  | NullValue
  | ListValue [YAMLValue]
  | DictionaryValue (Map YAMLValue YAMLValue)
  deriving (Eq, Show, Ord)

nextTokenPos :: (Position -> YAMLToken -> [YAMLToken] -> Position)
nextTokenPos (Position c l) _ tks = case tks of
  (t : _) -> sourcePos t
  [] -> Position c l

token :: YAMLToken -> Parser YAMLToken YAMLToken
token c = satisfy (sameToken c) nextTokenPos <?> (show c)

tokenFS :: Parser YAMLToken YAMLToken
tokenFS = token (FileStartToken (Position 0 0))

tokenInt :: Parser YAMLToken YAMLToken
tokenInt = token (IntegerToken (Position 0 0) 0)

tokenDouble :: Parser YAMLToken YAMLToken
tokenDouble = token (DoubleToken (Position 0 0) 0)

tokenBool :: Parser YAMLToken YAMLToken
tokenBool = token (BoolToken (Position 0 0) False)

tokenDoubleQuoteString :: Parser YAMLToken YAMLToken
tokenDoubleQuoteString = token (DoubleQuoteStringToken (Position 0 0) "")

tokenKey :: Parser YAMLToken YAMLToken
tokenKey = token (KeyToken (Position 0 0) "")

tokenItem :: Parser YAMLToken YAMLToken
tokenItem = token (ItemToken (Position 0 0))

identToken :: YAMLToken -> Int
identToken t = identPos $ sourcePos t

identPos :: Position -> Int
identPos p = col p

nested :: Int -> Parser i YAMLToken -> Parser i YAMLToken
nested fident p = do
  tk <- p
  let tkident = identToken tk
  if tkident > fident
    then return tk
    else perror "invalid nesting identation"

follows :: Int -> Parser i YAMLToken -> Parser i YAMLToken
follows fident p = do
  tk <- p
  let tkident = identToken tk
  if tkident == fident
    then return tk
    else perror "invalid following identation"

scalarValue :: Parser YAMLToken YAMLValue
scalarValue =
  ( do
      (IntegerToken _ v) <- tokenInt
      return $ IntegerValue v
  )
    <|> ( do
            (DoubleToken _ v) <- tokenDouble
            return $ DoubleValue v
        )
    <|> ( do
            (BoolToken _ v) <- tokenBool
            return $ BoolValue v
        )
    <|> ( do
            (DoubleQuoteStringToken _ v) <- tokenDoubleQuoteString
            return $ StringValue v
        )

object :: Int -> Parser YAMLToken YAMLValue
object ide = (scalarValue <|> (try $ dictionary ide) <|> (try $ blockStyleList ide) <|> return NullValue) <?> "field value"

blockStyleList :: Int -> Parser YAMLToken YAMLValue
blockStyleList ide =
  do
    (ItemToken p) <- nested ide $ try tokenItem
    let curident = identPos p
    val <- object (curident)
    items <-
      many0
        ( do
            (ItemToken _) <- follows curident $ try tokenItem
            v <- object (curident)
            return (v)
        )
    return $ ListValue ([val] ++ items)

dictionary :: Int -> Parser YAMLToken YAMLValue
dictionary ide =
  do
    (KeyToken p k) <- nested ide $ try tokenKey
    let curident = identPos p
    val <- object (curident)
    pairs <-
      many0
        ( do
            (KeyToken _ nk) <- follows curident $ try tokenKey
            v <- object (curident)
            return (StringValue nk, v)
        )
    return $ DictionaryValue (fromList $ [(StringValue k, val)] ++ pairs)

yamlFiles :: Parser YAMLToken [YAMLValue]
yamlFiles = do
  files <-
    manyAll
      ( do
          tokenFS
          dictionary $ -1
      )
  return files

parseYamlFromString :: String -> Parserka.Parser.Consumed YAMLToken [YAMLValue]
parseYamlFromString s =
  let res = runParserOnString yamlTokens s
   in case res of
        Consumed (Ok tokens _ _) -> runParser yamlFiles (State tokens (Position 0 1))
        other -> error (show other)

runParserOnTokens :: [YAMLToken] -> Consumed YAMLToken [YAMLValue]
runParserOnTokens tokens = runParser yamlFiles (State tokens (Position 0 1))
