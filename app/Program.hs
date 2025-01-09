module Program (Options (..), program) where

import Parserka.Parser
import Parserka.YAML.Lexer
import Parserka.YAML.Parser (runParserOnTokens)
import Parserka.Pretty

data Options = Options
  { filepath :: String
  }
  deriving (Show)

program :: Options -> IO ()
program (Options f) = do
  file <- readFile f
  let tkres = runParserOnString yamlTokens file
  case tkres of
    Consumed (Ok tokens _ _) -> tokenSuccess tokens
    Empty (Ok tokens _ _) -> tokenSuccess tokens
    Consumed (Error msg) -> parseError msg
    Empty (Error msg) -> parseError msg
  where
    parseError msg = putStrLn  $ "Failed because of:\n" ++ (pretty msg)
    tokenSuccess tokens = do
      let res = runParserOnTokens tokens
      case res of 
            Consumed (Ok vals _ _) -> parseSuccess vals
            Empty (Ok vals _ _) -> parseSuccess vals
            Consumed (Error msg) -> parseError msg
            Empty (Error msg) -> parseError msg
      where 
        parseSuccess vals = putStrLn  $ concat $ map (flip prettyYAML 0) vals 
      


