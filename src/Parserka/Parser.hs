{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parserka.Parser (char) where

import Control.Applicative (Alternative)
import Data.Char (isAlpha, isDigit)
import GHC.Base (Alternative (..))

data Consumed a i = Consumed (Reply a i) | Empty (Reply a i) deriving (Show)

data Reply a i = Ok a [i] | Error deriving (Show)

newtype Parser i a = Parser
  {runParser :: [i] -> Consumed a i}

instance Functor (Parser i) where
  fmap f m = m >>= return . f

instance Applicative (Parser i) where
  pure x = Parser $ \i -> Empty (Ok x i)
  m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad (Parser i) where
  p >>= f =
    Parser
      ( \i -> case (runParser p i) of
          Empty r1 ->
            case (r1) of
              Ok x rest -> (runParser (f x) rest)
              Error -> Empty Error
          Consumed r1 ->
            Consumed
              ( case (r1) of
                  Ok x rest -> case (runParser (f x) rest) of
                    Consumed r2 -> r2
                    Empty r2 -> r2
                  Error -> Error
              )
      )
  return = pure

instance Alternative (Parser i) where
  p <|> q =
    Parser
      ( \i -> case (runParser p i) of
          Empty Error -> (runParser q i)
          Empty ok -> case (runParser q i) of
            Empty _ -> Empty ok
            consumed -> consumed
          consumed -> consumed
      )
  empty = Parser $ \_ -> Empty Error

satisfy :: (i -> Bool) -> Parser i i
satisfy test = Parser $ \i -> case (i) of
  [] -> Empty Error
  (c : cs)
    | test c -> Consumed (Ok c cs)
    | otherwise -> Empty Error

char :: (Eq i) => i -> Parser i i
char c = satisfy (== c)

letter :: Parser Char Char
letter = satisfy isAlpha

digit :: Parser Char Char
digit = satisfy isDigit

string :: (Eq i) => [i] -> Parser i ()
string [] = return ()
string (i : is) = do
  char i
  string is

idPart :: Parser Char Char
idPart = letter <|> digit

stop :: Parser i ()
stop = Parser $ go
  where
    go [] = Empty (Ok () [])
    go _ = Empty Error

try :: Parser i a -> Parser i a
try p = Parser $
  \i -> case (runParser p i) of
    Consumed Error -> Empty Error
    other -> other

identificator = try (do idPart; stop) <|> (do idPart; identificator)
