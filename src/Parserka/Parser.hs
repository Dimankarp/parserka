{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parserka.Parser (Parser, Position, curPos, char, string, getParserState, (<?>), excluding, many1, many0, (<|>), try, runParserOnString, optional, stop) where

import Control.Applicative (Alternative)
import Data.Char (isAlpha, isDigit)
import GHC.Base (Alternative (..))

data Position = Position {col :: Int, line :: Int} deriving (Show)

data Message = Message {pos :: Position, unexpected :: String, expected :: [String]} deriving (Show)

data State i = State {input :: [i], curPos :: Position} deriving (Show)

data Reply i a = Ok a (State i) (Message) | Error (Message) deriving (Show)

data Consumed i a = Consumed (Reply i a) | Empty (Reply i a) deriving (Show)

newtype Parser i a = Parser
  {runParser :: State i -> Consumed i a}

instance Functor (Parser i) where
  fmap f m = m >>= return . f

instance Applicative (Parser i) where
  pure x = Parser $ \state -> Empty (Ok x state (Message (curPos state) [] []))
  m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad (Parser i) where
  p >>= f =
    Parser
      ( \state -> case (runParser p state) of
          Empty r1 ->
            case (r1) of
              Ok x state1 msg -> case (runParser (f x) state1) of
                Empty r2 -> case r2 of
                  Ok x' state2 msg2 -> mergeOk x' state2 msg msg2
                  Error msg2 -> mergeError msg msg2
                other -> other
              Error msg -> Empty (Error msg)
          Consumed r1 ->
            Consumed
              ( case (r1) of
                  Ok x rest msg -> case (runParser (f x) rest) of
                    Consumed r2 -> r2
                    Empty r2 -> r2
                  Error msg -> Error msg
              )
      )
  return = pure

instance Alternative (Parser i) where
  p <|> q =
    Parser
      ( \state -> case (runParser p state) of
          Empty (Error msg1) -> case (runParser q state) of
            Empty (Error msg2) -> mergeError msg1 msg2
            Empty (Ok x state' msg2) -> mergeOk x state' msg1 msg2
            consumed -> consumed
          Empty (Ok x state' msg1) -> case (runParser q state) of
            Empty (Error msg2) -> mergeOk x state' msg1 msg2
            Empty (Ok _ _ msg2) -> mergeOk x state' msg1 msg2
            consumed -> consumed
          consumed -> consumed
      )
  empty = Parser $ \_ -> Empty (Error (Message (Position 0 0) [] []))

(<?>) :: Parser i a -> String -> Parser i a
p <?> label = Parser $
  \state ->
    case (runParser p state) of
      Empty (Error msg) ->
        Empty (Error (expect msg label))
      Empty (Ok x st msg) ->
        Empty (Ok x st (expect msg label))
      other -> other
  where
    expect (Message msgPos unexp _) msgExp = (Message msgPos unexp [msgExp])

updateParserState :: (State i -> State i) -> Parser i (State i)
updateParserState f =
  Parser $ \s -> let s' = f s in Empty (Ok s' s' (Message (curPos s) [] []))

getParserState = updateParserState id

setParserState st = updateParserState (const st)

mergeOk :: a -> State i -> Message -> Message -> Consumed i a
mergeOk x st msg1 msg2 = Empty (Ok x st (merge msg1 msg2))

mergeError :: Message -> Message -> Consumed i a
mergeError msg1 msg2 = Empty (Error (merge msg1 msg2))

merge :: Message -> Message -> Message
merge (Message msgPos i exp1) (Message _ _ exp2) = Message msgPos i (exp1 ++ exp2)

satisfy :: (Show i) => (i -> Bool) -> (Position -> i -> [i] -> Position) -> Parser i i
satisfy test nextPos = Parser $ \(State input curPos) -> case (input) of
  (c : cs)
    | test c ->
        let newPos = nextPos curPos c cs
            newState = State cs newPos
         in seq newPos (Consumed (Ok c newState (Message curPos [] [])))
  (c : cs) -> Empty (Error (Message curPos (show c) []))
  [] -> Empty (Error (Message curPos "end of input" []))

nextCharPos :: (Position -> Char -> [Char] -> Position)
nextCharPos (Position c l) ch _ =
  if ch == '\n'
    then
      Position 0 (l + 1)
    else Position (c + 1) l

char :: Char -> Parser Char Char
char ch =
  satisfy
    (== ch)
    nextCharPos

excluding :: [Char] -> Parser Char Char
excluding chs = satisfy (not . flip elem chs) nextCharPos

letter :: Parser Char Char
letter = satisfy isAlpha nextCharPos <?> "letter"

digit :: Parser Char Char
digit = satisfy isDigit nextCharPos <?> "digit"

string :: [Char] -> Parser Char ()
string s = string' s <?> "string"

string' :: [Char] -> Parser Char ()
string' [] = return ()
string' (i : is) =
  ( do
      char i
      string is
  )

idPart :: Parser Char Char
idPart = letter <|> digit

stop :: (Show i) => Parser i ()
stop = Parser $ go
  where
    go state = case input state of
      [] -> Empty (Ok () state (Message (curPos state) [] []))
      (c : cs) -> Empty (Error (Message (curPos state) (show c) ["stop"]))

try :: Parser i a -> Parser i a
try p = Parser $
  \state -> case (runParser p state) of
    Consumed err -> Empty err
    other -> other

optional :: Parser i a -> Parser i (Maybe a)
optional p = Parser $
  \state -> case (runParser p state) of
    Empty err -> case (err) of
      Error msg -> Empty (Ok Nothing state msg)
      Ok x state' msg -> Empty (Ok (Just x) state' msg)
    Consumed err -> case (err) of
      Error msg -> Consumed (Error msg)
      Ok x state' msg -> Consumed (Ok (Just x) state' msg)

many1 :: Parser i a -> Parser i [a]
many1 p =
  do
    x <- p
    xs <- (many1 p <|> return [])
    return (x : xs)

many0 p = try (many1 p) <|> return []

identificator = do many1 (letter <|> digit <|> char '_'); stop

runParserOnString :: Parser Char a -> String -> Consumed Char a
runParserOnString p s = runParser p (State s (Position 0 0))
