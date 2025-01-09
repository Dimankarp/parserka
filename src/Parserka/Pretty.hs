{-# LANGUAGE InstanceSigs #-}

module Parserka.Pretty (pretty, PrettyPrintable (..), prettyYAML) where

import Data.Map (foldlWithKey)
import Parserka.Parser
import Parserka.YAML.Parser (YAMLValue (..))

class PrettyPrintable a where
  pretty :: a -> String

instance PrettyPrintable Position where
  pretty (Position c l) =
    "(ln " <> (show l) <> ": col " <> (show c) <> ")"

instance PrettyPrintable Message where
  pretty (Message p unexp exps) =
    "Unexpected input:"
      <> (pretty p)
      <> ": "
      <> unexp
      <> "\n"
      <> (concat $ map (\s -> "Expected " <> s <> "\n") exps)

prettyYAML :: YAMLValue -> Int -> String
prettyYAML (StringValue v) ident =  show v
prettyYAML (BoolValue v) ident =  show v
prettyYAML (IntegerValue v) ident = show v
prettyYAML (DoubleValue v) ident =  show v
prettyYAML (NullValue) ident =  "null"
prettyYAML (ListValue vals) ident =
  "[\n"
    <> (concat $ fmap (\i -> (replicate (ident + 1) ' ') <> prettyYAML i (0) <> ",\n") vals)
    <> (replicate ident ' ')
    <> "]\n"
prettyYAML (DictionaryValue m) ident =
  "{\n"
    <> prettyMap
    <> (replicate ident ' ')
    <> "}\n"
  where
    prettyMap = foldlWithKey (\a k v -> a <> ((replicate (ident + 1) ' ') <> (prettyYAML k 0) <> ";" <> prettyYAML v (ident + 1 + (length (prettyYAML k 0)) + 1) <> "\n")) "" m
