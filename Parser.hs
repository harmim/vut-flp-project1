{-
  Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
  Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz>
  Year: 2020
  Module: Parser
  Description: An input parser. Parsing and validating an input grammar and
               converting it to the internal representation.
-}


{-# LANGUAGE RecordWildCards #-}


module Parser (parseCFG, epsSymbol) where


import Control.Applicative ((<$>), (<*>), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.List (group, sort)
import Text.Parsec (char, count, endBy, eof, many1, newline, oneOf, parse,
  sepBy, sepBy1, string)
import Text.Parsec.String (Parser)
import Types (CFG(..), Err, Rule, Rules, Symbol, Symbols)


-- Parses and validates an input grammar and converts it to the internal
-- representation. Returns an error message in case of invalid input grammar.
parseCFG :: String -> Err CFG
parseCFG = validate <=< left show . parse parser ""


-- Parses the entire context-free grammar.
parser :: Parser CFG
parser = CFG
  <$> nonterminalsParser <* newline
  <*> terminalsParser <* newline
  <*> startingSymbolParser <* newline
  <*> rulesParser <* eof


-- Parses a list of nonterminal symbols.
nonterminalsParser :: Parser Symbols
nonterminalsParser = sepBy1 (oneOf nonterminalSymbols) symbolSeparator


-- Parses a list of terminal symbols.
terminalsParser :: Parser Symbols
terminalsParser = sepBy (oneOf terminalSymbols) symbolSeparator


-- Parses the starting symbol.
startingSymbolParser :: Parser Symbol
startingSymbolParser = oneOf nonterminalSymbols


-- Parses a list of production rules.
rulesParser :: Parser Rules
rulesParser = endBy ruleParser newline

-- Parses single production rules.
ruleParser :: Parser Rule
ruleParser = (,)
  <$> oneOf nonterminalSymbols <* string "->"
  <*> ( count 1 (char epsSymbol)
    <|> many1 (oneOf $ nonterminalSymbols ++ terminalSymbols) )


-- A symbol separator.
symbolSeparator :: Parser Char
symbolSeparator = char ','


-- A list of allowed nonterminal symbols.
nonterminalSymbols :: Symbols
nonterminalSymbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- A list of allowed terminal symbols.
terminalSymbols :: Symbols
terminalSymbols = "abcdefghijklmnopqrstuvwxyz"

-- The empty string (epsilon) symbol.
epsSymbol :: Symbol
epsSymbol = '#'


-- Validates an input grammar.
validate :: CFG -> Err CFG
validate cfg@CFG{..} =
  if isValid then Right cfg else Left "Invalid input context-free grammar."
  where
    allUnique l = all ((==) 1 . length) $ (group . sort) l
    isValid =
      allUnique nonterminals
      && allUnique terminals
      && elem startingSymbol nonterminals
      && allUnique rules
      && all ( \(l, r) ->
        elem l nonterminals
        && all (`elem` nonterminals ++ terminals ++ [epsSymbol]) r
      ) rules
