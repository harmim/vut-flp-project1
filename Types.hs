{-
  Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
  Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz>
  Year: 2020
  Module: Types
  Description: Definitions of data types. The internal representation of the
               context-free grammar.
-}


{-# LANGUAGE RecordWildCards #-}


module Types (CFG(..), Err, Rule, Rules, Symbol, Symbols) where


import Data.List (intercalate)


-- The nonterminal or terminal symbol.
type Symbol = Char

-- A list of nonterminal and terminal symbols.
type Symbols = [Symbol]


-- The production rule is a pair of the symbol and a list of symbols.
type Rule = (Symbol, Symbols)

-- A list of production rules.
type Rules = [Rule]


-- The context-free grammar (CFG) is represented by a list of nonterminal
-- symbols, a list of terminal symbols, the starting nonterminal symbol,
-- and a list of production rules.
data CFG = CFG
  { nonterminals :: Symbols
  , terminals :: Symbols
  , startingSymbol :: Symbol
  , rules :: Rules }

-- The definition of showing the CFG.
instance Show CFG where
  show CFG{..} = unlines $
    [intercalate "," $ map (: []) nonterminals] ++
    [intercalate "," $ map (: []) terminals] ++
    [[startingSymbol]] ++
    map (\(l, r) -> [l] ++ "->" ++ r) rules


-- A type of a result or an error message.
type Err = Either String
