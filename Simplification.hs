{-
  Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
  Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz>
  Year: 2020
  Module: Simplification
  Description: An implementation of the simplification of a context-free
               grammar algorithm.
-}


{-# LANGUAGE RecordWildCards #-}


module Simplification (simplify1, simplify2) where


import Data.List (intersect, nub, union)
import Parser (epsSymbol)
import Types (CFG(..), Rules, Symbols)


-- Converts an input grammar to the form where there are only nonterminal
-- symbols that generates terminal strings.
simplify1 :: CFG -> CFG
simplify1 CFG{..} = CFG
  { nonterminals= nonterminals'
  , terminals= terminals
  , startingSymbol= startingSymbol
  , rules= rules' }
  where
    terminatingNonterminals' = terminatingNonterminals rules terminals []
    nonterminals' =
      if startingSymbol `elem` terminatingNonterminals' then
        terminatingNonterminals'
      else terminatingNonterminals' ++ [startingSymbol]
    rules' = filter ( \(l, r) ->
        elem l terminatingNonterminals'
        && all (`elem` terminatingNonterminals' ++ terminals ++ [epsSymbol]) r
      ) rules

-- Computes a list of nonterminal symbols that generates terminal strings.
terminatingNonterminals :: Rules -> Symbols -> Symbols -> Symbols
terminatingNonterminals rules terminals n0 =
  if n0 == n1 then nub n1 else terminatingNonterminals rules terminals n1
  where
    n1 = map fst $
      filter (all (`elem` n0 ++ terminals ++ [epsSymbol]) . snd) rules


-- Converts an input grammar to the form where there are only nonterminal
-- symbols that generates terminal strings and where there are only reachable
-- symbols.
simplify2 :: CFG -> CFG
simplify2 = removeUnreachableSymbols . simplify1

-- Removes unreachable symbols from an input grammar.
removeUnreachableSymbols :: CFG -> CFG
removeUnreachableSymbols CFG{..} = CFG
  { nonterminals= nonterminals'
  , terminals= terminals'
  , startingSymbol= startingSymbol
  , rules= rules' }
  where
    reachableSymbols' = reachableSymbols rules [startingSymbol]
    nonterminals' = nonterminals `intersect` reachableSymbols'
    terminals' = terminals `intersect` reachableSymbols'
    rules' = filter ( \(l, r) ->
        elem l nonterminals'
        && all (`elem` reachableSymbols' ++ [epsSymbol]) r
      ) rules

-- Computes a list of reachable symbols.
reachableSymbols :: Rules -> Symbols -> Symbols
reachableSymbols rules v0 = if v0 == v1 then v1 else reachableSymbols rules v1
  where v1 = concatMap snd (filter ((`elem` v0) . fst) rules) `union` v0
