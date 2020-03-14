{-
  Project: VUT FIT FLP 1. Project (Functional) - SIMPLIFY-BKG
  Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz>
  Year: 2020
  Module: Main
  Description: The main program. Performs input and output operations.
               Initiates the computation.
-}


module Main (main) where


import Parser (parseCFG)
import Simplification (simplify1, simplify2)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Types (CFG(..))


-- The main program.
main :: IO ()
main = do
  (action, input) <- processArgs =<< getArgs
  either die action $ parseCFG input


-- Processes input arguments and returns an action to be performed and an
-- input file.
processArgs :: [String] -> IO (CFG -> IO (), String)
processArgs [option] = processOptions option =<< getContents
processArgs [option, inputFile] = processOptions option =<< readFile inputFile
processArgs _ = die "Excepting an option argument and optionally an input \
  \file: simplify-bkg (-i|-1|-2) [input-file]"

-- Processes input options and returns an action to be performed and an
-- input file.
processOptions :: String -> String -> IO (CFG -> IO (), String)
processOptions option input = case option of
  "-i" -> return (printCFG, input)
  "-1" -> return (printCFGSimplify1, input)
  "-2" -> return (printCFGSimplify2, input)
  _ -> die $ "Unknown option: " ++ option


-- Prints the context-free grammar to the standard output.
printCFG :: CFG -> IO ()
printCFG = putStr . show


-- Prints the context-free grammar to the standard output after the 'simplify1'
-- function has been performed.
printCFGSimplify1 :: CFG -> IO ()
printCFGSimplify1 = printCFG . simplify1


-- Prints the context-free grammar to the standard output after the 'simplify2'
-- function has been performed.
printCFGSimplify2 :: CFG -> IO ()
printCFGSimplify2 = printCFG . simplify2


-- Terminates the program with an error code and an error message.
die :: String -> IO a
die str = hPutStrLn stderr str >> exitFailure
