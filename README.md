# Functional and Logic Programming - Functional Project
## SIMPLIFY-BKG

##### Author: Dominik Harmim <xharmi00@stud.fit.vutbr.cz>

## Build
Using the command `make`, the project is compiled using the `ghc` compiler
and a program `simplify-bkg` is created.

## Run
After the compilation (see the section above), it can be run as follows:
```bash
$ ./simplify-bkg (-i|-1|-2) [input-file]
```
`input-file` is the name of an input file. If it is not specified, the program
reads the standard input. With the option `-i`, the program prints the loaded
context-free grammar. With the option `-1`, the program prints the
context-free grammar without nonterminal symbols that generate nonterminal
strings. With the option `-2`, the program prints the context-free grammar
without nonterminal symbols that generate nonterminal strings and without
unreachable symbols.

## Description
The program removes unnecessary symbols from the context-free grammar. An
input grammar is first validated and in case of invalid format or invalid
grammar, an error message is printed to the standard error output and the
program is terminated with an error code. Otherwise, the unnecessary symbols
are removed and the grammar is printed to the standard output. The source
codes are located in the following files:
- `Types.hs` - Definitions of data types. The internal representation of the
  context-free grammar.
  * The grammar is represented as a structure where nonterminal symbols are
    a list of characters, terminal symbols are a list of characters, the
    starting symbol is a character, and production rules are a list of pairs
    where the left side of the rule is a character and the right side is
    a list of characters.
- `Parser.hs` - An input parser. Parsing and validating an input grammar and
  converting it to the internal representation.
  * The library `Text.Parsec` is used for parsing the input grammar.
- `Simplification.hs` - An implementation of the simplification of the
  context-free grammar algorithm.
- `Main.hs` - The main program. Performs input and output operations. Initiates
  the computation.
