{-# LANGUAGE QuasiQuotes #-}

module DocOptParsingArgsSample where

import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Console.Docopt

-----------------------------------------------------------------------------------

patterns :: Docopt
patterns = [docoptFile|DOCOPT_SAMPLE_USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

-----------------------------------------------------------------------------------

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "cat")) $ do
    file <- args `getArgOrExit` (argument "file")
    putStr =<< readFile file

  when (args `isPresent` (command "echo")) $ do
    let charTransform = if args `isPresent` (longOption "caps")
                          then toUpper
                          else id
    string <- args `getArgOrExit` (argument "string")
    putStrLn $ map charTransform string

-- |
-- A command-line interface parser that will make you smile
-- https://hackage.haskell.org/package/docopt
-- https://github.com/docopt/docopt.hs
--
-- $ stack install doctopt
--
-- $ stack DocOptParsingArgsSample.hs echo "hello world"
-- hello world
--
-- $ stack DocOptParsingArgsSample.hs echo -c "hello world"
-- HELLO WORLD
--
-- $ stack DocOptParsingArgsSample.hs cat ./DocOptParsingArgsSample.hs

-----------------------------------------------------------------------------------
