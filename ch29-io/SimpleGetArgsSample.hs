module SimpleGetArgsSample where

import System.Environment
import System.Exit

-----------------------------------------------------------------------------------

tac  = unlines . reverse . lines

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

-----------------------------------------------------------------------------------

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

-----------------------------------------------------------------------------------


main = getArgs >>= parse >>= putStr . tac

-- |
-- Command line argument handling
-- https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling
--
-- $ stack SimpleGetArgsSample.hs -h
-- Usage: tac [-vh] [file ..]
--
-- $ stack SimpleGetArgsSample.hs -v
-- Haskell tac 0.1
--
-- $ stack SimpleGetArgsSample.hs "SimpleGetArgsSample.hs"

-----------------------------------------------------------------------------------
