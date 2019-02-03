{-# LANGUAGE DeriveDataTypeable #-}

module CmdArgsSample where

import System.Console.CmdArgs

-----------------------------------------------------------------------------------

data Sample = Sample {hello :: String, world :: String} deriving (Show, Data, Typeable)

sample = Sample {
           hello = def &= help "hello argument" &= opt "hi hello "
         , world = def &= help "world argument" &= opt "other world"
         } &= summary "CmdArgsSample v1"

-----------------------------------------------------------------------------------

main = print =<< cmdArgs sample

-- |
-- Command line argument processing
-- http://hackage.haskell.org/package/cmdargs
-- https://github.com/ndmitchell/cmdargs
--
-- $ stack install cmdargs
--
-- $ stack CmdArgsSample.hs --hello=gio --world=myWorld
-- Sample {hello = "gio", world = "myWorld"}
--
-- $ stack CmdArgsSample.hs
-- Sample {hello = "", world = ""}
--
-- $ stack CmdArgsSample.hs --help
-- CmdArgsSample v1
--
-- sample [OPTIONS]
--
-- Common flags:
--   -h --hello[=ITEM]     hello argument
--   -w --world[=ITEM]     world argument
--   -? --help             Display help message
--   -V --version          Print version information
--      --numeric-version  Print just the version number

-----------------------------------------------------------------------------------
