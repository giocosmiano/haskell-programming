{-# LANGUAGE QuasiQuotes #-}

module Quasimodo where

--
-- Raw string literals for Haskell.
-- https://github.com/23Skidoo/raw-strings-qq
--
-- Prelude> stack install raw-strings-qq
-- http://hackage.haskell.org/package/raw-strings-qq
import Text.RawString.QQ

-----------------------------------------------------------------------------------

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

main1 :: IO ()
main1 = putStr eitherOr

-----------------------------------------------------------------------------------
-- e.g.
-- https://www.schoolofhaskell.com/user/dshevchenko/cookbook/raw-string
-----------------------------------------------------------------------------------

helpInfo :: String
helpInfo = [r|

This is raw string, "strange
  -- Very strange
  {-
  '"Yes, very strange\ string"
  -}

But in some cases
                   it's
            very convenient...
|]

main2 :: IO ()
main2 = putStr helpInfo

-----------------------------------------------------------------------------------
