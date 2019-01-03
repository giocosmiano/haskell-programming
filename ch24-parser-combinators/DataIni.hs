{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Test.Hspec
import Text.Trifecta

--
-- Raw string literals for Haskell.
-- https://github.com/23Skidoo/raw-strings-qq
-- https://www.schoolofhaskell.com/user/dshevchenko/cookbook/raw-string
--
-- Prelude> stack install raw-strings-qq
-- http://hackage.haskell.org/package/raw-strings-qq
import Text.RawString.QQ

-----------------------------------------------------------------------------------

headerEx :: ByteString
headerEx = "[blah]"

-- "[blah]" -> Section "blah"
newtype Header = Header String
               deriving (Eq, Ord, Show)

-- these operators mean the brackets will be parsed and then discarded but the `p` will remain as our result
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-----------------------------------------------------------------------------------
-- see for details
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html
-----------------------------------------------------------------------------------
-- all these functions are in Control.Applicative
-- see the definitions below
--
-- (<*) :: Applicative f => f a -> f b -> f a
-- Sequence actions, discarding the value of the second argument.

-- (*>) :: Applicative f => f a -> f b -> f b
-- Sequence actions, discarding the value of the first argument.
-- This is the same as (>>) monad operator
-----------------------------------------------------------------------------------

type Name = String
type Value = String
type Assignments = Map Name Value

assignmentEx :: ByteString
assignmentEx = "woot=1"

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL -- important!
  return (name, val)

-- Skip end of line and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-----------------------------------------------------------------------------------

parseAssignment' :: Parser (Name, Value)
parseAssignment' = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  return (name, val)

-----------------------------------------------------------------------------------

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n \n;hah"

-- | Skip comments starting at the
-- beginning of the line.
skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)

-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------


