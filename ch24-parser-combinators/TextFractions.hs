{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

-----------------------------------------------------------------------------------
-- see details about `OverloadedStrings`
-- https://www.schoolofhaskell.com/user/kseo/overloaded-string-literals
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions
-----------------------------------------------------------------------------------

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

-----------------------------------------------------------------------------------
-- char    :: CharParsing  m => Char   -> m Char
-- string  :: CharParsing  m => String -> m String
-- decimal :: TokenParsing m =>           m Integer

-- parseString :: Parser a
--             -> Text.Trifecta.Delta.Delta
--             -> String
--             -> Result a
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- this does throws a divide by 0 exception error
-----------------------------------------------------------------------------------

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

-----------------------------------------------------------------------------------
-- this handles the divide by 0 exception error
-----------------------------------------------------------------------------------

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' = parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

-----------------------------------------------------------------------------------

-- Exercise
-- e.g.
-- parseString (integer >> eof) mempty "123" -> Success ()
--
-- You may have already deduced why it returns () as a Success result here; it’s
-- consumed all the input but there is no result to return from having done so.
-- The result `Success ()` tells you the parse was successful and consumed the entire
-- input, so there’s nothing to return.

-- What we want you to try now is rewriting the example so it returns the integer that
-- it parsed instead of `Success ()`. It should return the integer successfully when it
-- receives an input with an integer followed by an EOF and fail in all other cases:

-- e.g.
-- parseString parsePractice mempty "123"    -> Success 123
-- parseString parsePractice mempty "123abc" ->
-- Failure (interactive):1:1: error: expected: digit; 123abc<EOF>

-- using the `do` notation
-- parseString (do val <- integer; _ <- eof; return val) mempty "123" -> Success 123
parsePractice :: Parser Integer
parsePractice = do
  val  <- integer
  _    <- eof -- ignore the `eof`
  return val

-----------------------------------------------------------------------------------
