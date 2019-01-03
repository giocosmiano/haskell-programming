{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
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

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

eitherOrGood :: String
eitherOrGood = [r|
123
abc
456
def|]

eitherOrBad :: String
eitherOrBad = [r|
123
abc
456
def
|]

-----------------------------------------------------------------------------------
-- all these functions are in Control.Applicative
-- see the definitions below
--
-- (<|>) :: Alternative f => f a -> f a -> f a
-- some  :: Alternative f => f a -> f [a]
-- many  :: Alternative f => f a -> f [a]
-- empty :: Alternative f => f a
-----------------------------------------------------------------------------------

-- We can read <|> as being an or, or disjunction, of our two parsers;
-- `many` is zero or more
-- `some` is one or more.

-----------------------------------------------------------------------------------

-- skipMany :: Parsing m => m a -> m ()
-- oneOf    :: CharParsing m => [Char] -> m Char

-----------------------------------------------------------------------------------

-- e.g.
-- parseString parseNos mempty eitherOrBad         -> Success (Left 123)
-- parseString (some parseNos) mempty eitherOrBad  ->
-- Failure (interactive):1:2: error: unexpected EOF; <EOF>

-- The issue here is that while `skipMany` lets us skip zero or more times, it means
-- we started the next run of the parser before we hit EOF. This means it expects us
-- to match an integer or some letters after having seen the newline character after “def”.

-- To fix it, remove the `EOF` from the last element of raw string. See eitherOrBad vs eitherOrGood
-- parseString parseNos mempty eitherOrGood        -> Success (Left 123)
-- parseString (some parseNos) mempty eitherOrGood -> Success [Left 123,Right "abc",Left 456,Right "def"]

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

-----------------------------------------------------------------------------------

-- OR
-- ignore the EOF from both ends

-- e.g.
-- parseString parseNos2 mempty eitherOrBad         -> Success (Left 123)
-- parseString (some parseNos2) mempty eitherOrBad  -> Success [Left 123,Right "abc",Left 456,Right "def"]
-- parseString parseNos2 mempty eitherOrGood        -> Success (Left 123)
-- parseString (some parseNos2) mempty eitherOrGood -> Success [Left 123,Right "abc",Left 456,Right "def"]

parseNos2 :: Parser NumberOrString
parseNos2 = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

-----------------------------------------------------------------------------------

-- OR
-- use `token` parsers

-- e.g.
-- parseString (some (token parseNos3)) mempty eitherOrBad  -> Success [Left 123,Right "abc",Left 456,Right "def"]
-- parseString (some (token parseNos3)) mempty eitherOrGood -> Success [Left 123,Right "abc",Left 456,Right "def"]

parseNos3 :: Parser NumberOrString
parseNos3 = skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

-----------------------------------------------------------------------------------

main = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c

main2 = do
  let p f i = parseString f mempty i
  print $ p parseNos eitherOrGood

-----------------------------------------------------------------------------------

{-

class Applicative f => Alternative f where

-- | The identity of '<|>'
  empty :: f a

-- | An associative binary operation
  (<|>) :: f a -> f a -> f a

-- | One or more.
  some :: f a -> f [a]
  some v = some_v
    where many_v = some_v <|> pure []
          some_v = (fmap (:) v) <*> many_v

-- | Zero or more.
  many :: f a -> f [a]
  many v = many_v
    where many_v = some_v <|> pure []
          some_v = (fmap (:) v) <*> many_v

-}

-----------------------------------------------------------------------------------
