{-# LANGUAGE OverloadedStrings #-}

module BackTrackingFailure where

import Control.Applicative
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

--
-- Fast combinator parsing for bytestrings and text
-- https://github.com/bos/attoparsec
--
-- Prelude> stack install attoparsec
-- http://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)

-----------------------------------------------------------------------------------

-- helper function to run a trifecta parser and print the result
trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

-- helper function to run a parsec parser and print the result
parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

-- helper function for attoparsec parser and print the result
attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

-----------------------------------------------------------------------------------

noBackParse :: (Monad f, CharParsing f) => f Char
noBackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

-----------------------------------------------------------------------------------

main :: IO ()
main = do

  -- trifecta
  trifP noBackParse "13"
  trifP tryParse "13"

  -- parsec
  parsecP noBackParse "13"
  parsecP tryParse "13"

  -- attoparsec
  attoP noBackParse "13"
  attoP tryParse "13"

-----------------------------------------------------------------------------------
