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

import Text.Trifecta

--
-- A Testing Framework for Haskell
-- https://github.com/hspec/hspec
-- http://hspec.github.io/
--
-- Prelude> stack install hspec
-- http://hackage.haskell.org/package/hspec
import Test.Hspec

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

-- these operators mean the brackets will be parsed and then discarded but the `p` will remain as our result
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

-----------------------------------------------------------------------------------
-- see for details
-- http://hackage.haskell.org/package/parsers
-- http://hackage.haskell.org/package/parsers-0.12.9/docs/Text-Parser-Char.html
-- http://hackage.haskell.org/package/parsers-0.12.9/docs/Text-Parser-Combinators.html
-----------------------------------------------------------------------------------
-- char    :: CharParsing  m => Char   -> m Char
-- string  :: CharParsing  m => String -> m String
-- decimal :: TokenParsing m =>           m Integer
-- letter  :: CharParsing  m => m Char
-----------------------------------------------------------------------------------

-- We’ve combined two parsers in order to parse a Header.
-- e.g.
-- some letter

-- e.g.
-- Prelude> :t some letter            -> some letter :: CharParsing f => f [Char]
-- Prelude> :t Header <$> some letter -> Header <$> some letter :: CharParsing f => f Header
-- Prelude> slp = Header <$> some letter :: Parser Header
parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

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

-- e.g.
-- with skipEOL
-- Prelude> s = "key=value\nblah=123"
-- Prelude> spa = some parseAssignment
-- Prelude> parseString spa mempty s
-- Success [("key","value"),("blah","123")]
-- Prelude> d = "key=value\n\n\ntest=data"
-- Prelude> parseString spa mempty d
-- Success [("key","value"),("test","data")]
-----------------------------------------------------------------------------------

parseAssignment' :: Parser (Name, Value)
parseAssignment' = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  return (name, val)

-- e.g.
-- without skipEOL
-- Prelude> s = "key=value\nblah=123"
-- Prelude> spa' = some parseAssignment'
-- Prelude> parseString spa' mempty s
-- Success [("key","value")]

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

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

-----------------------------------------------------------------------------------

-- e.g.
-- parseByteString parseSection mempty sectionEx
-- Success (Section (Header "states") (fromList [("Chris","Texas")]))

data Section = Section Header Assignments
             deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
               deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

-----------------------------------------------------------------------------------

-- e.g.
-- parseByteString parseIni mempty sectionEx
-- Success (Config (fromList [(Header "states",fromList [("Chris","Texas")])]))

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

-----------------------------------------------------------------------------------

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-----------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do

  -- assignmentEx = "woot=1"
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString
              parseAssignment
              mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  -- headerEx = "[blah]"
  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader
              mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  -- i = "; woot\n[blah]"
  describe "Comment parsing" $
    it "Skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  -- sectionEx = "; ignore me\n[states]\nChris=Texas"
  describe "Section parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString parseSection
              mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected' = Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'

{-
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]
-}
  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m = parseByteString parseIni
              mempty sectionEx''
          r' = maybeSuccess m
          sectionValues =
            M.fromList
            [ ("alias", "claw")
            , ("host", "wikipedia.org")]
          whatIsItValues =
            M.fromList
            [("red", "intoothandclaw")]
          expected' =
            Just (Config
              (M.fromList
              [ (Header "section"
              , sectionValues)
              , (Header "whatisit"
              , whatIsItValues)]))
      print m
      r' `shouldBe` expected'

-----------------------------------------------------------------------------------

