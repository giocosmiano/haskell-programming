{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingExercises where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- parser for semantic versions as defined by http://semver.org/
-- After making a working parser, write an Ord instance for the SemVer type that
-- obeys the specification outlined on the SemVer website.
-----------------------------------------------------------------------------------
data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Eq, Show, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch 
            deriving (Eq, Show, Ord)
-- data SemVer = SemVer Major Minor Patch Release Metadata
--             deriving (Eq, Show, Ord)

-- e.g.
-- parseString parseSemVer mempty "1.0.0-x.7.z.92"
-- parseString parseSemVer mempty "1.0.0-gamma+002"
-- parseString parseSemVer mempty "1.0.0-beta+oof.sha.41af286"
parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
--   char '-'
--   release <- string
--   skipMany (noneOf alphaNum)
  return $ SemVer major minor patch


-- Skip end of line and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-----------------------------------------------------------------------------------

