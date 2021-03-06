{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingSemVerExercises where

import Control.Applicative
import Control.Monad
import Data.Char
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

data SemVer = SemVer Major Minor Patch Release Metadata
            deriving (Eq, Show)

-----------------------------------------------------------------------------------

-- e.g.
-- SemVer 2 1 1 [] [] >  SemVer 1 1 0 [] [] -> True
-- SemVer 2 1 1 [] [] >  SemVer 3 1 0 [] [] -> False
-- SemVer 2 2 1 [] [] >  SemVer 2 1 0 [] [] -> True
-- SemVer 2 1 1 [] [] >  SemVer 2 2 0 [] [] -> False
-- SemVer 2 2 1 [] [] >  SemVer 2 2 0 [] [] -> True
-- SemVer 2 2 1 [] [] >  SemVer 2 2 2 [] [] -> False
-- SemVer 2 2 1 [] [] == SemVer 2 2 1 [] [] -> True
instance Ord SemVer where
  (SemVer maj' min' pat' _ _)
    `compare`
       (SemVer maj'' min'' pat'' _ _)
         | (maj' `compare` maj'') /= EQ = (maj' `compare` maj'')
         | (min' `compare` min'') /= EQ = (min' `compare` min'')
         | otherwise                    = (pat' `compare` pat'')

-- e.g.
-- parseString parseSemVer mempty "2.1.1"                      -> Success (SemVer 2 1 1 [] [])
-- parseString parseSemVer mempty "1.0.0-x.7.z.92"             -> Success (SemVer 1 0 0 [NOSS "x",NOSI 7,NOSS "z",NOSI 92] [])
-- parseString parseSemVer mempty "1.0.0-gamma+002"            -> Success (SemVer 1 0 0 [NOSS "gamma"] [NOSI 2])
-- parseString parseSemVer mempty "1.0.0-beta+oof.sha.41af286" ->
-- Success (SemVer 1 0 0 [NOSS "beta"] [NOSS "oof",NOSS "sha",NOSS "41af286"])
parseSemVer :: Parser SemVer
parseSemVer = do
  major    <- integer
  _        <- char '.'
  minor    <- integer
  _        <- char '.'
  patch    <- integer
  _        <- many (oneOf "-")
  releases <- parseNbrOrStr
  _        <- many (oneOf "+")
  metadata <- parseNbrOrStr
  return $ SemVer major minor patch releases metadata

-----------------------------------------------------------------------------------

parseNbrOrStr :: Parser [NumberOrString]
parseNbrOrStr = sepBy parseAlphaNum (symbol ".")

-----------------------------------------------------------------------------------

parseAlphaNum :: Parser NumberOrString
parseAlphaNum = do
  alpNbr <- some (noneOf ".+")
  let val = if all isDigit alpNbr then (NOSI (read alpNbr :: Integer)) else NOSS alpNbr
  return val

-----------------------------------------------------------------------------------
