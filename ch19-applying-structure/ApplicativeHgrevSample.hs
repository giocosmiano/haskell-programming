{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApplicativeHgrevSample where

import           Data.Monoid          ((<>))
import           Data.Text            (Text, pack)
import           Development.HgRev.TH (defFormat, hgRevStateTH, jsonFormat)
import           Options.Applicative  (Parser, ParserInfo, execParser, fullDesc,
                                        help, helper, info, infoOption, long,
                                        progDesc, short)

-----------------------------------------------------------------------------------

-- NOTE: make sure `cabal` is up-to-date
-- http://hackage.haskell.org/package/cabal-install
-- Prelude> cabal update
--
-- Compile Mercurial (hg) version info into Haskell code
-- https://github.com/bitnomial/hgrev
-- Prelude> cabal install hgrev

-----------------------------------------------------------------------------------

main :: IO ()
main = execParser parserInfo >> return ()

verSwitch :: Parser (a -> a)
verSwitch =
    infoOption ("HG rev: " <> $(hgRevStateTH defFormat))
    $  long "version"
    <> short 'v'
    <> help "Display version information"

jsonSwitch :: Parser (a -> a)
jsonSwitch =
    infoOption $(hgRevStateTH jsonFormat)
    $  long "json"
    <> short 'J'
    <> help "Display JSON version information"

parserInfo :: ParserInfo (a -> a)
parserInfo = info (helper <*> verSwitch <* jsonSwitch) fullDesc
