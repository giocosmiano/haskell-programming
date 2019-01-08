{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingIPv4IPv6Exercises where

import Control.Applicative
import Control.Monad
import Data.Word
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- parser for IPv4 address https://en.wikipedia.org/wiki/IPv4
-----------------------------------------------------------------------------------

type Octet  = Word32
type Octet1 = Word32
type Octet2 = Word32
type Octet3 = Word32
type Octet4 = Word32

data IPv4 = IPv4 Octet1 Octet2 Octet3 Octet4
          deriving (Eq, Ord, Show)

data IPAddress = IPAddress Word32
               deriving (Eq, Ord, Show)

data IPAddress6 = IPAddress6 Word64 Word64
                deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------------

-- e.g.
-- parseString parseIPv4Address mempty "255.255.255.255" -> Success (IPAddress 4294967295)
-- parseString parseIPv4Address mempty "198.179.137.194" -> Success (IPAddress 3333654978)
-- parseString parseIPv4Address mempty "172.16.254.1"    -> Success (IPAddress 2886794753)
-- parseString parseIPv4Address mempty "204.120.0.15"    -> Success (IPAddress 3430416399)
parseIPv4Address :: Parser IPAddress
parseIPv4Address = do
  octet1 <- parseOctet
  _      <- char '.'
  octet2 <- parseOctet
  _      <- char '.'
  octet3 <- parseOctet
  _      <- char '.'
  octet4 <- parseOctet
  skipEOL
  return $ ipv4ToIPAddress $ IPv4 octet1 octet2 octet3 octet4

parseOctet :: Parser Octet
parseOctet = do
  skipOptional skipWhitespace
  v <- try ((try (count 3 digit) <|> count 2 digit)) <|> count 1 digit
  return $ read v

ipv4ToIPAddress :: IPv4 -> IPAddress
ipv4ToIPAddress (IPv4 o1 o2 o3 o4) =
  let w1 = o1 * (256 ^3)
      w2 = o2 * (256 ^2)
      w3 = o3 * (256 ^1)
      w4 = o4
  in  IPAddress $ w1 + w2 + w3 + w4

-----------------------------------------------------------------------------------

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

-----------------------------------------------------------------------------------
