{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParsingIPv4IPv6Exercises where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Int
import Data.Word
import Text.Trifecta
import Text.RawString.QQ

-----------------------------------------------------------------------------------
-- parser for IPv4 address https://en.wikipedia.org/wiki/IPv4
-----------------------------------------------------------------------------------

type Octet  = Word32

data IPv4 = IPv4 Octet Octet Octet Octet
          deriving (Eq, Ord, Show)

data IPAddress = IPAddress Word32
               deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------------

-- e.g.
-- parseString parseIPv4Address mempty "255.255.255.255" -> Success (IPAddress 4294967295)
-- parseString parseIPv4Address mempty "198.179.137.194" -> Success (IPAddress 3333654978)
-- parseString parseIPv4Address mempty "172.16.254.1"    -> Success (IPAddress 2886794753)
-- parseString parseIPv4Address mempty "204.120.0.15"    -> Success (IPAddress 3430416399)
parseIPv4Address :: Parser IPAddress
parseIPv4Address = do
  skipOptional skipWhitespace
  o1 <- parseOctet
  _  <- char '.'
  o2 <- parseOctet
  _  <- char '.'
  o3 <- parseOctet
  _  <- char '.'
  o4 <- parseOctet
  skipEOL
  return $ ipv4ToIPAddress $ IPv4 o1 o2 o3 o4

parseOctet :: Parser Octet
parseOctet = do
  v <- try (try (count 3 octDigit) <|> count 2 octDigit) <|> count 1 octDigit
  return $ read v

ipv4ToIPAddress :: IPv4 -> IPAddress
ipv4ToIPAddress (IPv4 o1 o2 o3 o4) =
  let w1 = o1 * (256 ^3)
      w2 = o2 * (256 ^2)
      w3 = o3 * (256 ^1)
      w4 = o4
  in  IPAddress $ w1 + w2 + w3 + w4

-----------------------------------------------------------------------------------
-- TODO: very messy and needs cleaning up, will revisit to implement ipv6 to word64
-- parser for IPv6 address https://en.wikipedia.org/wiki/IPv6
-----------------------------------------------------------------------------------

-- Per discussion -> http://answers.google.com/answers/threadview/id/770645.html
-- The use of '::' indicates one or more groups of 16 bits of zeros. The '::' can only appear
-- once in an address.  The '::' can also be used to compress leading or trailing zeros in an address.

-----------------------------------------------------------------------------------

type HexDigits = String
type IPv6Abbr  = Int8

data IPv6 = IPv6 HexDigits HexDigits HexDigits HexDigits
                 HexDigits HexDigits HexDigits HexDigits
          deriving (Eq, Ord)

instance Show IPv6 where
  show (IPv6 h1 h2 h3 h4 h5 h6 h7 h8) =
    show $ h1 ++ ":" ++ h2 ++ ":" ++ h3 ++ ":" ++ h4 ++ ":" ++
           h5 ++ ":" ++ h6 ++ ":" ++ h7 ++ ":" ++ h8

data IPAddress6 = IPAddress6 Word64 Word64
                deriving (Eq, Ord, Show)

-- e.g.
-- parseString parseIPv6Address mempty "0:0:0:0:0:ffff:ac10:fe01"                ->
-- parseString parseIPv6Address mempty "0:0:0:0:0:ffff:cc78:f"                   ->
-- parseString parseIPv6Address mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329" ->
-- parseString parseIPv6Address mempty "2001:DB8::8:800:200C:417A"               ->
-- parseString parseIPv6Address mempty "FE80::0202:B3FF:FE1E:8329"               ->

-- are these valid ipv6 formats???
-- 1::0:3
-- 1:0::3
-- ::0:3
-- 1:0::
parseIPv6Address' :: Parser [Maybe HexDigits]
parseIPv6Address' = do
  skipOptional skipWhitespace
  h1 <- optional parseHexDigits
  _  <- optional $ char ':'
  h2 <- optional parseHexDigits
  _  <- optional $ char ':'
  h3 <- optional parseHexDigits
  _  <- optional $ char ':'
  h4 <- optional parseHexDigits
  _  <- optional $ char ':'
  h5 <- optional parseHexDigits
  _  <- optional $ char ':'
  h6 <- optional parseHexDigits
  _  <- optional $ char ':'
  h7 <- optional parseHexDigits
  _  <- optional $ char ':'
  h8 <- optional parseHexDigits
  ab <- optional parseIPv6Abbr
  skipEOL
  return [h1, h2, h3, h4, h5, h6, h7, h8]

parseIPv6Address :: Parser IPv6
parseIPv6Address = do
  skipOptional skipWhitespace
  h1 <- optional parseHexDigits
  _  <- optional $ char ':'
  h2 <- optional parseHexDigits
  _  <- optional $ char ':'
  h3 <- optional parseHexDigits
  _  <- optional $ char ':'
  h4 <- optional parseHexDigits
  _  <- optional $ char ':'
  h5 <- optional parseHexDigits
  _  <- optional $ char ':'
  h6 <- optional parseHexDigits
  _  <- optional $ char ':'
  h7 <- optional parseHexDigits
  _  <- optional $ char ':'
  h8 <- optional parseHexDigits
  ab <- optional parseIPv6Abbr
  let ipv6List  = [h1, h2, h3, h4, h5, h6, h7, h8]
      ctrIpv6   = countEmptyIpv6 ipv6List
      tmpInIpv6 = fillInIpv6 ctrIpv6 ipv6List
      newIpv6   = take 8 $ foldr (\x b -> [getHexDigit x] ++ b) [] tmpInIpv6
  skipEOL
  return $ generateIPv6 newIpv6

parseHexDigits :: Parser HexDigits
parseHexDigits = do
  v <- try (try (try (count 4 hexDigit) <|> count 3 hexDigit) <|> count 2 hexDigit) <|> count 1 hexDigit
  return v

parseIPv6Abbr :: Parser Int8
parseIPv6Abbr = do
  _ <- char '/'
  v <- try (count 2 octDigit) <|> count 1 octDigit
  return $ read v

-----------------------------------------------------------------------------------
-- NOTE *** this is the tricky part mentioned in the book
-- Per discussion -> http://answers.google.com/answers/threadview/id/770645.html
-- The use of '::' indicates one or more groups of 16 bits of zeros. The '::' can only appear
-- once in an address.  The '::' can also be used to compress leading or trailing zeros in an address.
-----------------------------------------------------------------------------------
-- `countEmptyIpv6` is used to determine the counts of empty elements that will be move from the tail
-- e.g.

-- resulting to 2 counts from tail that will be moved into the middle
-- "FE80::0202:B3FF:FE1E:8329"
-- will be parsed to           -> [Just "FE80",Nothing,Just "0202",Just "B3FF",Just "FE1E",Just "8329",Nothing,Nothing]
-- which should be turned into -> [Just "FE80",Nothing,Nothing,Nothing,Just "0202",Just "B3FF",Just "FE1E",Just "8329"]

-- resulting to 2 counts from tail that will be moved into the after the 1st Nothing
-- "::0202:B3FF:FE1E:8329"
-- will be parsed to           -> [Nothing,Nothing,Just "0202",Just "B3FF",Just "FE1E",Just "8329",Nothing,Nothing]
-- which should be turned into -> [Nothing,Nothing,Nothing,Nothing,Just "0202",Just "B3FF",Just "FE1E",Just "8329"]
countEmptyIpv6 :: [Maybe HexDigits] -> Int
countEmptyIpv6 hexDigits =
  let emptyTailCtr = length $ takeWhile (\x -> isNothing x) $ reverse hexDigits
      emptyCtr     = length $ filter (\x -> isNothing x) hexDigits
  in  if emptyTailCtr > 0 then emptyTailCtr else emptyCtr

fillInIpv6 :: Int -> [Maybe HexDigits] -> [Maybe HexDigits]
fillInIpv6 ctr [] = []
fillInIpv6 ctr (x:xs) =
  let emptyList = if ctr > 0 && isNothing x then replicate ctr Nothing else []
      newCtr    = if length emptyList > 0 then 0 else ctr
  in  [x] ++ emptyList ++ fillInIpv6 newCtr xs

getHexDigit :: Maybe HexDigits -> HexDigits
getHexDigit Nothing  = "0"
getHexDigit (Just x) = x

getIPv6Abbr :: Maybe Int8 -> Int8
getIPv6Abbr Nothing  = 0
getIPv6Abbr (Just x) = x

generateIPv6 :: [HexDigits] -> IPv6
generateIPv6 xs =
  let h1 = xs !! 0
      h2 = xs !! 1
      h3 = xs !! 2
      h4 = xs !! 3
      h5 = xs !! 4
      h6 = xs !! 5
      h7 = xs !! 6
      h8 = xs !! 7
  in  IPv6 h1 h2 h3 h4 h5 h6 h7 h8

-----------------------------------------------------------------------------------

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

-----------------------------------------------------------------------------------
