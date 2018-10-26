module CaesarCipher where

import Data.Char (chr, ord, isUpper, isLower)

-- get the decimal value of the character
-- https://en.wikipedia.org/wiki/List_of_Unicode_characters
getDecOfA :: Char -> Int
getDecOfA x
   | isUpper x = 65 -- A=65
   | isLower x = 97 -- a=97
   | otherwise = 0

charToDec :: Char -> Int
charToDec x = ord x - getDecOfA x

shift :: Int -> Char -> Char
shift n x
   | isUpper x = chr $ newDec + getDecOfA x
   | isLower x = chr $ newDec + getDecOfA x
   | otherwise = x
   where decimal = charToDec x
         newDec  = mod (decimal + n) 26

-- Ciphering only the 26-letters of the alphabet,
-- with +Int as right-shift and -Int as left-shift
caesar :: Int -> String -> String
caesar n [] = []
caesar n (x:xs) = shift n x : caesar n xs

unCaesar :: Int -> String -> String
unCaesar n xs = caesar (negate n) xs

-- e.g.
-- caesar   2  "abc.xyz.$ABC.XYZ" -> "cde.zab.$CDE.ZAB"
-- caesar (-2) "abc.xyz.$ABC.XYZ" -> "yza.vwx.$YZA.VWX"

-- unCaesar   2  "cde.zab.$CDE.ZAB" -> "abc.xyz.$ABC.XYZ"
-- unCaesar (-2) "yza.vwx.$YZA.VWX" -> "abc.xyz.$ABC.XYZ"

