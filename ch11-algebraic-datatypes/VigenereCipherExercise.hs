module VigenereCipherExercise where

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

shift :: Char -> Char -> Char
shift key str
   | isUpper str = chr $ newDec + getDecOfA str
   | isLower str = chr $ newDec + getDecOfA str
   | otherwise   = str
   where decKey  = charToDec key
         decStr  = charToDec str
         newDec  = mod (decKey + decStr) 26

-- e.g. key       = "ALLY"
--    , plainText = "MEET AT DAWN"
--   -> resultKey = "ALLY AL LYAL"
-- mapKey :: key -> plainText -> resultKey
mapKey :: String -> String -> String
mapKey key str =
   let pairStr = zip [1..] str
   in  map (\(i, s) -> if (isLower s || isUpper s) then replaceKeyChar i key str else s) pairStr

replaceKeyChar :: Int -> String -> String -> Char
replaceKeyChar idx key str
   | newIdx == 0 = key !! (lenKey - 1)
   | otherwise   = key !! (newIdx - 1)
   where lenKey  = length key
         curLen  = length $ filter (\x -> isLower x || isUpper x) $ take idx str
         newIdx  = mod curLen lenKey

-- Vigenere ciphering only the 26-letters of the alphabet
-- see https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher
-- cipher :: key -> plainText -> cipherText
cipher :: String -> String -> String
cipher  _ [] = []
cipher []  _ = []
cipher key str =
   let keyStr    = mapKey key str
       pairStr   = zip keyStr str
       cipherStr = map (\(k, s) -> shift k s) pairStr
   in  cipherStr

-- e.g.
-- cipher "ALLY" "MEET AT DAWN" -> "MPPR AE OYWY"
--    , key        = "ALLY"
--    , plainText  = "MEET AT DAWN"
--   -> cipherText = "MPPR AE OYWY"

-- cipher "ALLY" "Meet At Dawn" -> "Mppr Ae Oywy"
--    , key        = "ALLY"
--    , plainText  = "Meet At Dawn"
--   -> cipherText = "Mppr Ae Oywy"
