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
   | isLower str || isUpper str = chr $ newDec + getDecOfA str
   | otherwise  = str
   where decKey = charToDec key
         decStr = charToDec str
         newDec = mod (decKey + decStr) 26

-- e.g. key       = "ALLY"
--    , plainText = "MEET AT DAWN"
--   -> resultKey = "ALLY AL LYAL"
-- replaceKeyChar :: key -> plainText -> resultKey
replaceKeyChar :: String -> String -> String
replaceKeyChar [] _ = []
replaceKeyChar _ [] = []
replaceKeyChar key@(x:xs) (y:ys)
   | isLower y || isUpper y = x : replaceKeyChar xs  ys
   | otherwise              = y : replaceKeyChar key ys

-- e.g.
--    , key       = "ALLY"
--    , plainText = "MEET AT DAWN"
--   -> resultKey = "ALLY AL LYAL"
-- mapKey "ALLY" "MEET AT DAWN" -> "ALLY AL LYAL"
-- mapKey :: key -> plainText -> resultKey
mapKey :: String -> String -> String
mapKey key str   =
   let nbrCopies = quot (length str) (length key)
       dupedKeys = concat $ replicate (nbrCopies + 1) key
   in  replaceKeyChar dupedKeys str

-- Vigenere ciphering only the 26-letters of the alphabet
-- see https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher
-- cipher :: key -> plainText -> cipherText
cipher :: String -> String -> String
cipher  _ [] = []
cipher []  _ = []
cipher key str =
   let keyStr    = mapKey key str
       pairStr   = zip keyStr str
   in  map (\(k, s) -> shift k s) pairStr

-- e.g.
-- cipher "ALLY" "MEET AT DAWN" -> "MPPR AE OYWY"
--    , key        = "ALLY"
--    , plainText  = "MEET AT DAWN"
--   -> cipherText = "MPPR AE OYWY"

-- cipher "ALLY" "Meet At Dawn" -> "Mppr Ae Oywy"
--    , key        = "ALLY"
--    , plainText  = "Meet At Dawn"
--   -> cipherText = "Mppr Ae Oywy"
