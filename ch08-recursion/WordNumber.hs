module WordNumber where

import Data.Char (digitToInt)
import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n
   | n == 0 = "zero"
   | n == 1 = "one"
   | n == 2 = "two"
   | n == 3 = "three"
   | n == 4 = "four"
   | n == 5 = "five"
   | n == 6 = "six"
   | n == 7 = "seven"
   | n == 8 = "eight"
   | n == 9 = "nine"
   | otherwise = wordNumber' n

digits :: Int -> [Int]
digits = map digitToInt . show

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

wordNumber' :: Int -> String
wordNumber' = intercalate "-" . map digitToWord . map digitToInt . show

-- e.g.
-- wordNumber 1234567 -> "one-two-three-four-five-six-seven"
-- wordNumber' 1234567 -> "one-two-three-four-five-six-seven"
