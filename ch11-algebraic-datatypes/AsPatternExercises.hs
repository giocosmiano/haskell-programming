module AsPatternExercises where

import Data.List.Split (split, endsWith)
import Data.Char (toUpper, isAlpha)
import Data.String (words)

-- e.g.
-- isSubseqOf "blah" "blahwoot" -> True
-- isSubseqOf "blah" "wootblah" -> True
-- isSubseqOf "blah" "wboloath" -> True
-- isSubseqOf "blah" "wootbla"  -> False
-- isSubseqOf "blah" "halbwoot" -> False
-- isSubseqOf "blah" "blawhoot" -> True
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf key@(x:xs) (y:ys)
   | x == y    = isSubseqOf xs  ys
   | otherwise = isSubseqOf key ys

-- e.g. capitalizeWord " hello" -> " Hello"
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
   | isAlpha x = toUpper x : xs
   | otherwise = x : capitalizeWord xs

-- e.g. capitalizeWords "hello world" -> [("hello","Hello"),("world","World")]
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w -> (w, capitalizeWord w)) . words

-- e.g. capitalizeParagraph " this is. just a. test." -> " This is. Just a. Test."
capitalizeParagraph :: String -> String
capitalizeParagraph = concat . map (\s -> capitalizeWord s) . split (endsWith ".")

{-
capitalizeParagraph' :: String -> String
capitalizeParagraph' [] = []
capitalizeParagraph' xs =
   let sentence = split (endsWith ".") xs
       capSentence = map (\s -> capitalizeWord s) sentence
   in  concat capSentence
-}
