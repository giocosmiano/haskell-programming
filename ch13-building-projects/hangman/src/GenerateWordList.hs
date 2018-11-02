module GenerateWordList where

import Data.List (filter, isSuffixOf)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
   dict <- readFile "data/dict.txt"
   return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
   (WordList aw) <- allWords
   return $ WordList (filter gameLength $ filter (not . isSuffixOf "'s") aw) -- removing words that ends with "'s"
      where gameLength w =
             let l  = length (w :: String)
             in  l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
   randomIndex <- randomRIO (0, (length wl - 1))
   return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
