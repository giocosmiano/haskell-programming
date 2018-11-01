module GenerateWordList where

import System.Random (randomRIO)

type WordList = [String]

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
   dict <- readFile "data/dict.txt"
   return (lines dict)

gameWords :: IO WordList
gameWords = do
   aw <- allWords
   return (filter gameLength aw)
      where gameLength w =
             let l  = length (w :: String)
             in  l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
   randomIndex <- randomRIO (1, length wl)
   return $ wl !! (randomIndex - 1)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
