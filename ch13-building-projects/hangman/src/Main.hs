module Main where

import Data.Char (toLower)

import GenerateWordList
import PlayTheGame

main :: IO ()
main = do
   word <- randomWord'
   let puzzle = freshPuzzle (fmap toLower word)
   runGame puzzle
