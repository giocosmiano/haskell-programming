module FizzBuzzExercise where

import Control.Monad
import Control.Monad.Trans.State

--
-- Difference Lists in Haskell
-- https://github.com/spl/dlist
--
-- Prelude> stack install dlist
-- http://hackage.haskell.org/package/dlist
--
-- for further reading
-- https://en.wikipedia.org/wiki/Difference_list
-- https://wiki.haskell.org/Difference_list
-- https://stackoverflow.com/questions/3352418/what-is-a-dlist
import qualified Data.DList as DL

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

-----------------------------------------------------------------------------------

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-----------------------------------------------------------------------------------

fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
  let dlist = execState (mapM_ addResult' list) DL.empty
  -- convert back to normal list
  in  DL.apply dlist []

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike
  -- cons which adds to the front
  put (DL.snoc xs result)

-----------------------------------------------------------------------------------
-- mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
-----------------------------------------------------------------------------------

fizzbuzzList'' :: [Integer] -> DL.DList String
fizzbuzzList'' list = execState (mapM_ addResult'' list) DL.empty

addResult'' :: Integer -> State (DL.DList String) ()
addResult'' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-----------------------------------------------------------------------------------

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo = undefined

-----------------------------------------------------------------------------------

main :: IO ()
main = do

  putStrLn "\nfizzBuzz"
  mapM_ (putStrLn . fizzBuzz) [1..20]

  putStrLn "\nfizzbuzzList"
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..20]

  putStrLn "\nfizzbuzzList'"
  mapM_ putStrLn $ fizzbuzzList' [1..20]

  putStrLn "\nfizzbuzzList''"
  mapM_ putStrLn $ fizzbuzzList'' [1..20]

-----------------------------------------------------------------------------------
