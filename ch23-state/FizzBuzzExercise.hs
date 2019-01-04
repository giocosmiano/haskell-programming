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

-----------------------------------------------------------------------------------

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

-----------------------------------------------------------------------------------

-- e.g.
-- fizzBuzzList [1..20] ->
-- ["Buzz","19","Fizz","17","16","FizzBuzz","14","13","Fizz","11","Buzz","Fizz","8","7","Fizz","Buzz","4","Fizz","2","1"]
fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-----------------------------------------------------------------------------------

-- e.g.
-- fizzBuzzList' [1..20] ->
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]
fizzBuzzList' :: [Integer] -> [String]
fizzBuzzList' list =
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

-- Because mapping a function that returns an I/O action over a list and then
-- sequencing it is so common, the utility functions `mapM` and `mapM_` were introduced.
-- `mapM` takes a function and a list, maps the function over the list, and
-- then sequences it. `mapM_` does the same thing, but it throws away the result
-- later. We usually use mapM_ when we don’t care what result our sequenced I/O actions have.

-- e.g.
-- mapM  print [1,2,3] -> [(),(),()]   -- same as `traverse  print [1,2,3]`
-- mapM_ print [1,2,3] -> ()           -- same as `traverse_ print [1,2,3]`
-- mapM  Just [1,2,3]  -> Just [1,2,3] -- same as `traverse  Just [1,2,3]`
-- mapM_ Just [1,2,3]  -> Just ()      -- same as `traverse_ Just [1,2,3]`
--
-- execState (mapM_ addResult [1,2,3,4,5,6,7,8,9,10,11,12]) []
-- ["Fizz","11","Buzz","Fizz","8","7","Fizz","Buzz","4","Fizz","2","1"]
--
-- execState (mapM_ addResult' [1,2,3,4,5,6,7,8,9,10,11,12]) DL.empty
-- fromList ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz"]
--
-- execState (mapM_ addResult'' [1,2,3,4,5,6,7,8,9,10,11,12]) DL.empty
-- fromList ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz"]

-----------------------------------------------------------------------------------

-- e.g.
-- fizzBuzzList'' [1..20] ->
-- fromList ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz",
-- "16","17","Fizz","19","Buzz"]
fizzBuzzList'' :: [Integer] -> DL.DList String
fizzBuzzList'' list = execState (mapM_ addResult'' list) DL.empty

addResult'' :: Integer -> State (DL.DList String) ()
addResult'' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-----------------------------------------------------------------------------------

-- e.g.
-- fizzBuzzFromTo 1 20 ->
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]
fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = execState (mapM_ addResultFromTo $ enumFromTo from to) []

addResultFromTo :: Integer -> State [String] ()
addResultFromTo n = do
  xs <- get
  let result = fizzBuzz n
  put (xs ++ [result])

-- OR
--fizzBuzzFromTo from to = fizzBuzzList' $ enumFromTo from to

-----------------------------------------------------------------------------------

main :: IO ()
main = do

  putStrLn "\nfizzBuzz"
  mapM_ (putStrLn . fizzBuzz) [1..20]

  putStrLn "\nfizzBuzzList"
  mapM_ putStrLn $ reverse $ fizzBuzzList [1..20]

  putStrLn "\nfizzBuzzList'"
  mapM_ putStrLn $ fizzBuzzList' [1..20]

  putStrLn "\nfizzBuzzList''"
  mapM_ putStrLn $ fizzBuzzList'' [1..20]

  putStrLn "\nfizzBuzzFromTo"
  mapM_ putStrLn $ fizzBuzzFromTo 1 20

-----------------------------------------------------------------------------------
