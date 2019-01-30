module BenchmarkingSequence where

import Criterion.Main
import Debug.Trace
import qualified Data.Sequence as S

-----------------------------------------------------------------------------------

-- |
-- unpacking strict fields
-- https://wiki.haskell.org/Performance/Data_types#Unpacking_strict_fields
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html
-- https://stackoverflow.com/questions/33931991/what-does-the-unpack-pragma-do-in-this-case

-- |
-- Elem is so elements and nodes can be
-- distinguished in the types of the
-- implementation. Don't sweat it.
--
--newtype Seq a = Seq (FingerTree (Elem a))
--
--newtype Elem a = Elem { getElem :: a }
--
--data FingerTree a = Empty
--  | Single a
--  | Deep {-# UNPACK #-} !Int !(Digit a)
--     (FingerTree (Node a)) !(Digit a)

-----------------------------------------------------------------------------------

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1..100000])

main :: IO ()
main = defaultMain
  [ bench "concatenate lists" $
    nf mconcat lists
  , bench "concatenate sequences" $
    nf mconcat seqs
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- benchmarking concatenate lists
-- time                 13.48 ms   (13.40 ms .. 13.55 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 10.98 ms   (10.32 ms .. 11.46 ms)
-- std dev              1.236 ms   (815.4 μs .. 1.700 ms)
-- variance introduced by outliers: 53% (severely inflated)
--
-- benchmarking concatenate sequences
-- time                 13.37 ms   (13.34 ms .. 13.41 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 10.89 ms   (10.15 ms .. 11.32 ms)
-- std dev              1.241 ms   (860.1 μs .. 1.822 ms)
-- variance introduced by outliers: 53% (severely inflated)

-----------------------------------------------------------------------------------

lists' :: [Int]
lists' = [1..100000]

seqs' :: S.Seq Int
seqs' = S.fromList [1..100000]

main' :: IO ()
main' = defaultMain
  [ bench "indexing lists" $
    whnf (\xs -> xs !! 9001) lists'
  , bench "indexing sequences" $
    whnf (flip S.index 9001) seqs'
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main'
-- benchmarking indexing lists
-- time                 18.97 μs   (18.94 μs .. 19.01 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 11.30 μs   (10.87 μs .. 11.58 μs)
-- std dev              456.2 ns   (198.2 ns .. 602.2 ns)
-- variance introduced by outliers: 43% (moderately inflated)
--
-- benchmarking indexing sequences
-- time                 128.8 ns   (128.6 ns .. 129.1 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 77.71 ns   (74.77 ns .. 80.64 ns)
-- std dev              3.910 ns   (2.378 ns .. 5.459 ns)
-- variance introduced by outliers: 68% (severely inflated)

-- |
-- What’s slower with Sequence?
-- Sequence is a persistent data structure like Map, so the memory density isn’t as good
-- as it is with Vector. Indexing by Int will be faster with Vector as well. List will be
-- faster with consing and concatenation in some cases, if the lists are small. When we
-- know we need cheap appending to the end of a long list, it’s worth giving Sequence a try,
-- but it’s better to base the final decision on benchmarking data if performance matters.

-----------------------------------------------------------------------------------






