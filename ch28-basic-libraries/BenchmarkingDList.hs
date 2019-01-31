module BenchmarkingDList where

import Criterion.Main
import Debug.Trace

-----------------------------------------------------------------------------------

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

-----------------------------------------------------------------------------------

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDList 123456
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
--  benchmarking concat list
--  time                 129.4 ms   (128.9 ms .. 130.0 ms)
--                       1.000 R²   (1.000 R² .. 1.000 R²)
--  mean                 130.1 ms   (129.9 ms .. 130.2 ms)
--  std dev              238.3 us   (117.6 us .. 354.9 us)
--  variance introduced by outliers: 11% (moderately inflated)
--
--  benchmarking concat dlist
--  time                 129.9 ms   (129.5 ms .. 130.2 ms)
--                       1.000 R²   (1.000 R² .. 1.000 R²)
--  mean                 129.5 ms   (129.0 ms .. 129.7 ms)
--  std dev              410.3 us   (169.5 us .. 605.6 us)
--  variance introduced by outliers: 11% (moderately inflated)


-----------------------------------------------------------------------------------






