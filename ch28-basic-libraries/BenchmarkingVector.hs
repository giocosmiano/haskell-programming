module BenchmarkingVector where

import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import Debug.Trace
import Data.Vector ((//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

-----------------------------------------------------------------------------------

-- |
-- unpacking strict fields
-- https://wiki.haskell.org/Performance/Data_types#Unpacking_strict_fields
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html
-- https://stackoverflow.com/questions/33931991/what-does-the-unpack-pragma-do-in-this-case

-- | Boxed vectors, supporting efficient slicing.
--data Vector a =
--  Vector {-# UNPACK #-} !Int
--         {-# UNPACK #-} !Int
--         {-# UNPACK #-} !(Array a)
--  deriving ( Typeable )

-----------------------------------------------------------------------------------

slice :: Int -> Int -> [a] -> [a]
slice from len xs = take len (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

main :: IO ()
main = defaultMain
  [ bench "slicing list" $
    whnf (head . slice 100 900) l
  , bench "slicing vector" $
    whnf (V.head . V.slice 100 900) v
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- benchmarking slicing list
-- time                 430.3 ns   (429.2 ns .. 431.6 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 270.4 ns   (259.9 ns .. 278.7 ns)
-- std dev              12.90 ns   (7.400 ns .. 19.66 ns)
-- variance introduced by outliers: 63% (severely inflated)
--
-- benchmarking slicing vector
-- time                 90.55 ns   (90.20 ns .. 91.11 ns)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 56.05 ns   (54.13 ns .. 57.99 ns)
-- std dev              2.788 ns   (1.958 ns .. 3.643 ns)
-- variance introduced by outliers: 68% (severely inflated)

-- |
-- What makes Vector nicer than lists and Array in this respect is that when we construct
-- a slice or view of another Vector, it doesn’t have to construct as much new data. It
-- returns a new wrapper around the original underlying array with a new index and offset
-- with a reference to the same original Array. Doing the same with an ordinary Array or
-- a list would’ve required copying more data. Speed comes from being sneaky and skipping work.

-----------------------------------------------------------------------------------

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
  where go 0 v = v
        go n v = go (n-1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
  where updates = fmap (\n -> (n, 0)) [0..n]

batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
  where updates = fmap (\n -> (n, 0)) (V.fromList [0..n])

main' :: IO ()
main' = defaultMain
  [ bench "slow" $ whnf slow 9998
  , bench "batch list" $ whnf batchList 9998
  , bench "batch vector" $ whnf batchVector 9998
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main'
-- benchmarking slow
-- time                 1.244 s    (1.228 s .. 1.263 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 1.243 s    (1.238 s .. 1.245 s)
-- std dev              4.308 ms   (0.0 s .. 4.858 ms)
-- variance introduced by outliers: 19% (moderately inflated)
--
-- benchmarking batch list
-- time                 1.257 ms   (1.252 ms .. 1.263 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 832.0 μs   (786.9 μs .. 874.4 μs)
-- std dev              88.88 μs   (71.31 μs .. 109.5 μs)
-- variance introduced by outliers: 69% (severely inflated)
--
-- benchmarking batch vector
-- time                 2.045 ms   (2.040 ms .. 2.050 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 1.404 ms   (1.324 ms .. 1.475 ms)
-- std dev              173.8 μs   (140.0 μs .. 212.0 μs)
-- variance introduced by outliers: 72% (severely inflated)

-----------------------------------------------------------------------------------

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = return v
        go n v = (MV.write v n 0) >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = V.freeze v
        go n v = (MV.write v n 0) >> go (n-1) v

main'' :: IO ()
main'' = defaultMain
  [ bench "mutable IO vector" $
    whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector" $
    whnf mutableUpdateST 9998
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main''
-- benchmarking mutable IO vector
-- time                 4.671 ms   (4.656 ms .. 4.682 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 3.412 ms   (3.235 ms .. 3.575 ms)
-- std dev              425.3 μs   (339.4 μs .. 527.7 μs)
-- variance introduced by outliers: 66% (severely inflated)
--
-- benchmarking mutable ST vector
-- time                 4.742 ms   (4.683 ms .. 4.803 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 3.455 ms   (3.250 ms .. 3.637 ms)
-- std dev              466.7 μs   (374.7 μs .. 592.8 μs)
-- variance introduced by outliers: 72% (severely inflated)

-----------------------------------------------------------------------------------




