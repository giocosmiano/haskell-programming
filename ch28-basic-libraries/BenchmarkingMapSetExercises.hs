module BenchmarkingMapSetExercises where

import Criterion.Main
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM

-----------------------------------------------------------------------------------

-- |
-- unpacking strict fields
-- https://wiki.haskell.org/Performance/Data_types#Unpacking_strict_fields
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html
-- https://stackoverflow.com/questions/33931991/what-does-the-unpack-pragma-do-in-this-case

--data Map k a = Bin
--  {-# UNPACK #-}
--  !Size !k a
--  !(Map k a) !(Map k a)
--  | Tip
--
--data Set a = Bin
--  {-# UNPACK #-}
--  !Size !a !(Set a) !(Set a)
--  | Tip
--
--type Size = Int

-----------------------------------------------------------------------------------

bumpIt :: (Num a, Num b) => (a, b) -> (a, b)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

m' :: M.Map Int Int
m' = M.fromList $ take 5000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s' :: S.Set Int
s' = S.fromList $ take 5000 stream
  where stream = iterate (+1) 0

main :: IO ()
main = defaultMain
  [ bench "union map" $
    whnf (M.union m) m'
  , bench "union set" $
    whnf (S.union s) s'
  , bench "intersection map" $
    whnf (M.intersection m) m'
  , bench "intersection set" $
    whnf (S.intersection s) s'
  , bench "difference map" $
    whnf (M.difference m) m'
  , bench "difference set" $
    whnf (S.difference s) s'
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- benchmarking union map
-- time                 131.4 μs   (129.8 μs .. 133.3 μs)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 76.22 μs   (71.91 μs .. 79.50 μs)
-- std dev              4.363 μs   (1.869 μs .. 5.843 μs)
-- variance introduced by outliers: 51% (severely inflated)
--
-- benchmarking union set
-- time                 108.4 μs   (107.7 μs .. 109.3 μs)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 61.90 μs   (60.57 μs .. 62.95 μs)
-- std dev              1.460 μs   (553.4 ns .. 2.009 μs)
-- variance introduced by outliers: 16% (moderately inflated)
--
-- benchmarking intersection map
-- time                 143.4 μs   (141.9 μs .. 145.2 μs)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 83.77 μs   (81.29 μs .. 85.74 μs)
-- std dev              2.885 μs   (1.603 μs .. 4.048 μs)
-- variance introduced by outliers: 27% (moderately inflated)
--
-- benchmarking intersection set
-- time                 130.5 μs   (129.1 μs .. 133.3 μs)
--                      0.995 R²   (0.987 R² .. 0.999 R²)
-- mean                 72.82 μs   (69.16 μs .. 76.14 μs)
-- std dev              3.881 μs   (3.360 μs .. 4.321 μs)
-- variance introduced by outliers: 47% (moderately inflated)
--
-- benchmarking difference map
-- time                 160.6 μs   (159.3 μs .. 162.0 μs)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 92.46 μs   (89.55 μs .. 96.57 μs)
-- std dev              3.889 μs   (162.1 ns .. 4.823 μs)
-- variance introduced by outliers: 34% (moderately inflated)
--
-- benchmarking difference set
-- time                 139.4 μs   (138.4 μs .. 140.9 μs)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 78.74 μs   (76.53 μs .. 80.26 μs)
-- std dev              2.281 μs   (0.0 s .. 2.631 μs)
-- variance introduced by outliers: 20% (moderately inflated)

-----------------------------------------------------------------------------------






