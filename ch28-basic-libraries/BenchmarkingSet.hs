module BenchmarkingSet where

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

data Set a = Bin
  {-# UNPACK #-}
  !Size !a !(Set a) !(Set a)
  | Tip

type Size = Int

-----------------------------------------------------------------------------------

bumpIt :: (Num a, Num b) => (a, b) -> (a, b)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

main :: IO ()
main = defaultMain
  [ bench "member check map" $
    whnf membersMap 9999
  , bench "member check set" $
    whnf membersSet 9999
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- benchmarking member check map
-- time                 246.1 ns   (245.6 ns .. 246.6 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 140.6 ns   (135.0 ns .. 144.0 ns)
-- std dev              5.538 ns   (2.542 ns .. 7.408 ns)
-- variance introduced by outliers: 55% (severely inflated)
--
-- benchmarking member check set
-- time                 248.1 ns   (245.4 ns .. 250.8 ns)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 141.8 ns   (137.9 ns .. 144.4 ns)
-- std dev              4.202 ns   (2.518 ns .. 5.942 ns)
-- variance introduced by outliers: 41% (moderately inflated)


-----------------------------------------------------------------------------------






