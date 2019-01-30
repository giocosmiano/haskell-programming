module BenchmarkingMap where

import Criterion.Main
import Debug.Trace
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

-----------------------------------------------------------------------------------

-- |
-- unpacking strict fields
-- https://wiki.haskell.org/Performance/Data_types#Unpacking_strict_fields
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html
-- https://stackoverflow.com/questions/33931991/what-does-the-unpack-pragma-do-in-this-case

data Map k a = Bin
  {-# UNPACK #-}
  !Size !k a
  !(Map k a) !(Map k a)
  | Tip

type Size = Int

-----------------------------------------------------------------------------------

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0  xs = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

testHashMap :: HM.HashMap String Int
testHashMap = HM.fromList pairList

main :: IO ()
main = defaultMain
  [ bench "lookup one thing, list" $
    whnf (lookup "doesntExist") pairList
  , bench "lookup one thing, map" $
    whnf (M.lookup "doesntExist") testMap
  , bench "lookup one thing, hashMap" $
    whnf (HM.lookup "doesntExist") testHashMap
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- benchmarking lookup one thing, list
-- time                 184.8 μs   (183.5 μs .. 186.1 μs)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 106.0 μs   (103.1 μs .. 108.2 μs)
-- std dev              3.353 μs   (0.0 s .. 3.816 μs)
-- variance introduced by outliers: 23% (moderately inflated)
--
-- benchmarking lookup one thing, map
-- time                 315.7 ns   (315.3 ns .. 316.2 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 170.4 ns   (163.7 ns .. 175.1 ns)
-- std dev              6.964 ns   (0.0 s .. 8.026 ns)
-- variance introduced by outliers: 56% (severely inflated)
--
-- benchmarking lookup one thing, hashMap
-- time                 198.8 ns   (196.8 ns .. 203.2 ns)
--                      0.998 R²   (0.995 R² .. 1.000 R²)
-- mean                 108.4 ns   (104.8 ns .. 110.5 ns)
-- std dev              3.267 ns   (0.0 s .. 3.689 ns)
-- variance introduced by outliers: 42% (moderately inflated)



-- |
-- What’s slower with Map?
-- Using an Int as our key type is usually a sign we’d be better off with a HashMap, IntMap,
-- or Vector, depending on the semantics of our problem. If we need good memory density and
-- locality which will make aggregating and reading values of a large Vector faster, then Map
-- might be inappropriate and we’ll want Vector instead.

-----------------------------------------------------------------------------------






