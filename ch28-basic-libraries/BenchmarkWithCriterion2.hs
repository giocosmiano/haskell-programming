module BenchmarkWithCriterion2 where

import Criterion.Main
import Debug.Trace

-----------------------------------------------------------------------------------

-- |
-- *** NOTE ***
-- Note that if we get weird benchmark results, we’ll want to resort to the old programmer’s
-- trick of wiping our build. With Stack we’d run `stack clean`, with Cabal it’d be `cabal clean`.
--
-- Implementing (!?) for our benchmark, similar to (!!) in base. We have declared
-- that it’s a left-associating infix operator (infixl) with a precedence of 9.
--
-- The functions whnf and nf (also in criterion) refer to weak head normal form and
-- normal form, respectively. Weak head normal form evaluates to the first data constructor.
-- That means that if the outermost data constructor is a Maybe, it’s only going to
-- evaluate enough to find out if it’s a Nothing or a Just — if there is a Just a,
-- it won’t count the cost of evaluating the `a` value.
--
-- Using nf would mean we wanted to include the cost of fully evaluating the `a` as well
-- as the first data constructor. The key when determining whether we want whnf or nf is
-- to think about what we’re trying to benchmark and if reaching the first data constructor
-- will do all the work we’re trying to measure or not.
--
-- Prelude> :t defaultMain
-- defaultMain :: [Benchmark] -> IO ()
--
-- Prelude> :t whnf
-- whnf :: (a -> b) -> a -> Benchmarkable
--
-- Prelude> :t nf
-- nf :: Control.DeepSeq.NFData b => (a -> b) -> a -> Benchmarkable
--
-- The sample below, what we want to compare two things: the weak head normal form
-- evaluation of the original indexing operator (!!) and that of our safe version (!?),
-- applied to the same long list. We only need weak head normal form because (!!) and
-- (!?) don’t return a data constructor until they’ve done the work already
--
-- Our goal with this example is to equalize the performance difference between (!?), (!?!) and (!!).
--
-- In general when deciding between whnf and nf, ask yourself, “when I have reached the first data
-- constructor, have I done most or all of the work that matters?”

-----------------------------------------------------------------------------------

infixl 9 !?

(!?) :: [a] -> Int -> Maybe a

_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n-1)

-----------------------------------------------------------------------------------

-- |
-- see http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#%21%21

infixl 9 !?!

{-# INLINABLE (!?!) #-}

(!?!) :: [a] -> Int -> Maybe a

xs !?! n
  | n < 0 = Nothing
  | otherwise =
      foldr
        (\x r k ->
          case k of
            0 -> Just x
            _ -> r (k-1))
        (const Nothing) xs n

-----------------------------------------------------------------------------------

myList :: [Int]
myList = trace "myList was evaluated"
  ([1..9999] ++ [undefined])

main :: IO ()
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
    , bench "index list maybe with !?  index 9999"
    $ nf (myList !?) 9999
    , bench "index list maybe with !?! index 9999"
    $ nf (myList !?!) 9999
  ]

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- benchmarking index list 9999
-- myList was evaluated
-- time                 23.18 us   (22.95 us .. 23.55 us)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 23.28 us   (23.09 us .. 23.59 us)
-- std dev              835.2 ns   (507.0 ns .. 1.401 us)
-- variance introduced by outliers: 41% (moderately inflated)
--
-- benchmarking index list maybe with !?  index 9999
-- *** Exception: Prelude.undefined

-----------------------------------------------------------------------------------

-- |
-- Evaluating to weak head normal form, the above will continue until it reaches
-- the index, we reach the element, or we hit the end of the list.
--
-- [1, 2, 3] !? 2
-- matches final case
--
-- (_: [2, 3]) !? 2
--   = [2, 3]  !? (2-1)
-- not a data constructor, keep going
--
-- [2, 3] !? 1
-- matches final case
--
-- (_: [3]) !? 1
--   = [3]  !? (1-1)
-- not a data constructor, keep going
--
-- [3] !? 0
-- matches final case
--
-- (x:[]) !? 0 = Just x
-- We stop at Just

-----------------------------------------------------------------------------------
