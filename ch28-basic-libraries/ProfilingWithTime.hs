module ProfilingWithTime where

import Control.Monad

-----------------------------------------------------------------------------------

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 9999999)
  putStrLn "g"

main :: IO ()
main = do
  f
  g


-- |
--
-- $ stack ghc -- -prof -fprof-auto \
-- > -rtsopts -O2 profile.hs
-- ./profile +RTS -P
-- cat profile.prof

-----------------------------------------------------------------------------------

blah :: [Integer]
blah = [1..1000]

main' :: IO ()
main' = replicateM_ 10000 (print blah)

-- |
--
-- ghc -prof -fprof-auto -rtsopts -O2 loci.hs
-- ./loci +RTS -hc -p
-- hp2ps loci.hp

-----------------------------------------------------------------------------------


