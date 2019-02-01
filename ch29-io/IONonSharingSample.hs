module IONonSharingSample where

import Debug.Trace

-----------------------------------------------------------------------------------

blah :: IO String
blah = return "blah"

blah' :: IO String
blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

main :: IO ()
main = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

-----------------------------------------------------------------------------------

-- |
-- Prelude> main
-- outer trace
-- blah
-- blah
-- inner trace
-- woot
-- woot

-- |
-- We only saw inner and outer emitted once because IO is not intended to disable sharing
-- for values not in IO that happen to be used in the course of running of an IO action.

-----------------------------------------------------------------------------------




