module Semigroup2Exercises where

import Data.Semigroup

data Validation a b = Failure a
                    | Success b
                    deriving (Eq, Show)

-- take note that type parameter `a` is the *ONLY* semigroup, and `b` is NOT
instance Semigroup a => Semigroup (Validation a b) where
  Success x <> _         = Success x
  _         <> Success x = Success x
  Failure x <> Failure y = Failure (x <> y)

-- e.g.
-- in Prelude > main
-- Success 1
-- Failure "wootblah"
-- Success 1
-- Success 2
main = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
