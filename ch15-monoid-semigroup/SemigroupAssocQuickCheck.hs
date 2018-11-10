module SemigroupAssocQuickCheck where

import Data.Semigroup
import Test.QuickCheck

-----------------------------------------------------------------------------------

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-----------------------------------------------------------------------------------

-- e.g.
-- Prelude> main
-- +++ OK, passed 100 tests.
main :: IO ()
main =
  quickCheck (semigroupAssoc :: TrivAssoc)
