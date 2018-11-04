module MonoidAssocQuickCheck where

import Data.Monoid
import Test.QuickCheck

-- e.g.
-- in Prelude >
-- type S = String
-- type B = Bool
-- quickCheck (monoidAssoc :: S -> S -> S -> B)
-- +++ OK, passed 100 tests.
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- e.g.
-- in Prelude >
-- quickCheck (monoidLeftIdentity :: String -> Bool)
-- +++ OK, passed 100 tests.
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

-- e.g.
-- in Prelude >
-- quickCheck (monoidRightIdentity :: String -> Bool)
-- +++ OK, passed 100 tests.
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
