module MonoidAssocQuickCheck where

import Data.Monoid
import Test.QuickCheck

-----------------------------------------------------------------------------------

assoc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
assoc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- e.g.
-- Prelude> type S = String
-- Prelude> type B = Bool
-- Prelude> quickCheck (monoidAssoc :: S -> S -> S -> B)
-- +++ OK, passed 100 tests.

-----------------------------------------------------------------------------------

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

-- e.g.
-- Prelude> quickCheck (monoidLeftIdentity :: String -> Bool)
-- +++ OK, passed 100 tests.

-----------------------------------------------------------------------------------

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- e.g.
-- Prelude> quickCheck (monoidRightIdentity :: String -> Bool)
-- +++ OK, passed 100 tests.
