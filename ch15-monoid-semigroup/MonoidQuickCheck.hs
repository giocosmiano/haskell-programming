module MonoidQuickCheck where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import MonoidAssocQuickCheck

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

type BullMappend = Bull -> Bull -> Bull -> Bool

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

-- the tests will fail because `mappend` doesn't obey the identity laws
instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

main :: IO ()
main = do
  let ma  = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)

-- e.g.
-- +++ OK, passed 100 tests.
-- *** Failed! Falsifiable (after 3 tests):
-- Twoo
-- *** Failed! Falsifiable (after 1 test):
-- Twoo
