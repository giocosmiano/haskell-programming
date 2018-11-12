module BadMonoidUsingCheckers where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- Install `checkers` package --> https://github.com/conal/checkers
-- Prelude> stack build checkers
--
-- Restart ghci after installing `checkers` package
-- Prelude> stack ghci
-----------------------------------------------------------------------------------

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)
