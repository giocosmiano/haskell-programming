module ValidationApplicativeExercise where

import GHC.Generics
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Gen (oneof)

data Validation' e a = Failure' e
                     | Success' a
                     deriving (Eq, Show)

-----------------------------------------------------------------------------------

instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation' e) where
  pure = Success'
  (Failure' x) <*> (Failure' y) = Failure' (x <> y)
  (Success' f) <*> (Success' x) = Success' $ f x
  _            <*> (Failure' x) = Failure' x
  (Failure' x) <*> _            = Failure' x

-----------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Success' a,
           return $ Failure' b]

instance (Eq a, Eq b) => EqProp (Validation' a b) where (=-=) = eq

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

main :: IO ()
main = quickBatch $ applicative (undefined :: Validation' String (Int, Double, Char))
