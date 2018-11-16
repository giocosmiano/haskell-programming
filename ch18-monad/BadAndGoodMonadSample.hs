module BadAndGoodMonadSample where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- Sample of BAD Functor, Applicative, Monad
-----------------------------------------------------------------------------------

data CountMeBad a = CountMeBad Integer a
                  deriving (Eq, Show)

instance Functor CountMeBad where
  fmap f (CountMeBad i a) = CountMeBad (i + 1) (f a)

instance Applicative CountMeBad where
  pure = CountMeBad 0
  CountMeBad n f <*> CountMeBad n' a = CountMeBad (n + n') (f a)

instance Monad CountMeBad where
  return = pure
  CountMeBad n a >>= f =
    let CountMeBad _ b = f a
    in  CountMeBad (n + 1) b

instance Arbitrary a => Arbitrary (CountMeBad a) where
  arbitrary = CountMeBad <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMeBad a) where (=-=) = eq

-----------------------------------------------------------------------------------
-- Sample of GOOD Functor, Applicative, Monad
-----------------------------------------------------------------------------------

data CountMeGood a = CountMeGood Integer a
                  deriving (Eq, Show)

instance Functor CountMeGood where
  fmap f (CountMeGood i a) = CountMeGood i (f a)

instance Applicative CountMeGood where
  pure = CountMeGood 0
  CountMeGood n f <*> CountMeGood n' a = CountMeGood (n + n') (f a)

instance Monad CountMeGood where
  return = pure
  CountMeGood n a >>= f =
    let CountMeGood n' b = f a
    in  CountMeGood (n + n') b

instance Arbitrary a => Arbitrary (CountMeGood a) where
  arbitrary = CountMeGood <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMeGood a) where (=-=) = eq

-----------------------------------------------------------------------------------

main = do
  putStrLn "\nTesting Monad : CountMeBad"
  let trigger :: CountMeBad (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

  putStrLn "\nTesting Monad : CountMeGood"
  let trigger :: CountMeGood (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
