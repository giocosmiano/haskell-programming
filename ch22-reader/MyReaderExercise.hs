{-# LANGUAGE InstanceSigs #-}

module MyReaderExercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

-----------------------------------------------------------------------------------

-- e.g.
-- fmap (+3) (*5) 7                                               -> 38
-- fmap (runReader $ Reader $ (+3)) (runReader $ Reader $ (*5)) 7 -> 38
instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \x -> f (ra x)

-- e.g.
-- (+) <$> (+3) <*> (*5) $ 7                                               -> 45
-- (+) <$> (runReader $ Reader $ (+3)) <*> (runReader $ Reader $ (*5)) $ 7 -> 45
instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (Reader rab) <*> (Reader ra) = Reader $ \x -> rab x (ra x)

-- e.g.
-- (+3) >>= return . (*5) $ 7                                               -> 50
-- (runReader $ Reader $ (+3)) >>= return . (runReader $ Reader $ (*5)) $ 7 -> 50
instance Monad (Reader r) where
  return = pure
  (Reader ra) >>= aRb = Reader $ \x -> runReader (aRb (ra x)) x

-- TODO: how-to implement quickBatch for Reader???
{-
instance (Arbitrary r, CoArbitrary r,
          Arbitrary a, CoArbitrary a) => Arbitrary (Reader r a) where
  arbitrary = do
    r <- arbitrary
    a <- arbitrary
    return $ Reader $ \r -> a

instance (Eq r, Eq a) => EqProp (Reader r a) where (=-=) = eq
-}

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

--main = do
--
--  putStrLn "\nTesting Applicative, Monad : Reader"
--  quickBatch $ functor (undefined :: Reader (Int, Double, Char))
--  quickBatch $ applicative (undefined :: Reader (Int, Double, Char))
--  quickBatch $ monad (undefined :: Reader (Int, Double, Char))
