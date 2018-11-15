{-# LANGUAGE InstanceSigs #-}

module ApplicativeReader where

import Text.Printf (printf)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- https://www.reddit.com/r/haskellquestions/comments/6r46zp/applicative_instance_for_reader/
-- https://gist.github.com/twopoint718/c02164137c6fca9e0c4c
-----------------------------------------------------------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

myLift :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLift f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)
-- OR
--  pure x = (\_ -> x)
--  pure x = Reader (\_ -> x)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader f) <*> (Reader g) = Reader $ \x -> f x (g x)
-- OR
--  f <*> g = \x -> f x (g x)
--  f <*> g = Reader (\x -> (runReader f x) (runReader g x))

-- e.g.
-- addThenMultiply x =  (x+2) * (x+1)
-- addThenMultiply 1 -> 6  (3 * 2)
-- addThenMultiply 3 -> 20 (5 * 4)
-- addThenMultiply 5 -> 42 (7 * 6)
addThenMultiply :: Int -> Int
addThenMultiply x = runReader (pure (*) <*> Reader (+2) <*> Reader (+1)) x
