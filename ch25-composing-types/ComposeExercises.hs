{-# LANGUAGE InstanceSigs #-}

module ComposeExercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

newtype One f a = One (f a)
                deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-----------------------------------------------------------------------------------

newtype Three f g h a = Three (f (g (h a)))
                      deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-----------------------------------------------------------------------------------

newtype Compose f g a = Compose { getCompose :: f (g a) }
                      deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure $ pure a

--  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = undefined

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
--  putStrLn "\nTesting Applicative, Monad : Compose"
--  quickBatch $ functor (undefined :: Compose [Just (Int, Double, Char)]
--  quickBatch $ applicative (undefined :: Compose (Int, Double, Char))
--  quickBatch $ monad (undefined :: Compose (Int, Double, Char))
