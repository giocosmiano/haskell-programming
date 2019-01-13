{-# LANGUAGE InstanceSigs #-}

module IdentityTExercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- | Identity vs IdentityT
-----------------------------------------------------------------------------------

-- |
-- Plain old Identity. 'a' can be something with more structure, but it's not required
-- and Identity won't know anything about it.
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

-- |
-- The identity monad transformer, serving only to specify that additional structure should exist.
newtype IdentityT m a = IdentityT { runIdentityT :: m a } deriving (Eq, Show)

-----------------------------------------------------------------------------------
-- | Functor implementations
-----------------------------------------------------------------------------------

instance Functor Identity where

  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where

  fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
  fmap f (IdentityT ma) = IdentityT (fmap f ma)

-----------------------------------------------------------------------------------
-- | Applicative implementations
-----------------------------------------------------------------------------------

instance Applicative Identity where

  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where

  pure :: Applicative m => a -> IdentityT m a
  pure = IdentityT . pure

  (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (IdentityT maf) <*> (IdentityT ma) = IdentityT (maf <*> ma)

-----------------------------------------------------------------------------------
-- | Monad implementations
-----------------------------------------------------------------------------------

instance Monad Identity where
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure

-- | NOTE ***
-- `Identity ma` has a structure of `Identity (m a)` and when we bind/feed it to
-- function `f` (a -> IdentityT m b), the result is `IdentityT (m b)`, therefore
-- we need to feed it to `runIdentityT` to extract `(m b)` out of the wrapper `IdentityT (m b)`
-- and use it to create the structure back to `IdentityT (m b)` on line 65

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f

-- | OR ***
--   (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--   (IdentityT ma) >>= f =
--     let aimb = join (fmap runIdentityT (fmap f ma))
--     in  IdentityT aimb

-- | OR ***
--   (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--   (IdentityT ma) >>= f =
--     let aimb = join (fmap (runIdentityT . f) ma)
--     in  IdentityT aimb

-- | OR ***
--   (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--   m >>= k =
--     IdentityT $ runIdentityT . k
--     =<< runIdentityT m

-----------------------------------------------------------------------------------
-- |
-- (>>=) :: m a -> (a -> m b) -> m b
--
-- The trick is we need `runIdentityT` because
-- 1) 𝑓 returns IdentityT m b, but the
-- 2) >>= for the Monad m => has the type `m a -> (a -> m b) -> m b`
--
-- It’ll end up trying to `join m (IdentityT m b)`, which won’t work because
-- `m and IdentityT m` are not the same type. We use `runIdentityT` to unpack the value.
-- Doing this  has the type `IdentityT m b -> m b` and the composition `runIdentityT . f`
-- in this context has the type `a -> m b`.
--

-----------------------------------------------------------------------------------
-- | Foldable implementations
-----------------------------------------------------------------------------------

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = f a z

instance (Foldable fa) => Foldable (IdentityT fa) where
  foldMap f (IdentityT fa) = foldMap f fa
  foldr f z (IdentityT fa) = foldr f z fa

-----------------------------------------------------------------------------------
-- | Traversable implementations
-----------------------------------------------------------------------------------

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Traversable fa) => Traversable (IdentityT fa) where
  traverse f (IdentityT fa) = IdentityT <$> traverse f fa

-----------------------------------------------------------------------------------

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance (Eq (fa a)) => EqProp (IdentityT fa a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a  <- arbitrary
    return $ Identity a

instance (Arbitrary (fa a), CoArbitrary (fa a)) => Arbitrary (IdentityT fa a) where
  arbitrary = do
    fa <- arbitrary
    return $ IdentityT fa

-----------------------------------------------------------------------------------

main = do

  putStrLn "\nTesting Functor, Applicative, Monad, Traversable : Identity"
  quickBatch $ functor (undefined :: Identity (Int, Double, Char))
  quickBatch $ applicative (undefined :: Identity (Int, Double, Char))
  quickBatch $ monad (undefined :: Identity (Int, Double, Char))
  quickBatch $ traversable (undefined :: Identity (Int, Double, [Int]))

  putStrLn "\nTesting Functor, Applicative, Monad, Traversable : IdentityT"
  quickBatch $ functor (undefined :: IdentityT Maybe (Int, Double, Char))
  quickBatch $ applicative (undefined :: IdentityT Maybe (Int, Double, Char))
  quickBatch $ monad (undefined :: IdentityT Maybe (Int, Double, Char))
  quickBatch $ traversable (undefined :: IdentityT Maybe (Int, Double, [Int]))

-----------------------------------------------------------------------------------




