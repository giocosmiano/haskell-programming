{-# LANGUAGE InstanceSigs #-}

module IdentityTSample where

import Control.Monad

-----------------------------------------------------------------------------------

-- |
-- Plain old Identity. 'a' can be something with more structure, but it's not required
-- and Identity won't know anything about it.
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

-- |
-- The identity monad transformer, serving only to specify that additional structure should exist.
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

-----------------------------------------------------------------------------------

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

-----------------------------------------------------------------------------------

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

-----------------------------------------------------------------------------------

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure

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

-----------------------------------------------------------------------------------

