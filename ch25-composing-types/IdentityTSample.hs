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

-----------------------------------------------------------------------------------

