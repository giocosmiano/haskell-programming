{-# LANGUAGE InstanceSigs #-}

module StateTExercises where

import Control.Arrow (first)

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- import Data.Monoid
-- (runStateT $ StateT (\s -> return (1, s))) 7            -> (1,7)
-- (runStateT $ StateT (\s -> Just (1, s))) 7              -> Just (1,7)
-- (runStateT $ StateT (\s -> Just (1, s))) $ Identity 7   -> Just (1,Identity 7)
--
-- (runStateT $ StateT (\s -> return (1, s))) $ Identity 7 -> (1,Identity 7)
-- (runStateT $ StateT (\s -> [(1, s)])) $ Identity 7      -> [(1,Identity 7)]
--
-- (runStateT $ StateT (\s -> Just (1,s))) [7]             -> Just (1,[7])
-- (runStateT $ StateT (\s -> [(1,s)])) $ Just 7           -> [(1,Just 7)]

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-----------------------------------------------------------------------------------

-- | *** Note *** The monadic structure is inside the function `s -> m (a, s)` so we need to unpack things first
-- 1) Unpack/extract the prior computation/function `s -> m (a, s)` from `StateT ma` via `runStateT`
-- 2) Then, apply the unpacked prior computation/function to argument `s` to get the monadic structure `m (a, s)`
-- 3) Finally, lift the function `\(a, s') -> (f a, s')` and apply to `m (a, s)`,
--    resulting to monadic structure `m (b, s')`, which will become the argument to `StateT $`

-- |
-- e.g.
-- import Data.Functor.Identity
-- runStateT ((+3) <$> (StateT $ \s -> Just (5, s))) 7     -> Just (8,7)
-- runStateT ((*3) <$> (StateT $ \s -> Identity (5, s))) 7 -> Identity (15,7)
instance (Functor m) => Functor (StateT s m) where

-- | Using Control.Arrow (first)
-- http://tuttlem.github.io/2014/07/26/practical-arrow-usage.html
-- https://en.wikibooks.org/wiki/Haskell/Understanding_arrows
-- https://wiki.haskell.org/Arrow_tutorial
-- https://www.schoolofhaskell.com/user/peter/arrow-tutorial

  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT ma) =
    StateT $ fmap (first f) . runStateT (StateT ma)

-- |
-- OR
--   fmap f (StateT ma) =
--     StateT $ \s -> (\(a, s') -> (f a, s')) <$> runStateT (StateT ma) s

-- |
-- OR
--   fmap f (StateT ma) =
--     StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ (runStateT (StateT ma)) s

-----------------------------------------------------------------------------------

-- | See these threads on --> Why does `Applicative (StateT s f)` instance require `Monad f` bound?
-- https://github.com/data61/fp-course/issues/134
-- https://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m

-- |
-- 1) Apply the prior computation/function `g` to argument `s` to get a new computation/function and state `(f, s')`
-- 2) Then, apply the current computation/function `h` to state `s'` from prior computation, resulting to new state `(x, s'')`
-- 3) Then, apply the new computation/function `f` to value `x`, resulting to `(b, s'')`
-- 4) Finally, wrap the result `(b, s'')` with `return`, resulting to monadic structure `m (b, s'')`
--    which will become the argument to `StateT $`

-- |
-- e.g.
-- import Data.Functor.Identity
-- runStateT ((StateT $ \s -> Just((+3), s)) <*> (StateT $ \s -> Just(5, s))) 7              -> Just (8,7)
-- runStateT ((*) <$> (StateT $ \s -> Identity(3, s)) <*> (StateT $ \s -> Identity(5, s))) 7 -> Identity (15,7)
instance (Monad m) => Applicative (StateT s m) where

  pure :: Applicative m => a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT g <*> StateT h =
    StateT $ \s -> do
      (f, s') <- g s
      (x, s'') <- h s'
      return (f x, s'')

-----------------------------------------------------------------------------------

-- |
-- 1) First, unpack/extract the prior computation/function `s -> m (a, s)` from `StateT ma` via `runStateT`
-- 2) Apply the unpacked prior computation/function to argument `s` to get the monadic structure `m (a, s)`
-- 3) Use `<-` to extract the value `(a, s)` from monadic structure `m (a, s)`
-- 4) Apply the current computation/function `f` to value `a` resulting to `StateT s m b` -- see (>>=) signature
-- 5) Finally, unpack/extract the function `s -> m (b, s)` from `StateT s m b` and apply to `s'`,
--    resulting to monadic structure `m (b, s')`, which will become the argument to `StateT $`

-- |
-- e.g.
-- import Data.Functor.Identity
-- runStateT ((StateT $ \s -> Just (5, s)) >>= return . (+3)) 7     -> Just (8,7)
-- runStateT ((StateT $ \s -> Identity (5, s)) >>= return . (*3)) 7 -> Identity (15,7)
instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT ma) >>= f =
    StateT $ \s -> do
      (a, s') <- runStateT (StateT ma) s
      runStateT (f a) s'

-- |
-- OR
--   (StateT ma) >>= f =
--     StateT $ \s -> do
--       (a, s') <- (runStateT (StateT ma)) s
--       (runStateT (f a)) s'

-----------------------------------------------------------------------------------




