{-# LANGUAGE InstanceSigs #-}

module StateTExercises where

-----------------------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-----------------------------------------------------------------------------------

-- |
-- 1) Note that the monadic structure is inside the function `s -> m (a, s)` so we need to unpack things first
-- 2) Extract the function `s -> m (a, s)` from `StateT ma` via `runStateT`
-- 3) Then, apply the extracted function to argument `s` to get the monadic structure `m (a, s)`
-- 4) Finally, lift the function `\(a, s') -> (f a, s')` and apply to `m (a, s)`

-- e.g.
-- runStateT ((+3) <$> (StateT $ \s -> Just (5, s))) 7     -> Just (8,7)
-- runStateT ((*3) <$> (StateT $ \s -> Identity (5, s))) 7 -> Identity (15,7)
instance (Functor m) => Functor (StateT s m) where

  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT ma) =
    StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ (runStateT (StateT ma)) s

-----------------------------------------------------------------------------------

-- | See these threads on --> Why does `Applicative (StateT s f)` instance require `Monad f` bound?
-- https://github.com/data61/fp-course/issues/134
-- https://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m

-- e.g.
-- runStateT ((+) <$> (StateT $ \s -> Just(3, s))     <*> (StateT $ \s -> Just(5, s))) 7     -> Just (8,7)
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
-- 1) First, extract the function `s -> m (a, s)` from `StateT ma` via `runStateT`
-- 2) Apply the extracted function to argument `s` to get the monadic structure `m (a, s)`
-- 3) Use `<-` to extract the value `(a, s)` from monadic structure `m (a, s)`
-- 3) Apply the function `f` to value `a` which will result to `StateT s m b` -- see (>>=) signature
-- 4) Finally, extract the function `s -> m (b, s)` from `StateT s m b` and apply to `s'`
--    which will become the argument to `StateT $`

-- e.g.
-- runStateT ((StateT $ \s -> Just (5, s)) >>= return . (+3)) 7     -> Just (8,7)
-- runStateT ((StateT $ \s -> Identity (5, s)) >>= return . (*3)) 7 -> Identity (15,7)
instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT ma) >>= f =
    StateT $ \s -> do
      (a, s') <- (runStateT (StateT ma)) s
      (runStateT (f a)) s'

-----------------------------------------------------------------------------------




