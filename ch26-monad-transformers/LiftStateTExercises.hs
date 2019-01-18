{-# LANGUAGE InstanceSigs #-}

module LiftStateTExercises where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class

-----------------------------------------------------------------------------------
-- Using `lift` from Control.Monad.Trans.Class
-----------------------------------------------------------------------------------
-- |
-- Lift a computation from the argument monad to the constructed monad.
--
-- Here the `t` is a (constructed) monad transformer type that has an instance of MonadTrans defined.
--
-- class MonadTrans t where
--   lift :: (Monad m) => m a -> t m a

-----------------------------------------------------------------------------------
-- |
-- *** Note *** The monadic structure is inside the function `s -> m (a, s)` so we just need to
-- `lift` the monadic value then create the StateT

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT r) where
  lift ma =
    StateT $ \s -> do
              a <- ma
              return (a, s)

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- (runStateT $ StateT (\s -> return (1, s))) 7            -> (1,7)
-- (runStateT $ StateT (\s -> Just (1, s))) 7              -> Just (1,7)
-- (runStateT $ StateT (\s -> Just (1, s))) $ Identity 7   -> Just (1,Identity 7)
--
-- (runStateT $ StateT (\s -> return (1, s))) $ Identity 7 -> (1,Identity 7)
-- (runStateT $ StateT (\s -> [(1, s)])) $ Identity 7      -> [(1,Identity 7)]
--
-- (runStateT $ StateT (\s -> Just (1,s))) [7]             -> Just (1,[7])
-- (runStateT $ StateT (\s -> [(1,s)])) $ Just 7           -> [(1,Just 7)]

-- |
-- e.g.
-- (runStateT . lift $ return 1) 7             -> (1,7)
-- (runStateT . lift $ Just 1) 7               -> Just (1,7)
-- (runStateT . lift $ Just 1) $ Identity 7    -> Just (1,Identity 7)
--
-- (runStateT . lift $ return 1) $ Identity 7  -> (1,Identity 7)
-- (runStateT . lift $ [1]) $ Identity 7       -> [(1,Identity 7)]
--
-- (runStateT . lift $ Just 1) [7]             -> Just (1,[7])
-- (runStateT . lift $ [1]) $ Just 7           -> [(1,Just 7)]

-----------------------------------------------------------------------------------



