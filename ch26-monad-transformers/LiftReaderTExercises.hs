{-# LANGUAGE InstanceSigs #-}

module LiftReaderTExercises where

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

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- (runReaderT $ ReaderT (\x -> return x)) 1            -> 1
-- (runReaderT $ ReaderT (\x -> return x)) (Identity 1) -> Identity 1
-- (runReaderT $ ReaderT (\x -> Just x)) (Identity 1)   -> Just (Identity 1)
-- (runReaderT $ ReaderT (\x -> [x])) (Identity 1)      -> [Identity 1]
--
-- (runReaderT $ ReaderT (\x -> Just x)) [1]            -> Just [1]
-- (runReaderT $ ReaderT (\x -> [x])) $ Just 1          -> [Just 1]

-----------------------------------------------------------------------------------



