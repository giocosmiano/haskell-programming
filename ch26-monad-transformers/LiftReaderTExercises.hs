{-# LANGUAGE InstanceSigs #-}

module LiftReaderTExercises where

import Data.Functor.Identity
import Control.Monad (liftM)
import Control.Monad.IO.Class
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

--instance (MonadIO m) => MonadIO (ReaderT r m) where
--  liftIO = lift . liftIO

-- |
-- *** Getting an error compiling with MonadIO instance ***
--
--    • Could not deduce (Monad (ReaderT r m))
--        arising from the superclasses of an instance declaration
--      from the context: MonadIO m
--        bound by the instance declaration
----
--      There are instances for similar types:
--        instance [safe] Monad m =>
--                        Monad (Control.Monad.Trans.Reader.ReaderT r m)
--          -- Defined in ‘Control.Monad.Trans.Reader’
--    • In the instance declaration for ‘MonadIO (ReaderT r m)’

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

-- |
-- e.g.
-- (runReaderT . lift $ return 1) 7            -> 1
-- (runReaderT . lift $ return 1) (Identity 7) -> 1
-- (runReaderT . lift $ Just 1) (Identity 7)   -> Just 1
-- (runReaderT . lift $ [1]) (Identity 7)      -> [1]
--
-- (runReaderT . lift $ Just 1) [7]            -> Just 1
-- (runReaderT . lift $ [1]) $ Just 1          -> [1]

-----------------------------------------------------------------------------------



