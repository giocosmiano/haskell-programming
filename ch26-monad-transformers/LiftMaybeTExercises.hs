{-# LANGUAGE InstanceSigs #-}

module LiftMaybeTExercises where

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

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

-----------------------------------------------------------------------------------

--instance (MonadIO m) => MonadIO (MaybeT m) where
--  liftIO = lift . liftIO

-- |
-- *** Getting an error compiling with MonadIO instance ***
--
--    • Could not deduce (Monad (MaybeT m))
--        arising from the superclasses of an instance declaration
--      from the context: MonadIO m
--
--      There are instances for similar types:
--        instance [safe] Monad m =>
--                        Monad (Control.Monad.Trans.Maybe.MaybeT m)
--          -- Defined in ‘Control.Monad.Trans.Maybe’
--    • In the instance declaration for ‘MonadIO (MaybeT m)’

-----------------------------------------------------------------------------------
-- |
-- lift :: (Monad m) => m a -> t m a
-- (MaybeT . liftM Just) :: Monad m => m a -> MaybeT m a
--
-- MaybeT :: m (Maybe a) -> MaybeT m a
-- (liftM Just) :: Monad m => m a -> m (Maybe a)
--
-- v :: Monad m => m a
-- liftM Just :: Monad m => m a -> m (Maybe a)
-- liftM Just v :: m (Maybe a)
-- MaybeT (liftM Just v) :: MaybeT m a
--
-----------------------------------------------------------------------------------
-- Roughly speaking, this has taken an m a and lifted it into a MaybeT context
--
-- The general pattern with MonadTrans instances demonstrated by MaybeT is that you’re usually
-- going to lift the injection of the known structure (with MaybeT, the known structure is Maybe)
-- over some Monad. Injection of structure usually means return, but since with MaybeT we know we
-- want Maybe structure, we use Just. That transforms an m a into m (T a) where capital T is some
-- concrete type you’re lifting the m a into. Then to cap it all off, you use the data constructor
-- for your monad transformer, and the value is now lifted into the larger context.

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- See MaybeTExercises how to run these `runMaybeT` samples
-- because these won't run as it conflicts with Control.Monad.Trans.Maybe
-- runMaybeT $ return 1                   -> Just 1
-- runMaybeT $ MaybeT $ Just $ return 1   -> Just (Just 1)
-- runMaybeT $ MaybeT [Just 1]            -> [Just 1]
-- runMaybeT $ MaybeT $ Just $ Just 1     -> Just (Just 1)
-- runMaybeT $ MaybeT $ Identity $ Just 1 -> Identity (Just 1)

-- |
-- e.g.
-- runMaybeT . lift $ return 1          -> Just 1
-- runMaybeT . lift $ Just 1            -> Just (Just 1)
-- runMaybeT . lift $ [Just 1]          -> [Just (Just 1)]
-- runMaybeT . lift $ Just $ Just 1     -> Just (Just (Just 1))
-- runMaybeT . lift $ Identity $ Just 1 -> Identity (Just (Just 1))

-----------------------------------------------------------------------------------



