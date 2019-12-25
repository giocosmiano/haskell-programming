{-# LANGUAGE InstanceSigs #-}

module LiftIOExercises where

-- import Control.Monad.Trans.Class

-- import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Trans.Class (lift)

-- class (Monad m) => MonadIO m where
--   liftIO :: IO a -> m a

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

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (MonadIO m) => MonadIO (EitherT e m) where
 liftIO = lift . liftIO

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (MonadIO m) => MonadIO (MaybeT m) where
 liftIO = lift . liftIO

-----------------------------------------------------------------------------------

--instance (MonadIO m) => MonadIO (EitherT e m) where
--  liftIO = lift . liftIO

-- |
-- *** Getting an error compiling with MonadIO instance ***
--
--    • Could not deduce (Monad (EitherT e m))
--        arising from the superclasses of an instance declaration
--      from the context: MonadIO m
--        bound by the instance declaration
--
--    • In the instance declaration for ‘MonadIO (EitherT e m)’

-----------------------------------------------------------------------------------
-- |
-- e.g.
-- runEitherT $ return 1                     -> Right 1
-- runEitherT $ EitherT $ Right $ return 1   -> Right (Right 1)
-- runEitherT $ EitherT $ [Right 1]          -> [Right 1]
-- runEitherT $ EitherT $ Right $ Right 1    -> Right (Right 1)
-- runEitherT $ EitherT $ Identity $ Right 1 -> Identity (Right 1)
--
-- runEitherT $ EitherT $ [Left 1]           -> [Left 1]
-- runEitherT $ EitherT $ Identity $ Left 1  -> Identity (Left 1)

-- |
-- e.g.
-- runEitherT . lift $ return 1           -> Right 1
-- runEitherT . lift $ Right 1            -> Right (Right 1)
-- runEitherT . lift $ [Right 1]          -> [Right (Right 1)]
-- runEitherT . lift $ Right $ Right 1    -> Right (Right (Right 1))
-- runEitherT . lift $ Identity $ Right 1 -> Identity (Right (Right 1))
--
-- runEitherT . lift $ [Left 1]           -> [Right (Left 1)]
-- runEitherT . lift $ Identity $ Left 1  -> Identity (Right (Left 1))

-- |
-- e.g.
-- runExceptT . lift $ return 1           -> Right 1
-- runExceptT . lift $ Right 1            -> Right (Right 1)
-- runExceptT . lift $ [Right 1]          -> [Right (Right 1)]
-- runExceptT . lift $ Right $ Right 1    -> Right (Right (Right 1))
-- runExceptT . lift $ Identity $ Right 1 -> Identity (Right (Right 1))
--
-- runExceptT . lift $ [Left 1]           -> [Right (Left 1)]
-- runExceptT . lift $ Identity $ Left 1  -> Identity (Right (Left 1))

-----------------------------------------------------------------------------------
