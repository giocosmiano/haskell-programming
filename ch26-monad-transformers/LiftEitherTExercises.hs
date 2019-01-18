{-# LANGUAGE InstanceSigs #-}

module LiftEitherTExercises where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

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

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

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
