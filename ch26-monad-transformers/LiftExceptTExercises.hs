{-# LANGUAGE InstanceSigs #-}

module LiftExceptTExercises where

import Data.Functor.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

-----------------------------------------------------------------------------------
-- Using `lift` from Control.Monad.Trans.Class
-----------------------------------------------------------------------------------

{-

class MonadTrans t where

-- | Lift a computation from the argument monad to the constructed monad.
  lift :: (Monad m) => m a -> t m a

-- |
-- Here the `t` is a (constructed) monad transformer type that has an instance of MonadTrans defined.

-}

-----------------------------------------------------------------------------------
-- |
-- newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
-----------------------------------------------------------------------------------

-- |
-- e.g.
-- runExceptT $ return 1                     -> Right 1
-- runExceptT $ ExceptT $ [Right 1]          -> [Right 1]
-- runExceptT $ ExceptT $ Right $ Right 1    -> Right (Right 1)
-- runExceptT $ ExceptT $ Identity $ Right 1 -> Identity (Right 1)
--
-- runExceptT $ ExceptT $ [Left 1]           -> [Left 1]
-- runExceptT $ ExceptT $ Identity $ Left 1  -> Identity (Left 1)

-- |
-- e.g.
-- runExceptT . lift $ return 1           -> Right 1
-- runExceptT . lift $ [Right 1]          -> [Right (Right 1)]
-- runExceptT . lift $ Right $ Right 1    -> Right (Right (Right 1))
-- runExceptT . lift $ Identity $ Right 1 -> Identity (Right (Right 1))
--
-- runExceptT . lift $ [Left 1]           -> [Right (Left 1)]
-- runExceptT . lift $ Identity $ Left 1  -> Identity (Right (Left 1))

-----------------------------------------------------------------------------------
