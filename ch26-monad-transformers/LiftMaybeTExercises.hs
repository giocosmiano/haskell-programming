{-# LANGUAGE InstanceSigs #-}

module LiftMaybeTExercises where

import Data.Functor.Identity
import Control.Monad.Trans.Maybe
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
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-----------------------------------------------------------------------------------

-- |
-- e.g.
-- runMaybeT $ return 1                   -> Just 1
-- runMaybeT $ MaybeT [Just 1]            -> [Just 1]
-- runMaybeT $ MaybeT $ Just $ Just 1     -> Just (Just 1)
-- runMaybeT $ MaybeT $ Identity $ Just 1 -> Identity (Just 1)

-- |
-- e.g.
-- runMaybeT . lift $ return 1          -> Just 1
-- runMaybeT . lift $ [Just 1]          -> [Just (Just 1)]
-- runMaybeT . lift $ Just $ Just 1     -> Just (Just (Just 1))
-- runMaybeT . lift $ Identity $ Just 1 -> Identity (Just (Just 1))

-----------------------------------------------------------------------------------



