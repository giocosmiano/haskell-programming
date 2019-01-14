{-# LANGUAGE InstanceSigs #-}

module ReaderTExercises where

-----------------------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-----------------------------------------------------------------------------------

-- e.g.
-- import Data.Functor.Identity
-- fmap (runReaderT $ ReaderT (+3)) (runReaderT $ ReaderT (*5)) (Identity 7) -> Identity 38
instance (Functor m) => Functor (ReaderT r m) where

  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT ma) = ReaderT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

-- e.g.
-- import Data.Functor.Identity
-- (+) <$> (runReaderT $ ReaderT (*3)) <*> (runReaderT $ ReaderT (+5)) $ (Identity 7) -> Identity 33
-- (*) <$> (runReaderT $ ReaderT (+3)) <*> (runReaderT $ ReaderT (*5)) $ (Identity 7) -> Identity 350
instance (Applicative m) => Applicative (ReaderT r m) where

  pure :: Applicative m => a -> ReaderT r m a
  pure ma = ReaderT $ (pure . pure) ma

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT maf) <*> (ReaderT ma) = ReaderT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

-- e.g.
-- import Data.Functor.Identity
-- (runReaderT $ ReaderT (+3)) >>= return . (runReaderT $ ReaderT (*5)) $ (Identity 7) -> Identity 50
-- (runReaderT $ ReaderT (*3)) >>= return . (runReaderT $ ReaderT (+5)) $ (Identity 7) -> Identity 26
instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      x <- rma r
      runReaderT (f x) r

-----------------------------------------------------------------------------------




