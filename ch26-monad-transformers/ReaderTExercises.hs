{-# LANGUAGE InstanceSigs #-}

module ReaderTExercises where

-----------------------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-----------------------------------------------------------------------------------

instance (Functor m) => Functor (ReaderT r m) where

  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT ma) = ReaderT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

instance (Applicative m) => Applicative (ReaderT r m) where

  pure ma = ReaderT $ (pure . pure) ma

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT maf) <*> (ReaderT ma) = ReaderT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      x <- rma r
      runReaderT (f x) r

-----------------------------------------------------------------------------------



