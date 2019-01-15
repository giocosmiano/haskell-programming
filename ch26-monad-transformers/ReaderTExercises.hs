{-# LANGUAGE InstanceSigs #-}

module ReaderTExercises where

-----------------------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-----------------------------------------------------------------------------------

-- e.g.
-- import Data.Monoid
-- import Data.Functor.Identity
-- fmap (runReaderT $ ReaderT (+3)) (runReaderT $ ReaderT (*5)) (7 :: Product Integer) -> Product {getProduct = 38}
-- fmap (runReaderT $ ReaderT (+3)) (runReaderT $ ReaderT (*5)) (Identity 7)           -> Identity 38
instance (Functor m) => Functor (ReaderT r m) where

  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT ma) = ReaderT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

-- e.g.
-- import Data.Monoid
-- import Data.Functor.Identity
-- (+) <$> (runReaderT $ ReaderT (*3)) <*> (runReaderT $ ReaderT (+5)) $ (7 :: Product Integer) -> Product {getProduct = 33}
-- (*) <$> (runReaderT $ ReaderT (+3)) <*> (runReaderT $ ReaderT (*5)) $ (Identity 7)           -> Identity 350
instance (Applicative m) => Applicative (ReaderT r m) where

  pure :: Applicative m => a -> ReaderT r m a
  pure ma = ReaderT $ (pure . pure) ma

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT maf) <*> (ReaderT ma) = ReaderT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

-- |
-- 1) Apply the prior computation/function `rma` to argument `r` to get the monadic structure `m a`
-- 2) Use `<-` to extract the value `a` from monadic structure `m a`
-- 3) Apply the current computation/function `f` to value `a`, resulting to `ReaderT r m b` -- see (>>=) signature
-- 4) Finally, unpack/extract the function `r -> m b` from `ReaderT r m b` and apply to `r`,
--    resulting to monadic structure `m b`, which will become the argument to `ReaderT $`

-- e.g.
-- import Data.Monoid
-- import Data.Functor.Identity
-- (runReaderT $ ReaderT (+3)) >>= return . (runReaderT $ ReaderT (*5)) $ (7 :: Sum Integer) -> Sum {getSum = 50}
-- (runReaderT $ ReaderT (*3)) >>= return . (runReaderT $ ReaderT (+5)) $ (Identity 7)       -> Identity 26
instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

-----------------------------------------------------------------------------------




