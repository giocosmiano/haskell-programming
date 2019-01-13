{-# LANGUAGE InstanceSigs #-}

module MaybeTExercises where

-----------------------------------------------------------------------------------

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-----------------------------------------------------------------------------------

instance (Functor m) => Functor (MaybeT m) where

  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

instance (Applicative m) => Applicative (MaybeT m) where

  pure :: Applicative m => a -> MaybeT m a
  pure ma = MaybeT $ (pure . pure) ma

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT maf) <*> (MaybeT ma) = MaybeT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just x  -> runMaybeT (f x)

-----------------------------------------------------------------------------------

instance (Foldable m) => Foldable (MaybeT m) where
  foldMap f (MaybeT ma) = (foldMap . foldMap) f ma

instance (Traversable m) => Traversable (MaybeT m) where
  traverse f (MaybeT ma) = MaybeT <$> (traverse . traverse) f ma

-----------------------------------------------------------------------------------



