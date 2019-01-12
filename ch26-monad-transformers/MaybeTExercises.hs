{-# LANGUAGE InstanceSigs #-}

module MaybeTExercises where

-----------------------------------------------------------------------------------

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure ma = MaybeT $ (pure . pure) ma

  (MaybeT mab) <*> (MaybeT ma) = MaybeT $ (fmap (<*>) mab) <*> ma

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



