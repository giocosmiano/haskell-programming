{-# LANGUAGE InstanceSigs #-}

module EitherTExercises where

-----------------------------------------------------------------------------------

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-----------------------------------------------------------------------------------

instance (Functor m) => Functor (EitherT e m) where

  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

instance (Applicative m) => Applicative (EitherT e m) where

  pure :: Applicative m => a -> EitherT e m a
  pure ma = EitherT $ (pure . pure) ma

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT maf) <*> (EitherT ma) = EitherT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        (Left  e) -> return $ Left e
        (Right x) -> runEitherT (f x)

-----------------------------------------------------------------------------------

instance (Foldable m) => Foldable (EitherT e m) where
  foldMap :: (Monoid mn, Foldable m) => (a -> mn) -> EitherT e m a -> mn
  foldMap f (EitherT ma) = (foldMap . foldMap) f ma

instance (Traversable m) => Traversable (EitherT e m) where
  traverse f (EitherT ma) = EitherT <$> (traverse . traverse) f ma

-----------------------------------------------------------------------------------

swapEither :: Either e a -> Either a e
swapEither (Left  e) = Right e
swapEither (Right a) = Left  a

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swapEither $ runEitherT (EitherT ma)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ma) =
  ma >>= \x ->
    case x of
      Left  e -> f e
      Right x -> g x

-----------------------------------------------------------------------------------
