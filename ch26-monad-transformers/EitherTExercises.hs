{-# LANGUAGE InstanceSigs #-}

module EitherTExercises where

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- import Data.Monoid
-- runEitherT $ return 1                     -> Right 1
-- runEitherT $ EitherT $ [Right 1]          -> [Right 1]
-- runEitherT $ EitherT $ Right $ Right 1    -> Right (Right 1)
-- runEitherT $ EitherT $ Identity $ Right 1 -> Identity (Right 1)
--
-- runEitherT $ EitherT $ [Left 1]           -> [Left 1]
-- runEitherT $ EitherT $ Identity $ Left 1  -> Identity (Left 1)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- runEitherT $ (*3) <$> EitherT [Right 3, Left 5, Right 7] -> [Right 9,Left 5,Right 21]
instance (Functor m) => Functor (EitherT e m) where

  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- runEitherT $ EitherT [Right (*3)] <*> EitherT [Right 5, Left 7]              -> [Right 15,Left 7]
-- runEitherT $ (*) <$> EitherT [Right 2, Left 3] <*> EitherT [Right 5, Left 7] -> [Right 10,Left 7,Left 3,Left 3]
instance (Applicative m) => Applicative (EitherT e m) where

  pure :: Applicative m => a -> EitherT e m a
  pure ma = EitherT $ (pure . pure) ma

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT maf) <*> (EitherT ma) = EitherT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- runEitherT $ EitherT [Right 5, Left 7] >>= return . (*3)               -> [Right 15,Left 7]
-- runEitherT $ EitherT [Right 5, Left 7] >>= return . \x -> [(x*5, x+3)] -> [Right [(25,8)],Left 7]
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

-- |
-- e.g.
-- import Data.Monoid
-- foldMap Product (EitherT [Right 5, Left 6, Right 7])                                           -> Product {getProduct = 35}
-- foldMap (*3) (EitherT [Right (5::Sum Integer), Left (6::Sum Integer), Right (7::Sum Integer)]) -> Sum {getSum = 36}
instance (Foldable m) => Foldable (EitherT e m) where
  foldMap :: (Monoid mn, Foldable m) => (a -> mn) -> EitherT e m a -> mn
  foldMap f (EitherT ma) = (foldMap . foldMap) f ma

-----------------------------------------------------------------------------------

instance (Traversable m) => Traversable (EitherT e m) where
  traverse :: Applicative fa => (a -> fa b) -> EitherT e m a -> fa (EitherT e m b)
  traverse f (EitherT ma) = EitherT <$> (traverse . traverse) f ma

-----------------------------------------------------------------------------------

swapEither :: Either e a -> Either a e
swapEither (Left  e) = Right e
swapEither (Right a) = Left  a

-- |
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
