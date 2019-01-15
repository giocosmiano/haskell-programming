{-# LANGUAGE InstanceSigs #-}

module MaybeTExercises where

-----------------------------------------------------------------------------------

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-----------------------------------------------------------------------------------

-- e.g.
-- runMaybeT $ (*3) <$> MaybeT [Just 3, Just 5, Just 7] -> [Just 9,Just 15,Just 21]
instance (Functor m) => Functor (MaybeT m) where

  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-----------------------------------------------------------------------------------

-- e.g.
-- runMaybeT $ MaybeT [Just (*3), Just (+5)] <*> MaybeT [Just 5, Just 7]   -> [Just 15,Just 21,Just 10,Just 12]
-- runMaybeT $ (*) <$> MaybeT [Just 2, Just 3] <*> MaybeT [Just 5, Just 7] -> [Just 10,Just 14,Just 15,Just 21]
instance (Applicative m) => Applicative (MaybeT m) where

  pure :: Applicative m => a -> MaybeT m a
  pure ma = MaybeT $ (pure . pure) ma

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT maf) <*> (MaybeT ma) = MaybeT $ (fmap (<*>) maf) <*> ma

-----------------------------------------------------------------------------------

-- e.g.
-- runMaybeT $ MaybeT [Just 5, Nothing, Just 7] >>= return . (*3)               -> [Just 15,Nothing,Just 21]
-- runMaybeT $ MaybeT [Just 5, Nothing, Just 7] >>= return . \x -> [(x*5, x+3)] -> [Just [(25,8)],Nothing,Just [(35,10)]]
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

-- e.g.
-- import Data.Monoid
-- foldMap Product (MaybeT [Just 5, Nothing, Just 7])                            -> Product {getProduct = 35}
-- foldMap (*3) (MaybeT [Just (5::Sum Integer), Nothing, Just (7::Sum Integer)]) -> Sum {getSum = 36}
instance (Foldable m) => Foldable (MaybeT m) where
  foldMap :: (Monoid mn, Foldable m) => (a -> mn) -> MaybeT m a -> mn
  foldMap f (MaybeT ma) = (foldMap . foldMap) f ma

-----------------------------------------------------------------------------------

instance (Traversable m) => Traversable (MaybeT m) where
  traverse :: Applicative fa => (a -> fa b) -> MaybeT m a -> fa (MaybeT m b)
  traverse f (MaybeT ma) = MaybeT <$> (traverse . traverse) f ma

-----------------------------------------------------------------------------------



