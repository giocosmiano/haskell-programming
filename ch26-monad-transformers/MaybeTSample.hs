{-# LANGUAGE InstanceSigs #-}

module MaybeTSample where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

-- | OR ***
--   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
--   (MaybeT ma) >>= f =
--     let aimb = join (fmap runMaybeT (fmap f ma))
--     in  MaybeT aimb

-- | OR ***
--   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
--   (MaybeT ma) >>= f =
--     let aimb = join (fmap (runMaybeT . f) ma)
--     in  MaybeT aimb

-- | OR ***
--   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
--   m >>= k =
--     MaybeT $ runMaybeT . k
--     =<< runMaybeT m

-----------------------------------------------------------------------------------

instance (Foldable m) => Foldable (MaybeT m) where
  foldMap f (MaybeT ma) = (foldMap . foldMap) f ma

instance (Traversable m) => Traversable (MaybeT m) where
  traverse f (MaybeT ma) = MaybeT <$> (traverse . traverse) f ma

-----------------------------------------------------------------------------------

{-

instance (Eq (ma a)) => EqProp (MaybeT ma a) where (=-=) = eq

instance (Arbitrary (ma a), CoArbitrary (ma a)) => Arbitrary (MaybeT ma a) where
  arbitrary = do
    ma <- arbitrary
    return $ MaybeT ma

--newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

main = do

  putStrLn "\nTesting Functor, Applicative, Monad, Traversable : MaybeT"
  quickBatch $ functor (undefined :: MaybeT Maybe (Int, Double, Char))
  quickBatch $ applicative (undefined :: MaybeT Maybe (Int, Double, Char))
  quickBatch $ monad (undefined :: MaybeT Maybe (Int, Double, Char))
  quickBatch $ traversable (undefined :: MaybeT Maybe (Int, Double, [Int]))

-}



