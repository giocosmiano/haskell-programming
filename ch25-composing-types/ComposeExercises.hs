{-# LANGUAGE InstanceSigs #-}

module ComposeExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- | Data structure with 1-layer of structure
-----------------------------------------------------------------------------------

newtype One f a = One (f a)
                deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

instance (Applicative f) => Applicative (One f) where
  pure :: a -> One f a
  pure a = One $ pure a

  (<*>) :: One f (a -> b) -> One f a -> One f b
  (One f) <*> (One a) = One $ f <*> a

instance (Foldable f) => Foldable (One f) where
  foldMap f (One fa) = foldMap f fa

instance (Traversable f) => Traversable (One f) where
  traverse f (One fa) = One <$> traverse f fa

instance (Eq (fa a)) => EqProp (One fa a) where (=-=) = eq

instance (Arbitrary (fa a), CoArbitrary (fa a)) => Arbitrary (One fa a) where
  arbitrary = do
    fa <- arbitrary
    return $ One fa

-----------------------------------------------------------------------------------
-- | Data structure with 3-layers of structure
-----------------------------------------------------------------------------------

newtype Three f g h a = Three (f (g (h a)))
                      deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

instance (Applicative f, Applicative g, Applicative h) => Applicative (Three f g h) where
  pure :: a -> Three f g h a
  pure a = Three $ (pure . pure . pure) a -- or Three $ pure $ pure $ pure a

  (<*>) :: Three f g h (a -> b) -> Three f g h a -> Three f g h b
  (Three f) <*> (Three a) = Three $ (fmap ((<*>) . fmap (<*>)) f) <*> a

instance (Foldable f, Foldable g, Foldable h) => Foldable (Three f g h) where
  foldMap f (Three fgha) = (foldMap . foldMap . foldMap) f fgha

instance (Traversable f, Traversable g, Traversable h) => Traversable (Three f g h) where
  traverse f (Three fgha) = Three <$> (traverse . traverse . traverse) f fgha

instance (Eq (f (g (h a)))) => EqProp (Three f g h a) where (=-=) = eq

instance (Arbitrary (f (g (h a))), CoArbitrary (f (g (h a)))) => Arbitrary (Three f g h a) where
  arbitrary = do
    f <- arbitrary
    return $ Three f

-----------------------------------------------------------------------------------
-- | Data structure with 2-layers of structure
-----------------------------------------------------------------------------------

newtype Compose f g a = Compose { getCompose :: f (g a) }
                      deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a -- or Compose $ pure $ pure a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

instance (Eq (f (g a))) => EqProp (Compose f g a) where (=-=) = eq

instance (Arbitrary (f (g a)), CoArbitrary (f (g a))) => Arbitrary (Compose f g a) where
  arbitrary = do
    f <- arbitrary
    return $ Compose f

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

main = do

  putStrLn "\nTesting Applicative, Traversable : One"
  quickBatch $ functor (undefined :: One [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: One [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: One [] (Int, Double, [Int]))

  putStrLn "\nTesting Applicative, Traversable : Three"
  quickBatch $ functor (undefined :: Three [] [] [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: Three [] [] [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Three [] [] [] (Int, Double, [Int]))

  putStrLn "\nTesting Applicative, Traversable : Compose"
  quickBatch $ functor (undefined :: Compose [] [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: Compose [] [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Compose [] [] (Int, Double, [Int]))

