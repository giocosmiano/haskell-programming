{-# LANGUAGE FlexibleContexts #-}

module SkiFreeExercises where

import Control.Applicative
import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- |
-- S implementing Functor, Applicative, Monad and Traversable
-----------------------------------------------------------------------------------

data S fa a = S (fa a) a deriving (Eq, Show)

instance Functor fa => Functor (S fa) where
  fmap f (S fa a) = S (fmap f fa) (f a)

instance Applicative fa => Applicative (S fa) where
  pure a = S (pure a) a
  (S fa f) <*> (S fa' a) = S (fa <*> fa') (f a)

-- TODO: fix the implementation of `monad` S
instance (Monad fa) => Monad (S fa) where
  return = pure
--  (S fa a) >>= f = S (fa >>= f) (f a)
  (S fa a) >>= f =
    let S fa' a' = f a
    in  S fa' a'

instance Foldable fa => Foldable (S fa) where
  foldMap f (S fa a) = foldMap f fa `mappend` f a
  foldr f z (S fa a) = foldr f (f a z) fa

instance Traversable fa => Traversable (S fa) where
  traverse f (S fa a) = S <$> traverse f fa <*> f a

instance (Eq (fa a), Eq a) => EqProp (S fa a) where (=-=) = eq

instance (Arbitrary (fa a), CoArbitrary (fa a),
          Arbitrary a, CoArbitrary a) => Arbitrary (S fa a) where
  arbitrary = do
    fa <- arbitrary
    a  <- arbitrary
    return $ S (fa a) a

{-
instance ( Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n, Testable (n Property), EqProp a ) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)
-}

-----------------------------------------------------------------------------------
-- |
-- ST implementing Functor, Applicative, MonadT, and Traversable
-----------------------------------------------------------------------------------

newtype ST fa a = ST { runST :: fa a } deriving (Eq, Show)

instance Functor fa => Functor (ST fa) where
  fmap f (ST fa) = ST $ fmap f fa

instance Applicative fa => Applicative (ST fa) where
  pure = ST . pure
  (ST fa) <*> (ST fa') = ST $ (fa <*> fa')

instance (Monad fa) => Monad (ST fa) where
  return = pure
  (ST fa) >>= f =
    ST $ fa >>= runST . f

instance Foldable fa => Foldable (ST fa) where
  foldMap f (ST fa) = foldMap f fa
  foldr f z (ST fa) = foldr f z fa

instance Traversable fa => Traversable (ST fa) where
  traverse f (ST fa) = ST <$> traverse f fa

instance (Eq (fa a)) => EqProp (ST fa a) where (=-=) = eq

instance (Arbitrary (fa a), CoArbitrary (fa a)) => Arbitrary (ST fa a) where
  arbitrary = do
    fa <- arbitrary
    return $ ST fa

-----------------------------------------------------------------------------------

main = do
--  sample' (arbitrary :: Gen (S [] Int))

  putStrLn "\nTesting Functor, Applicative, Monad, Traversable : S"
  quickBatch $ functor (undefined :: S Maybe (Int, Double, Char))
  quickBatch $ applicative (undefined :: S Maybe (Int, Double, Char))
  quickBatch $ monad (undefined :: S Maybe (Int, Double, Char))
  quickBatch $ traversable (undefined :: S Maybe (Int, Double, [Int]))

  putStrLn "\nTesting Functor, Applicative, Monad, Traversable : ST"
  quickBatch $ functor (undefined :: ST Maybe (Int, Double, Char))
  quickBatch $ applicative (undefined :: ST Maybe (Int, Double, Char))
  quickBatch $ monad (undefined :: ST Maybe (Int, Double, Char))
  quickBatch $ traversable (undefined :: ST Maybe (Int, Double, [Int]))

-----------------------------------------------------------------------------------

