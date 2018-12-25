{-# LANGUAGE FlexibleContexts #-}

module SkiFreeExercises where

import Control.Applicative
import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

data S n a = S (n a) a deriving (Eq, Show)

instance Functor fa => Functor (S fa) where
  fmap f (S fa a) = S (fmap f fa) (f a)

instance Applicative fa => Applicative (S fa) where
  pure a = S (pure a) a
  (S f f') <*> (S a a') = S (f <*> a) (f' a')

-- TODO: implement the `monad` of S
--instance (Monad fa) => Monad (S fa) where
--  return = pure
--  (S fa a) >>= f = (fa >>= f) (a >>= f)

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

main = do
--  sample' (arbitrary :: Gen (S [] Int))

  putStrLn "\nTesting Functor, Traversable : S"
  quickBatch $ functor (undefined :: S [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: S [] (Int, Double, Char))
--  quickBatch $ monad (undefined :: S [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: S [] (Int, Double, [Int]))

-----------------------------------------------------------------------------------

