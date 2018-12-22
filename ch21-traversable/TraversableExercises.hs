module TraversableExercises where

import Control.Applicative
import Data.Monoid
import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-----------------------------------------------------------------------------------

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  Constant x <*> Constant x' = Constant $ x `mappend` x'

{-
There's no Monad instance of Constant
see http://hackage.haskell.org/package/transformers-0.5.5.0/docs/Data-Functor-Constant.html
instance Monoid a => Monad (Constant a) where
  return = pure
  Constant a >>= f = Constant a
-}

instance Foldable (Constant a) where
  foldr f z _ = z
  foldMap f _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = Constant <$> pure a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data Optional a = Nada | Yep a
                deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Applicative Optional where
  pure = Yep
  _     <*> Nada  = Nada
  Yep f <*> Yep x = Yep $ f x

instance Monad Optional where
  return = pure
  (Yep a) >>= f = f a

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep y) = f y

  foldr _ z Nada    = z
  foldr f z (Yep y) = f y z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = Yep <$> arbitrary

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

-----------------------------------------------------------------------------------

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x              = Cons x Nil
  Nil         <*> _   = Nil
  _           <*> Nil = Nil
  (Cons f fs) <*> xs  = (fmap f xs) `append` (fs <*> xs)

instance Monad List where
  return = pure
  (Cons x xs) >>= f = f x `append` flatMap f xs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x $ foldr f z xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return (Cons a Nil)

instance Eq a => EqProp (List a) where (=-=) = eq

-----------------------------------------------------------------------------------

data Three a b c = Three a b c
                 deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three x y f) <*> (Three x' y' z) = Three (x `mappend` x') (y `mappend` y') $ f z

instance (Monoid a, Monoid b) => Monad (Three a b) where
  return = pure
  Three x y z >>= f =
    let Three x' y' z' = f z
    in  Three (x `mappend` x') (y `mappend` y') z'

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-----------------------------------------------------------------------------------

data Pair a b = Pair a b
              deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance (Monoid a) => Applicative (Pair a) where
  pure x = Pair mempty x
  (Pair x f) <*> (Pair x' z) = Pair (x `mappend` x') $ f z

instance (Monoid a) => Monad (Pair a) where
  return = pure
  Pair x y >>= f =
    let Pair x' y' = f y
    in  Pair (x `mappend` x') y'

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b
  foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

-----------------------------------------------------------------------------------

-- *** NOTE ***
-- When you have more than one value of type b, you’ll want
-- to use Monoid and Applicative for the Foldable and Traversable
-- instances respectively.

data Big a b = Big a b b
             deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) $ f b'

instance (Monoid a) => Applicative (Big a) where
  pure x = Big mempty x x
  (Big x f f') <*> (Big x' y y') = Big (x <> x') (f y) $ f' y'

instance (Monoid a) => Monad (Big a) where
  return = pure
  Big x y z >>= f =
    let Big x' y z' = f z
    in  Big (x <> x') y z'

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'
  foldr f z (Big a b b') = f b $ f b' z

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a  <- arbitrary
    b  <- arbitrary
    return (Big a b b)

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

-----------------------------------------------------------------------------------

-- *** NOTE ***
-- When you have more than one value of type b, you’ll want
-- to use Monoid and Applicative for the Foldable and Traversable
-- instances respectively.

data Bigger a b = Bigger a b b b
                deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') $ f b''

instance (Monoid a) => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  (Bigger x f f' f'') <*> (Bigger x' y y' y'') = Bigger (x <> x') (f y) (f' y') $ f'' y''

instance (Monoid a) => Monad (Bigger a) where
  return = pure
  Bigger w x y z >>= f =
    let Bigger w' x y z' = f z
    in  Bigger (w <> w') x y z'

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''
  foldr f z (Bigger a b b' b'') = f b $ f b' $ f b'' z

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a  <- arbitrary
    b  <- arbitrary
    return (Bigger a b b b)

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

-----------------------------------------------------------------------------------

main = do

  putStrLn "\nTesting Applicative, Monad, Traversable : Identity"
  quickBatch $ applicative (undefined :: Identity (Int, Double, Char))
  quickBatch $ monad (undefined :: Identity (Int, Double, Char))
  quickBatch $ traversable (undefined :: Identity (Int, Double, [Int]))

-----------------------------------------------------------------------------------
--There's no Monad instance of Constant
--see http://hackage.haskell.org/package/transformers-0.5.5.0/docs/Data-Functor-Constant.html

  putStrLn "\nTesting Applicative, Monad, Traversable : Constant"
  quickBatch $ applicative (undefined :: Constant [String] (Int, Double, Char))
--  quickBatch $ monad (undefined :: Constant [String] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Constant [String] (Int, Double, [Int]))

-----------------------------------------------------------------------------------

  putStrLn "\nTesting Applicative, Monad, Traversable : Optional"
  quickBatch $ applicative (undefined :: Optional (Int, Double, Char))
  quickBatch $ monad (undefined :: Optional (Int, Double, Char))
  quickBatch $ traversable (undefined :: Optional (Int, Double, [Int]))

-----------------------------------------------------------------------------------

  putStrLn "\nTesting Applicative, Monad, Traversable : List"
  quickBatch $ applicative (undefined :: List (Int, Double, Char))
  quickBatch $ monad (undefined :: List (Int, Double, Char))
  quickBatch $ traversable (undefined :: List (Int, Double, [Int]))

-----------------------------------------------------------------------------------

  putStrLn "\nTesting Applicative, Monad, Traversable : Three"
  quickBatch $ applicative (undefined :: Three [String] (Maybe String) (Int, Double, Char))
  quickBatch $ monad (undefined :: Three [String] (Maybe String) (Int, Double, Char))
  quickBatch $ traversable (undefined :: Three [String] (Maybe String) (Int, Double, [Int]))

-----------------------------------------------------------------------------------

  putStrLn "\nTesting Applicative, Monad, Traversable : Pair"
  quickBatch $ applicative (undefined :: Pair [String] (Int, Double, Char))
  quickBatch $ monad (undefined :: Pair [String] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Pair [String] (Int, Double, [Int]))

-----------------------------------------------------------------------------------

  putStrLn "\nTesting Applicative, Monad, Traversable : Big"
  quickBatch $ applicative (undefined :: Big [String] (Int, Double, Char))
  quickBatch $ monad (undefined :: Big [String] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Big [String] (Int, Double, [Int]))

-----------------------------------------------------------------------------------

  putStrLn "\nTesting Applicative, Monad, Traversable : Bigger"
  quickBatch $ applicative (undefined :: Bigger [String] (Int, Double, Char))
  quickBatch $ monad (undefined :: Bigger [String] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Bigger [String] (Int, Double, [Int]))

-----------------------------------------------------------------------------------

