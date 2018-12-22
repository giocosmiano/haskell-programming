module TraversableExercises where

import Control.Applicative
import Data.Monoid
import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- Needs to fix the failed `interchange` test of Applicative Tree
instance Applicative Tree where
  pure = Leaf
  (Leaf f) <*> (Leaf x) = Leaf $ f x
  (Leaf f) <*> (Node l x r) = Node (fmap f l) (f x) (fmap f r)
--  (Node _ f _) <*> (Node l x r) = Node (fmap f l) (f x) (fmap f r)
  _ <*> _ = Empty

-- Needs to implement the `monad` of Applicative Tree
{-
instance Monad Tree where
  return = pure
  (Leaf x) >>= f = f x
  (Node l x r) >>= f = Node (l >>= f) x (r >>= f)
-}

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node l x r) =
    let left  = foldr f z l
        right = foldr f left r
    in  f x $ right

--  foldr _ z Nil = z
--  foldr f z (Cons x xs) = f x $ foldr f z xs

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Empty,
           return $ Leaf a,
           return $ Node (Leaf a) a (Leaf a)]

instance Eq a => EqProp (Tree a) where (=-=) = eq

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

  putStrLn "\nTesting Applicative, Monad, Traversable : Tree"
  quickBatch $ functor (undefined :: Tree (Int, Double, Char))
  quickBatch $ applicative (undefined :: Tree (Int, Double, Char))
--  quickBatch $ monad (undefined :: Tree (Int, Double, Char))
  quickBatch $ traversable (undefined :: Tree (Int, Double, [Int]))

