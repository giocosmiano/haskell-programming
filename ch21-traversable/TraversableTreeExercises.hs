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

-- TODO: How to implement the `applicative` Tree???
{-
instance Applicative Tree where
  pure x              = Node Empty x Empty
  (Node l f r) <*> xs = append (l <*> xs) (fmap f xs) (r <*> xs)
  _ <*> _             = Empty

createNode :: a -> Tree a
createNode x = Node Empty x Empty

insertNode :: (Ord a) => Tree a -> Tree a -> Tree a
insertNode x Empty = createNode x
insertNode x (Node left a right)
  | x == a = Node left a right
  | x < a  = Node (insertNode x left) a right
  | x > a  = Node left a (insertNode x right)

append :: Tree a -> Tree a -> Tree a -> Tree a
append Empty y Empty                         = y

append (Leaf x) (Leaf y) Empty               = Node (Leaf x) y Empty
append Empty (Leaf y) (Leaf x)               = Node Empty y (Leaf x)
append (Leaf x) (Leaf y) (Leaf z)            = Node (Leaf x) y (Leaf z)

append (Node l x r) (Leaf y) Empty           = Node (append l (Leaf x) r) y Empty
append (Node l x r) (Leaf y) (Leaf z)        = Node (append l (Leaf x) r) y (Leaf z)
append Empty        (Leaf y) (Node l x r)    = Node Empty y (append l (Leaf x) r)
append (Leaf z)     (Leaf y) (Node l x r)    = Node (Leaf z) y (append l (Leaf x) r)
append (Node l x r) (Leaf y) (Node l' x' r') = Node (append l (Leaf x) r) y (append l' (Leaf x') r')
-}

-- TODO: How to implement the `monad` Tree???
{-
instance Monad Tree where
  return = pure
  (Leaf x) >>= f = f x
  (Node l x r) >>= f = Node (l >>= f) (f x) (r >>= f)
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

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Empty,
           return $ Leaf a,
           return $ Node (Leaf a) a (Leaf a),
           return $ Node (Node (Leaf a) a (Leaf a)) a (Node (Leaf a) a (Leaf a))]

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
--  quickBatch $ applicative (undefined :: Tree (Int, Double, Char))
--  quickBatch $ monad (undefined :: Tree (Int, Double, Char))
  quickBatch $ traversable (undefined :: Tree (Int, Double, [Int]))

