module ZipListApplicativeExercise where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- List Applicative is a cartesian product joining lists of lists
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

-- e.g.
-- take' 2 Nil -> Nil
-- take' 0 $ Cons 5 (Cons 4 (Cons 6 (Cons 6 (Cons 4 (Cons 8 Nil))))) -> Nil
-- take' 2 $ Cons 5 (Cons 4 (Cons 6 (Cons 6 (Cons 4 (Cons 8 Nil))))) -> Cons 5 (Cons 4 Nil)
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs)
  | n <= 0    = Nil
  | otherwise = Cons x $ take' (n-1) xs

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

newtype ZipList' a = ZipList' (List a)
                   deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x                            = ZipList' $ repeat' x
  _              <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' Nil) <*> _              = ZipList' Nil
  (ZipList' xs)  <*> (ZipList' ys)  = ZipList' (zipWith' id xs ys)


repeat' :: a -> List a
repeat' x  = xs
  where xs = Cons x xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _                   = Nil
zipWith' _ _ Nil                   = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return (ZipList' $ Cons a Nil)

--instance (Eq a) => EqProp (ZipList' a) where (=-=) = eq

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in  take' 3000 l
          ys' = let (ZipList' l) = ys
                in  take' 3000 l

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

main = do
  putStrLn "\nTesting applicative List"
  quickBatch $ applicative (undefined :: List (Int, Double, Char))

  putStrLn "\nTesting applicative ZipList'"
  quickBatch $ applicative (undefined :: ZipList' (Int, Double, Char))
