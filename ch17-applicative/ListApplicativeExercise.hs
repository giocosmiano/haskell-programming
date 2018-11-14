module ListApplicativeExercise where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Cons 1 (Cons 2 (Cons 3 Nil)) -> Cons 2 (Cons 3 (Cons 4 Nil))
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-----------------------------------------------------------------------------------
-- List Applicative is a cartesian product joining lists of lists
-----------------------------------------------------------------------------------

-- e.g.
-- (+) <$> [4,5,6]   <*> [1,2,3,4] -> [5,6,7,8,6,7,8,9,7,8,9,10]
-- (*) <$> [3,4,5,6] <*> [1,2,3]   -> [3,6,9,4,8,12,5,10,15,6,12,18]
--
-- [(+2), (*2)] <*> [3,2,4] -> [5,4,6,6,4,8]
-- []           <*> [3,2,4] -> []
-- [(+2), (*2)] <*> []      -> []
--
-- let f = Cons (+2) (Cons (*2) Nil)
-- let v = Cons 3 (Cons 2 (Cons 4 Nil))
-- f <*> v -> Cons 5 (Cons 4 (Cons 6 (Cons 6 (Cons 4 (Cons 8 Nil)))))
--
-- let f = Nil
-- let v = Cons 3 (Cons 2 (Cons 4 Nil))
-- f <*> v -> Nil
--
-- let f = Cons (+2) (Cons (*2) Nil)
-- let v = Nil
-- f <*> v -> Cons 5 (Cons 4 (Cons 6 (Cons 6 (Cons 4 (Cons 8 Nil)))))
instance Applicative List where
  pure x              = Cons x Nil
  Nil         <*> _   = Nil
  _           <*> Nil = Nil
  (Cons f fs) <*> xs  = (fmap f xs) `append` (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-----------------------------------------------------------------------------------
-- using flatMap with concat' and fmap
-----------------------------------------------------------------------------------

-- e.g.
-- fmap (\x -> [x, 9]) [1, 2, 3] -> [[1,9],[2,9],[3,9]]
--
-- let toMyList = foldr Cons Nil
-- let xs = toMyList [1, 2, 3]
-- let c = Cons
-- let f x = x `c` (9 `c` Nil)
--
-- flatMap f xs -> Cons 1 (Cons 9 (Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil)))))

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

-----------------------------------------------------------------------------------


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return (Cons a Nil)

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- type SSI = (List Char, List Int, Int)
-- :{
--    let trigger :: [SSI]
--        trigger = [(Cons 'X' Nil, Cons 5 Nil, 3)]
-- :}
-- quickBatch (applicative trigger)
