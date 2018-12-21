module FoldableExercises where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

data Constant a b = Constant b
                  deriving (Eq, Ord, Show)

-- e.g.
-- foldr (*) 2 (Constant 5) -> 10
instance Foldable (Constant a) where
  foldr f z (Constant a) = f a z
  foldMap f (Constant a) = f a

{-
instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a) where (=-=) = eq
-}


-----------------------------------------------------------------------------------

data Two a b = Two a b
             deriving (Eq, Show)

-- e.g.
-- foldr (*) 2 (Two "abc" (5:: Sum Integer))   -> Sum {getSum = 10}
-- foldMap (*2) (Two "abc" (5 :: Sum Integer)) -> Sum {getSum = 10}
instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldMap f (Two a b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data Three a b c = Three a b c
                 deriving (Eq, Show)

-- e.g.
-- foldr (*) 2 (Three "abc" [1,2,3] (5:: Sum Integer))   -> Sum {getSum = 10}
-- foldMap (*2) (Three "abc" [1,2,3] (5 :: Sum Integer)) -> Sum {getSum = 10}
instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z
  foldMap f (Three a b c) = f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-----------------------------------------------------------------------------------

data Three' a b = Three' a b b
                deriving (Eq, Show)

-- foldr vs foldl, see https://wiki.haskell.org/Fold

-- e.g.
-- foldr (*) 1 (Three' "abc" (3:: Product Integer) (5:: Product Integer)) -> Product {getProduct = 15}
-- foldMap Product (Three' "abc" 3 5)                                     -> Product {getProduct = 15}
instance Foldable (Three' a) where
  foldr f z (Three' a b b') = f b $ f b' z
  foldMap f (Three' a b b') = f b <> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a  <- arbitrary
    b  <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

-- e.g.
-- foldr (*) 2 (Four "abc" [1,2,3] (3:: Product Integer) (5:: Sum Integer))   -> Sum {getSum = 10}
-- foldMap (*2) (Four "abc" [1,2,3] (3:: Product Integer) (5 :: Sum Integer)) -> Sum {getSum = 10}
instance Foldable (Four a b c) where
  foldr f z (Four a b c d) = f d z
  foldMap f (Four a b c d) = f d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a  <- arbitrary
    b  <- arbitrary
    c  <- arbitrary
    d  <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-----------------------------------------------------------------------------------

data Four' a b = Four' a b b b
               deriving (Eq, Show)

-- foldr vs foldl, see https://wiki.haskell.org/Fold

-- e.g.
-- foldr (*) 1 (Four' "abc" (3:: Product Integer) (5:: Product Integer) (7:: Product Integer)) -> Product {getProduct = 105}
-- foldMap Product (Four' "abc" 3 5 7)                                                         -> Product {getProduct = 105}
instance Foldable (Four' a) where
  foldr f z (Four' a b b' b'') = f b $ f b' $ f b'' z
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a  <- arbitrary
    b  <- arbitrary
    return (Four' a b b b)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

-----------------------------------------------------------------------------------

-- *** NOTE *** foldable seems to be in the code but hasn't been exposed yet???
-- seems to be in the repo -> https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs
-- but not exposed yet?? -> https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

{-

main = do
  putStrLn "\nTesting Foldable : Constant"
  quickBatch $ foldable (undefined :: Constant (Int, Double, Char))

  putStrLn "\nTesting Foldable Two"
  quickBatch $ foldable (undefined :: Two [String] (Int, Double, Char))

  putStrLn "\nTesting Foldable Three"
  quickBatch $ foldable (undefined :: Three String (Maybe String) (Int, Double, Char))

  putStrLn "\nTesting Foldable Three'"
  quickBatch $ foldable (undefined :: Three' String (Int, Double, Char))

  putStrLn "\nTesting Foldable Four"
  quickBatch $ foldable (undefined :: Four String (Maybe String) [String] (Int, Double, Char))

  putStrLn "\nTesting Foldable Four'"
  quickBatch $ foldable (undefined :: Four' [String] (Int, Double, Char))

-}

