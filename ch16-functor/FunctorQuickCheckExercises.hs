{-# LANGUAGE ViewPatterns #-}

module FunctorQuickCheckExercises where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

{-

Prelude> :{
  let f :: [Int] -> Bool
      f x = functorIdentity
  :}
Prelude> quickCheck f
+++ OK, passed 100 tests.

Prelude> let c = functorCompose (+1) (*2)
Prelude> let li x = c (x :: [Int])
Prelude> quickCheck li
+++ OK, passed 100 tests.

Prelude> type IntToInt = Fun Int Int
Prelude> type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
Prelude> let fc' = functorCompose'
Prelude> quickCheck (fc' :: IntFC)

-}

-----------------------------------------------------------------------------------

newtype Identity a = Identity a
                   deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Identity 5 -> Identity 6
instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

{-
Prelude> functorIdentity $ Identity 5 -> true
-}

-----------------------------------------------------------------------------------

data Pair a = Pair a a
            deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Pair 3 5 -> Pair 4 6
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- arbitrary generators
instance (Arbitrary a, Num a) => Arbitrary (Pair a) where
  arbitrary = elements [(Pair 3 5), (Pair 5 7), (Pair 7 9)]

{-
Prelude> sample (arbitrary :: Gen (Pair Int))

Prelude> functorIdentity $ Pair 3 5 -> true

Prelude> type PairToPair = Fun (Pair Int) (Pair Int)
Prelude> type PairFC = [Pair Int] -> PairToPair -> PairToPair -> Bool
Prelude> let fc' = functorCompose'
Prelude> quickCheck (fc' :: PairFC)
-}

-----------------------------------------------------------------------------------

data Two a b = Two a b
             deriving (Eq, Show)

-- e.g.
-- fmap (++ " world") $ Two 5 "hello" -> Two 5 "hello world"
instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

-----------------------------------------------------------------------------------

data Three a b c = Three a b c
                 deriving (Eq, Show)

-- e.g.
-- fmap (++ " world") $ Three Nothing 5 "hello" -> Three Nothing 5 "hello world"
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

-----------------------------------------------------------------------------------

data Three' a b = Three' a b b
                deriving (Eq, Show)

-- e.g.
-- fmap (+2) $ Three' Nothing 3 5 -> Three' Nothing 5 7
instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

-----------------------------------------------------------------------------------

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

-- e.g.
-- fmap (+2) $ Four Nothing 3 5 7 -> Four Nothing 3 5 9
instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y $ f z

-----------------------------------------------------------------------------------

data Four' a b = Four' a a a b
               deriving (Eq, Show)

-- e.g.
-- fmap (+2) $ Four' 2 3 5 7 -> Four' 2 3 5 9
instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' $ f y
