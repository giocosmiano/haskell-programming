module SemigroupExercises where

import Data.Semigroup

import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

-----------------------------------------------------------------------------------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-----------------------------------------------------------------------------------

newtype Identity a = Identity a
                   deriving (Eq, Show)

-- e.g.
-- Identity (Sum 5) <> Identity (Sum 7) -> Identity (Sum {getSum = 12})
-- Identity "abc" <> Identity "xyz"     -> Identity "abcxyz"
instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: IdentityAssoc)
-- +++ OK, passed 100 tests.
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-----------------------------------------------------------------------------------


data Two a b = Two a b
             deriving (Eq, Show)

-- e.g.
-- Two (Sum 5) "hello" <> Two (Sum 7) " world" -> Two (Sum {getSum = 12}) "hello world"
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: TwoAssoc)
-- +++ OK, passed 100 tests.
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool

-----------------------------------------------------------------------------------

data Three a b c = Three a b c
                 deriving (Eq, Show)

-- e.g.
-- Three (Sum 5) "hello" (Product 2) <> Three (Sum 7) " world" (Product 3) -> Three (Sum {getSum = 12}) "hello world" (Product {getProduct = 6})
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: ThreeAssoc)
-- +++ OK, passed 100 tests.
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc = Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Bool

-----------------------------------------------------------------------------------

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

-- e.g.
-- Four (Sum 5) "hello" (Product 2) (Any False) <> Four (Sum 7) " world" (Product 3) (Any True) -> Four (Sum {getSum = 12}) "hello world" (Product {getProduct = 6}) (Any {getAny = True})
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: FourAssoc)
-- +++ OK, passed 100 tests.
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc = Four String (Sum Int) (Product Int) Any -> Four String (Sum Int) (Product Int) Any -> Four String (Sum Int) (Product Int) Any -> Bool

-----------------------------------------------------------------------------------

newtype BoolConj = BoolConj Bool
                 deriving (Eq, Show)

-- e.g.
-- BoolConj False <> BoolConj False -> BoolConj False
-- BoolConj True  <> BoolConj False -> BoolConj False
-- BoolConj False <> BoolConj True  -> BoolConj False
-- BoolConj True  <> BoolConj True  -> BoolConj True
instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: BoolConjAssoc)
-- +++ OK, passed 100 tests.
instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return (BoolConj b)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-----------------------------------------------------------------------------------

newtype BoolDisj = BoolDisj Bool
                 deriving (Eq, Show)

-- e.g.
-- BoolDisj False <> BoolDisj False -> BoolDisj False
-- BoolDisj True  <> BoolDisj False -> BoolDisj True
-- BoolDisj False <> BoolDisj True  -> BoolDisj True
-- BoolDisj True  <> BoolDisj True  -> BoolDisj True
instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: BoolDisjAssoc)
-- +++ OK, passed 100 tests.
instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return (BoolDisj b)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-----------------------------------------------------------------------------------

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

-- e.g.
-- this is a "sticky" Snd value sample
-- Fst 1 <> Snd 2 -> Snd 2
-- Fst 1 <> Fst 2 -> Fst 2
-- Snd 1 <> Fst 2 -> Snd 1
-- Snd 1 <> Snd 2 -> Snd 1
instance (Num a, Num b) => Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  _ <> (Snd b) = Snd b
  _ <> (Fst a) = Fst a
  (Fst a) <> _ = Fst a

-- e.g.
-- Prelude> quickCheck (semigroupAssoc :: OrAssoc)
-- +++ OK, passed 100 tests.
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a,
           return $ Snd b]

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-----------------------------------------------------------------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

-- e.g.
-- Prelude> let f = Combine $ \n -> Sum (n + 1)
-- Prelude> let g = Combine $ \n -> Sum (n - 1)
--
-- Prelude> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
--
-- Prelude> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
--
-- Prelude> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
--
-- Prelude> unCombine (g <> f) $ 1
-- Sum {getSum = 2}
instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

-----------------------------------------------------------------------------------

newtype Comp a = Comp { unComp :: (a -> a) }

-- e.g.
-- Prelude> let f = Comp $ \x -> "haskell " ++ x
-- Prelude> let g = Comp $ \x -> "fp " ++ x
--
-- Prelude> unComp (f <> g) $ "rocks. "
-- "haskell rocks. fp rocks. "
instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f <> g)

-----------------------------------------------------------------------------------

-- Renamed Success/Failure because they're colliding with
-- Test.QuickCheck.Success
-- Test.QuickCheck.Failure
data Validation a b = Failure' a
                    | Success' b
                    deriving (Eq, Show)

-- take note that type parameter `a` is the *ONLY* semigroup, and `b` is NOT
instance Semigroup a => Semigroup (Validation a b) where
  Success' x <> _          = Success' x
  _          <> Success' x = Success' x
  Failure' x <> Failure' y = Failure' (x <> y)

-- e.g.
-- in Prelude > main
-- Success 1
-- Failure "wootblah"
-- Success 1
-- Success 2
main = do
  let failure :: String -> Validation String Int
      failure = Failure'
      success :: Int -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
