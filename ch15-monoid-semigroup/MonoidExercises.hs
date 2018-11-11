module MonoidExercises where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

-----------------------------------------------------------------------------------

assoc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
assoc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-----------------------------------------------------------------------------------

newtype Identity a = Identity a
                   deriving (Eq, Show)

-- e.g.
-- Identity ""                          -> Identity ""
-- Identity (Sum 5) <> Identity (Sum 7) -> Identity (Sum {getSum = 12})
-- Identity "abc" <> Identity "xyz"     -> Identity "abcxyz"
-- Identity "abc" <> Identity ""        -> Identity "abc"
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity a') = Identity (a <> a')

-- e.g.
-- Prelude> quickCheck (monoidAssoc         :: IdentityAssoc)
-- Prelude> quickCheck (monoidLeftIdentity  :: IdentityLeftId)
-- Prelude> quickCheck (monoidRightIdentity :: IdentityRightId)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc   = Identity String -> Identity String -> Identity String -> Bool
type IdentityLeftId  = Identity String -> Bool
type IdentityRightId = Identity String -> Bool

-----------------------------------------------------------------------------------

data Two a b = Two a b
             deriving (Eq, Show)

-- e.g.
-- Two (Sum 5) "hello" <> Two (Sum 7) " world" -> Two (Sum {getSum = 12}) "hello world"
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b) (Two a' b') = Two (a <> a') (b <> b')

-- e.g.
-- Prelude> quickCheck (monoidAssoc         :: TwoAssoc)
-- Prelude> quickCheck (monoidLeftIdentity  :: TwoLeftId)
-- Prelude> quickCheck (monoidRightIdentity :: TwoRightId)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc   = Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool
type TwoLeftId  = Two String (Sum Int) -> Bool
type TwoRightId = Two String (Sum Int) -> Bool

-----------------------------------------------------------------------------------

data Three a b c = Three a b c
                 deriving (Eq, Show)

-- e.g.
-- Three (Sum 5) "hello" (Product 2) <> Three (Sum 7) " world" (Product 3) -> Three (Sum {getSum = 12}) "hello world" (Product {getProduct = 6})
instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

-- e.g.
-- Prelude> quickCheck (monoidAssoc         :: ThreeAssoc)
-- Prelude> quickCheck (monoidLeftIdentity  :: ThreeLeftId)
-- Prelude> quickCheck (monoidRightIdentity :: ThreeRightId)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc   = Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Bool
type ThreeLeftId  = Three String (Sum Int) (Product Int) -> Bool
type ThreeRightId = Three String (Sum Int) (Product Int) -> Bool

-----------------------------------------------------------------------------------

data Four a b c d = Four a b c d
                  deriving (Eq, Show)

-- e.g.
-- Four (Sum 5) "hello" (Product 2) (Any False) <> Four (Sum 7) " world" (Product 3) (Any True) -> Four (Sum {getSum = 12}) "hello world" (Product {getProduct = 6}) (Any {getAny = True})
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

-- e.g.
-- Prelude> quickCheck (monoidAssoc         :: FourAssoc)
-- Prelude> quickCheck (monoidLeftIdentity  :: FourLeftId)
-- Prelude> quickCheck (monoidRightIdentity :: FourRightId)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc   = Four String (Sum Int) (Product Int) Any -> Four String (Sum Int) (Product Int) Any -> Four String (Sum Int) (Product Int) Any -> Bool
type FourLeftId  = Four String (Sum Int) (Product Int) Any -> Bool
type FourRightId = Four String (Sum Int) (Product Int) Any -> Bool

-----------------------------------------------------------------------------------

newtype BoolConj = BoolConj Bool
                 deriving (Eq, Show)

-- e.g.
-- BoolConj False <> BoolConj False -> BoolConj False
-- BoolConj True  <> BoolConj False -> BoolConj False
-- BoolConj False <> BoolConj True  -> BoolConj False
-- BoolConj True  <> BoolConj True  -> BoolConj True
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj True) (BoolConj True) = BoolConj True
  mappend _ _ = BoolConj False

-- e.g.
-- Prelude> quickCheck (monoidAssoc         :: BoolConjAssoc)
-- Prelude> quickCheck (monoidLeftIdentity  :: BoolConjLeftId)
-- Prelude> quickCheck (monoidRightIdentity :: BoolConjRightId)
instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return (BoolConj b)

type BoolConjAssoc   = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjLeftId  = BoolConj -> Bool
type BoolConjRightId = BoolConj -> Bool

-----------------------------------------------------------------------------------

newtype BoolDisj = BoolDisj Bool
                 deriving (Eq, Show)

-- e.g.
-- BoolDisj False <> BoolDisj False -> BoolDisj False
-- BoolDisj True  <> BoolDisj False -> BoolDisj True
-- BoolDisj False <> BoolDisj True  -> BoolDisj True
-- BoolDisj True  <> BoolDisj True  -> BoolDisj True
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj False) (BoolDisj False) = BoolDisj False
  mappend _ _ = BoolDisj True

-- e.g.
-- Prelude> quickCheck (monoidAssoc         :: BoolDisjAssoc)
-- Prelude> quickCheck (monoidLeftIdentity  :: BoolDisjLeftId)
-- Prelude> quickCheck (monoidRightIdentity :: BoolDisjRightId)
instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return (BoolDisj b)

type BoolDisjAssoc   = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjLeftId  = BoolConj -> Bool
type BoolDisjRightId = BoolConj -> Bool

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
instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  mappend (Combine f) (Combine g) = Combine (f <> g)

-----------------------------------------------------------------------------------

newtype Comp a = Comp { unComp :: (a -> a) }

-- e.g.
-- Prelude> let f = Comp $ \x -> "haskell " ++ x
-- Prelude> let g = Comp $ \x -> "fp " ++ x
--
-- Prelude> unComp (f <> g) $ "rocks. "
-- "haskell rocks. fp rocks. "
instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp (\_ -> mempty)
  mappend (Comp f) (Comp g) = Comp (f <> g)

-----------------------------------------------------------------------------------

main = do
  putStrLn "quickCheck IdentityAssoc, IdentityLeftId, IdentityRightId"
  quickCheck (monoidAssoc         :: IdentityAssoc)
  quickCheck (monoidLeftIdentity  :: IdentityLeftId)
  quickCheck (monoidRightIdentity :: IdentityRightId)

  putStrLn "\nquickCheck TwoAssoc, TwoLeftId, TwoRightId"
  quickCheck (monoidAssoc         :: TwoAssoc)
  quickCheck (monoidLeftIdentity  :: TwoLeftId)
  quickCheck (monoidRightIdentity :: TwoRightId)

  putStrLn "\nquickCheck ThreeAssoc, ThreeLeftId, ThreeRightId"
  quickCheck (monoidAssoc         :: ThreeAssoc)
  quickCheck (monoidLeftIdentity  :: ThreeLeftId)
  quickCheck (monoidRightIdentity :: ThreeRightId)

  putStrLn "\nquickCheck FourAssoc, FourLeftId, FourRightId"
  quickCheck (monoidAssoc         :: FourAssoc)
  quickCheck (monoidLeftIdentity  :: FourLeftId)
  quickCheck (monoidRightIdentity :: FourRightId)

  putStrLn "\nquickCheck BoolConjAssoc, BoolConjLeftId, BoolConjRightId"
  quickCheck (monoidAssoc         :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity  :: BoolConjLeftId)
  quickCheck (monoidRightIdentity :: BoolConjRightId)

  putStrLn "\nquickCheck BoolDisjAssoc, BoolDisjLeftId, BoolDisjRightId"
  quickCheck (monoidAssoc         :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity  :: BoolDisjLeftId)
  quickCheck (monoidRightIdentity :: BoolDisjRightId)
