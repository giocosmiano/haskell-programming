module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

--  QuickCheck relies on a typeclass called Arbitrary and a newtype called Gen for generating its random data.
--  arbitrary is a value of type Gen

-- Prelude > :m Test.QuickCheck

-- Prelude > :t arbitrary
-- arbitrary :: Arbitrary a => Gen a

-- Prelude > :t sample
-- sample :: Show a => Gen a -> IO ()

-- Prelude > :t sample'
-- sample' :: Gen a -> IO [a]

-----------------------------------------------------------------------------------
-- some generator samples
-----------------------------------------------------------------------------------

-- choose :: System.Random.Random a
-- => (a, a) -> Gen a
-- elements :: [a] -> Gen a

-- e.g.
-- Prelude > sample genBool
genBool :: Gen Bool
genBool = choose (False, True)

-- e.g.
-- Prelude > sample genBool'
genBool' :: Gen Bool
genBool' = elements [False, True]

-- e.g.
-- Prelude > sample genOrdering
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

-- e.g.
-- Prelude > sample genChar
genChar :: Gen Char
genChar = elements ['a'..'z']

-- e.g.
-- Prelude > sample genString
genString :: Gen String
genString = do
  a <- arbitrary
  return a

-- OR
-- e.g.
-- Prelude > sample (arbitrary :: Gen String)
-- Prelude > sample genString'
genString' :: Gen String
genString' = arbitrary

-- e.g.
-- Prelude > sample (genTuple :: Gen (Int, String))
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- e.g.
-- Prelude > sample (genThreeple :: Gen (Maybe Int, Int, String))
genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

-- e.g.
-- Prelude > sample (genEither :: Gen (Either Int String))
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
-- e.g.
-- Prelude > sample (genMaybe :: Gen (Maybe String))
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-----------------------------------------------------------------------------------
-- arbitrary instance on type constant
-----------------------------------------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-----------------------------------------------------------------------------------
-- arbitrary instance on unary type
-----------------------------------------------------------------------------------
data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-----------------------------------------------------------------------------------
-- arbitrary instance on product type
-----------------------------------------------------------------------------------
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-----------------------------------------------------------------------------------
-- arbitrary instance on sum type
-----------------------------------------------------------------------------------
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-----------------------------------------------------------------------------------

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

-----------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn $ "----------------------------------------------------------------------------------"
  putStrLn $ "trivialGen"
  putStrLn $ "----------------------------------------------------------------------------------"
  sample trivialGen

  putStrLn $ "----------------------------------------------------------------------------------"
  putStrLn $ "identityGenInt"
  putStrLn $ "----------------------------------------------------------------------------------"
  sample identityGenInt

  putStrLn $ "----------------------------------------------------------------------------------"
  putStrLn $ "pairGenIntString"
  putStrLn $ "----------------------------------------------------------------------------------"
  sample pairGenIntString

  putStrLn $ "----------------------------------------------------------------------------------"
  putStrLn $ "sumGenCharInt"
  putStrLn $ "----------------------------------------------------------------------------------"
  sample sumGenCharInt

  putStrLn $ "----------------------------------------------------------------------------------"
  putStrLn $ "sumGenCharIntFirst"
  putStrLn $ "----------------------------------------------------------------------------------"
  sample sumGenCharIntFirst
