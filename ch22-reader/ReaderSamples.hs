{-# LANGUAGE InstanceSigs #-}

module ReaderSamples where

import Data.Char
import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- Sample Functor Reader
-----------------------------------------------------------------------------------

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

-- e.g.
-- composed "Hello World" -> "DLROW OLLEH"
composed :: [Char] -> [Char]
composed = cap . rev

-- e.g.
-- fmapped "Hello World" -> "DLROW OLLEH"
fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- e.g.
-- tupled "Hello" -> ("HELLO","olleH")
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

-- e.g.
-- tupled' "World" -> ("dlroW","WORLD")
tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

-----------------------------------------------------------------------------------
-- Sample Applicative Reader
-----------------------------------------------------------------------------------

newtype HumanName = HumanName String
                  deriving (Eq, Show)

newtype DogName = DogName String
                deriving (Eq, Show)

newtype Address = Address String
                deriving (Eq, Show)

data Person = Person
            { humanName :: HumanName
            , dogName :: DogName
            , address :: Address
            } deriving (Eq, Show)

data Dog = Dog
         { dogsName :: DogName
         , dogsAddress :: Address
         } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

-- e.g.
-- getDog pers  -> Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-- getDog chris -> Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- e.g.
-- getDogR pers  -> Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-- getDogR chris -> Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
-- with Reader, using applicative
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- e.g.
-- getDogR' pers  -> Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-- getDogR' chris -> Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
-- with Reader, using Reader function
getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

-- e.g.
-- getDogR'' pers  -> Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-- getDogR'' chris -> Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
-- with Reader, using liftA2
getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address

-----------------------------------------------------------------------------------
-- Sample Monad Reader
-----------------------------------------------------------------------------------

-- Objective
-- 1) increment the values inside our structure
-- 2) also tell us the length of the value
-- see `frooty' and `frooty'` below as composition of bar` and `foo` functions

-- e.g.
-- foo increments the value inside the structure
-- foo (*4) 5           -> 21
-- foo length [1,2,3,4] -> 5
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

-- e.g.
-- bar generates a tuple with
-- fst - structure of the 1st argument
-- snd - length of the 2nd argument
--
-- bar [1,2,3,4] (Just 5) -> ([1,2,3,4],1)
-- bar [1,2,3,4] [5,6,7]  -> ([1,2,3,4],3)
-- bar (Just 5) [2,3,4]   -> (Just 5,3)
bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- e.g.
-- froot generates a tuple with
-- fst - list with values incremented by 1
-- snd - length of the list
--
-- froot [1,2,3,4] -> ([2,3,4,5],4)
-- froot [5,6,7]   -> ([6,7,8],3)
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

-- e.g.
-- barOne [1,2,3,4] -> ([1,2,3,4],4)
-- barOne $ Just 5  -> (Just 5,1)
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- e.g.
-- foo (*4) 5           -> 21
-- foo length [1,2,3,4] -> 5
barPlus r = (foo r, length r)

-- e.g.
-- frooty generates a tuple with
-- fst - list with values incremented by 1
-- snd - length of the list
--
-- frooty [1,2,3,4] -> ([2,3,4,5],4)
-- frooty [5,6,7]   -> ([6,7,8],3)
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- e.g.
-- frooty' generates a tuple, using a Reader
-- fst - list with values incremented by 1
-- snd - length of the list
--
-- frooty' [1,2,3,4] -> ([2,3,4,5],4)
-- frooty' [5,6,7]   -> ([6,7,8],3)
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

--fooBind :: (t2 -> t1) -> (t1 -> t2 -> t) -> t2 -> t
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

-- e.g.
-- getDogRM pers  -> Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-- getDogRM chris -> Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-----------------------------------------------------------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

-----------------------------------------------------------------------------------

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \x -> f (ra x)

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (Reader rab) <*> (Reader ra) = Reader $ \x -> rab x (ra x)

instance Monad (Reader r) where
  return = pure
  (Reader ra) >>= aRb = Reader $ \x -> runReader (aRb (ra x)) x

-- TODO: how-to implement quickBatch Reader???
{-
instance (Arbitrary r, CoArbitrary r,
          Arbitrary a, CoArbitrary a) => Arbitrary (Reader r a) where
  arbitrary = do
    a <- arbitrary
    return $ Reader $ \r -> a

instance (Eq r, Eq a) => EqProp (Reader r a) where (=-=) = eq
-}

-----------------------------------------------------------------------------------

-- search --> haskell applicative function checkers
-- https://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

-- https://hackage.haskell.org/package/checkers
-- https://hackage.haskell.org/package/checkers-0.4.11/docs/Test-QuickCheck-Classes.html

-- https://github.com/conal/checkers
-- https://github.com/conal/checkers/blob/master/src/Test/QuickCheck/Classes.hs

--main = do
--
--  putStrLn "\nTesting Applicative, Monad : Reader"
--  quickBatch $ functor (undefined :: Reader (Int, Double, Char))
--  quickBatch $ applicative (undefined :: Reader (Int, Double, Char))
--  quickBatch $ monad (undefined :: Reader (Int, Double, Char))
