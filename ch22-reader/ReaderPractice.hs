{-# LANGUAGE InstanceSigs #-}

module ReaderPractice where

import Data.Maybe
import Control.Applicative

-----------------------------------------------------------------------------------
-- helper functions for this code
-----------------------------------------------------------------------------------

-- e.g.
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup 3 [(1,4), (2,5), (3,6)]  -> Just 6
-- lookup 4 [(1,4), (2,5), (3,6)]  -> Nothing

-- e.g.
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry (+) (3,5) -> 8
-- uncurry (*) (3,5) -> 15

-- e.g.
-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- sequenceA [Just 3, Just 2, Just 1] -> Just [3,2,1]
-- sequenceA [(>3), (<8), even] 7     -> [True,True,False]

-- e.g.
-- fromMaybe :: a -> Maybe a -> a
-- fromMaybe 3 (Just 5) -> 5
-- fromMaybe 3 Nothing  -> 3

-----------------------------------------------------------------------------------

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-----------------------------------------------------------------------------------

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-----------------------------------------------------------------------------------

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = \r -> (z' r, z' r)

-- e.g.
-- Prelude> x1
-- Just (6,9)
-- Prelude> x2
-- Nothing
-- Prelude> x3 3
-- (Just 9,Just 9)

-----------------------------------------------------------------------------------

-- e.g.
-- summed (5,3) -> 8
summed :: Num c => (c, c) -> c
summed = uncurry (+)
--summed = \r -> uncurry (+) r

-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = \r -> (r > 3) && (r < 8)

-- e.g.
-- sequA 5 -> [True,True,False]
-- sequA 4 -> [True,True,True]
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

summed' = summed <$> ((,) <$> xs <*> ys)

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed'
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z

  putStrLn $ "\nAdded few more practices"
  print $ all (==True) $ sequA 5
  print $ sequA $ fromMaybe 0 summed'
  print $ bolt $ fromMaybe 0 ys

-----------------------------------------------------------------------------------
-- When you run this in GHCi, your results should look like this
-----------------------------------------------------------------------------------
-- Prelude> main
-- Just [3,2,1]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- Just [6,9]
-- Just 15
-- Nothing
-- True
-- [True,False,False]

-----------------------------------------------------------------------------------
-- Added few more practices
-----------------------------------------------------------------------------------
-- 1. fold the boolean conjunction operator over the list of results of sequA (applied to some value).
-- 2. apply sequA to summed'; you’ll need fromMaybe.
-- 3. apply bolt to ys; you’ll need fromMaybe.

-- Added few more practices
-- False
-- [True,False,False]
-- False

