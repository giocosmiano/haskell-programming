{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}

module ApplicativeSamples where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

apply or <*>
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b

fmap OR <$>
  fmap  :: Functor f     =>   (a -> b) -> f a -> f b
  (<$>) :: Functor f     =>   (a -> b) -> f a -> f b

-----------------------------------------------------------------------------------
Applicative vs Functor vs function application
-----------------------------------------------------------------------------------
  (<*>) :: f (a -> b) -> f a -> f b
  (<$>) ::   (a -> b) -> f a -> f b
   ($)  ::   (a -> b) ->   a ->   b

-----------------------------------------------------------------------------------
Applicative are monoidal functors
-----------------------------------------------------------------------------------
mappend :: Monoid a => a -> a -> a

mappend :: f               f      f
$       ::   (a -> b) ->   a ->   b
(<*>)   :: f (a -> b) ->   a ->   b
(<$>)   ::   (a -> b) -> f a -> f b

-----------------------------------------------------------------------------------
liftA is just an fmap but only with Applicative while liftA2/A3 involves with more arguments
-----------------------------------------------------------------------------------
liftA  :: Applicative f => (a -> b)           -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

e.g.
Prelude> (+) <$> [1, 2] <*> [3, 5] -> [4,6,5,7]
Prelude> liftA2 (+) [1, 2] [3, 5]  -> [4,6,5,7]

Prelude> (,) <$> [1, 2] <*> [3, 4] -> [(1,3),(1,4),(2,3),(2,4)]
Prelude> liftA2 (,) [1, 2] [3, 4]  -> [(1,3),(1,4),(2,3),(2,4)]
-}

{-
Prelude> :set -XTypeApplications

Prelude> :type (<*>) @[]
(<*>) @[] :: [a -> b] -> [a] -> [b]

Prelude> :type pure @[]
pure @[] :: a -> [a]
-}

-----------------------------------------------------------------------------------
-- Applicative Identity
-----------------------------------------------------------------------------------
{-
 (<*>) ::  f (a -> b) ->  f a ->  f b
 (<*>) :: Id (a -> b) -> Id a -> Id b

 pure :: a ->  f a
 pure :: a -> Id a
-}

newtype Identity a = Identity a
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

-- e.g.
-- const <$>          [1, 2, 3] <*>          [9, 9, 9] -> [1,1,1,2,2,2,3,3,3]
-- const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9] -> Identity [1,2,3]

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a  <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-----------------------------------------------------------------------------------
-- Applicative Constant *** Recall that Applicatives are monoidal functors
-----------------------------------------------------------------------------------
{-
 (<*>) ::   f (a -> b) ->   f a ->   f b
 (<*>) :: C e (a -> b) -> C e a -> C e b

 pure :: a ->   f a
 pure :: a -> C e a
-}

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x `mappend` y

-- e.g.
-- Constant "hello" <*> Constant " world" -> Constant {getConstant = "hello world"}
-- pure 1 :: Constant String Int          -> Constant {getConstant = ""}

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a  <- arbitrary
    return (Constant a)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

-----------------------------------------------------------------------------------

main = do
  putStrLn "\nTesting applicative Identity"
  quickBatch $ applicative (undefined :: Identity (Int, Double, Char))

  putStrLn "\nTesting applicative Constant"
  quickBatch $ applicative (undefined :: Constant [String] (Int, Double, Char))
