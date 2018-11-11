{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}

module FunctorSamples where

{-
-----------------------------------------------------------------------------------

class Functor f where
  fmap :: (a -> b) -> f a -> f b

fmap OR <$>
  fmap  :: Functor f => (a -> b) -> f a -> f b
  (<$>) :: Functor f => (a -> b) -> f a -> f b

-----------------------------------------------------------------------------------

 (<$>) :: Functor f =>
          (a -> b) -> f a -> f b

  ($)  :: (a -> b) ->   a ->   b

-----------------------------------------------------------------------------------

type E e = Either e
type C e = Constant e
type I = Identity

-- Functor f =>
fmap :: (a -> b) -> f a -> f b
     :: (a -> b) -> [ ] a -> [ ] b
     :: (a -> b) -> Maybe a -> Maybe b
     :: (a -> b) -> E e a -> E e b
     :: (a -> b) -> (e, a) -> (e, b) -- will fmap the `snd` in tuple
     :: (a -> b) -> I a -> I b       -- Data.Functor.Identity
     :: (a -> b) -> C e a -> C e b   -- Data.Functor.Const
-}

{-
Prelude> :set -XTypeApplications

Prelude> :type fmap @Maybe
fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

Prelude> :type fmap @(Either _)
fmap @(Either _) :: (a -> b) -> Either t a -> Either t b
-}

-- Functor Laws
-- Identity => fmap id == id
-- Composition => fmap (f . g) == fmap f . fmap g

{-
 - Functor fmap for Pair (a, b)
 -}

-- fmap :: (a -> b) -> Pair c a -> Pair c b
-- e.g.
-- getPair $ fmap (*100) (Pair (2, 3)) -> (200,3)
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- fmap :: (a -> b) -> Pair c a -> Pair c b
-- e.g.
-- getPair' $ fmap (*100) (Pair' (2, 3)) -> (2,300)
newtype Pair' a b = Pair' { getPair' :: (a, b) }

instance Functor (Pair' c) where
  fmap f (Pair' (x, y)) = Pair' (x, f y)

-----------------------------------------------------------------------------------
-- sample of Functor within a Functor (Wrapper)
-----------------------------------------------------------------------------------

data Wrap f a = Wrap (f a)
              deriving (Eq, Show)

-- e.g.
-- fmap (+1) (Wrap (Just 1))  -> Wrap (Just 2)
-- fmap (+1) (Wrap [1, 2, 3]) -> Wrap [2,3,4]
instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-----------------------------------------------------------------------------------
-- sample of IO Functor
-----------------------------------------------------------------------------------

-- getLine :: IO String
-- read :: Read a => String -> a
getInt :: IO Int
getInt = fmap read getLine

{-
Prelude> fmap (const ()) getInt
10

Prelude> fmap (+1) getInt
10
11

Prelude> fmap (++ " and me too!") getLine
hello
"hello and me too!"
-}

-----------------------------------------------------------------------------------
-- sample of Functor Natural Transformation
-----------------------------------------------------------------------------------
{-

Prelude> :set -XRank2Types
Prelude> :{
  type Nat f g =
       forall a . f a -> g a
  :}

-}

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- e.g.
-- Prelude> maybeToList $ Just 5 -> [5]

-----------------------------------------------------------------------------------
-- sample of Flip Functor
-----------------------------------------------------------------------------------
data Tuple a b = Tuple a b
               deriving (Eq, Show)

newtype Flip f a b = Flip (f b a)
                   deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- e.g.
-- Prelude> fmap (+1) (Flip (Tuple 1 "blah")) -> Flip (Tuple 2 "blah")
