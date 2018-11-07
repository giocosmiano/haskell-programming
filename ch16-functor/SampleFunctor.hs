module SampleFunctor where

{-
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
-- e.g. getPair $ fmap (*100) (Pair (2, 3)) -> (200,3)
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- fmap :: (a -> b) -> Pair c a -> Pair c b
-- e.g. getPair' $ fmap (*100) (Pair' (2, 3)) -> (2,300)
newtype Pair' a b = Pair' { getPair' :: (a, b) }

instance Functor (Pair' c) where
  fmap f (Pair' (x, y)) = Pair' (x, f y)
