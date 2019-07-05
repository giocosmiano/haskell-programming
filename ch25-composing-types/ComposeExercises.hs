{-# LANGUAGE InstanceSigs #-}

module ComposeExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------
-- | Data structure with 1-layer of structure in it
-- On the applicative implementation we're applying function `f`,
-- via applicative `(<*>)`, to the value inside a 1-layer structure
-----------------------------------------------------------------------------------

newtype One f a = One (f a)
                deriving (Eq, Show)

-- |
-- e.g.
-- fmap (*5) $ One (Just 7) -> One (Just 35)
instance Functor f => Functor (One f) where
  fmap :: (a -> b) -> One f a -> One f b
  fmap f (One fa) = One $ fmap f fa

-- |
-- e.g.
-- One (Just (*5)) <*> One (Just 7)      -> One (Just 35)
-- (+) <$> One (Just 7) <*> One (Just 9) -> One (Just 16)
instance (Applicative f) => Applicative (One f) where
  pure :: a -> One f a
  pure a = One $ pure a

  (<*>) :: One f (a -> b) -> One f a -> One f b
  (One f) <*> (One a) = One $ f <*> a

-- |
-- e.g.
-- import Data.Monoid
-- getSum $ sum $ One ([3,7,10,2]::[Sum Integer])          -> 22
-- getSum $ foldMap (+5) $ One ([3,7,10,2]::[Sum Integer]) -> 42
instance (Foldable f) => Foldable (One f) where
  foldMap :: (Monoid m, Foldable f) => (a -> m) -> One f a -> m
  foldMap f (One fa) = foldMap f fa

  foldr f z (One fa) = foldr f z fa

-- |
-- e.g.
-- import Data.Functor.Identity
-- traverse (Identity . (*5)) $ One [3,5,7] -> Identity (One [15,25,35])
instance (Traversable f) => Traversable (One f) where
  traverse :: Applicative fa => (a -> fa b) -> One f a -> fa (One f b)
  traverse f (One fa) = One <$> traverse f fa

instance (Eq (fa a)) => EqProp (One fa a) where (=-=) = eq

instance (Arbitrary (fa a), CoArbitrary (fa a)) => Arbitrary (One fa a) where
  arbitrary = do
    fa <- arbitrary
    return $ One fa

-----------------------------------------------------------------------------------
-- | Data structure with 3-layers of structure in it
-- The applicative implementation looks a little bit overwhelming but
-- essentially we're lifting a composed applicative functions ((<*>) . fmap (<*>)),
-- via fmap, so we can apply the function `f` to the value inside a 3-layers of structure
-----------------------------------------------------------------------------------

newtype Three f g h a = Three (f (g (h a)))
                      deriving (Eq, Show)

-- |
-- e.g.
-- fmap (*5) $ Three (Just [Just 7]) -> Three (Just [Just 35])
instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap :: (a -> b) -> Three f g h a -> Three f g h b
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- | OR ***
--  fmap f (Three fgha) = Three $ (fmap . fmap) (fmap f) fgha

-- | OR ***
--  fmap f (Three fgha) = Three $ fmap (fmap (fmap f)) fgha

-- |
-- e.g.
-- Three (Just ([Just (*5)])) <*> Three (Just [Just 7])    -> Three (Just [Just 35])
-- (+) <$> Three (Just [Just 7]) <*> Three (Just [Just 9]) -> Three (Just [Just 16])
instance (Applicative f, Applicative g, Applicative h) => Applicative (Three f g h) where
  pure :: a -> Three f g h a
  pure a = Three $ (pure . pure . pure) a -- or Three $ pure $ pure $ pure a

  (<*>) :: Three f g h (a -> b) -> Three f g h a -> Three f g h b
  (Three f) <*> (Three a) = Three $ (fmap ((<*>) . fmap (<*>)) f) <*> a

-- |
-- e.g.
-- import Data.Monoid
-- getProduct $ product $ Three (Just (Just ([3,5,7]::[Product Integer])))      -> 105
-- getProduct $ foldMap (+3) $ Three (Just (Just ([3,5,7]::[Product Integer]))) -> 480
instance (Foldable f, Foldable g, Foldable h) => Foldable (Three f g h) where
  foldMap :: (Monoid m, Foldable h) => (a -> m) -> Three f g h a -> m
  foldMap f (Three fgha) = (foldMap . foldMap . foldMap) f fgha

-- | OR ***
--  foldMap f (Three fgha) = (foldMap . foldMap) (foldMap f) fgha

-- | OR ***
--  foldMap f (Three fgha) = foldMap (foldMap (foldMap f)) fgha

-- |
-- e.g.
-- import Data.Functor.Identity
-- traverse (Identity . (*5)) $ Three (Just (Just [3,5,7])) -> Identity (Three (Just (Just [15,25,35])))
instance (Traversable f, Traversable g, Traversable h) => Traversable (Three f g h) where
  traverse :: Applicative fa => (a -> fa b) -> Three f g h a -> fa (Three f g h b)
  traverse f (Three fgha) = Three <$> (traverse . traverse . traverse) f fgha

-- | OR ***
--  traverse f (Three fgha) = Three <$> (traverse . traverse) (traverse f) fgha

-- | OR ***
--  traverse f (Three fgha) = Three <$> traverse (traverse (traverse f)) fgha

instance (Eq (f (g (h a)))) => EqProp (Three f g h a) where (=-=) = eq

instance (Arbitrary (f (g (h a))), CoArbitrary (f (g (h a)))) => Arbitrary (Three f g h a) where
  arbitrary = do
    f <- arbitrary
    return $ Three f

-----------------------------------------------------------------------------------
-- | Data structure with 4-layers of structure in it
-- The applicative implementation looks a little bit overwhelming but
-- essentially we're lifting a composed applicative functions ((<*>) . fmap (<*>)),
-- via fmap, so we can apply the function `f` to the value inside a 4-layers of structure
-----------------------------------------------------------------------------------

newtype Four f g h i a = Four (f (g (h (i a))))
                      deriving (Eq, Show)

-- |
-- e.g.
-- fmap (*5) $ Four [(Just [Just 7])] -> Four [Just [Just 35]]
instance (Functor f, Functor g, Functor h, Functor i) => Functor (Four f g h i) where
  fmap :: (a -> b) -> Four f g h i a -> Four f g h i b
  fmap f (Four fghia) = Four $ (fmap . fmap . fmap . fmap) f fghia

-- |
-- e.g.
-- Four [(Just ([Just (*5)]))] <*> Four [(Just [Just 7])]    -> Four [Just [Just 35]]
-- (+) <$> Four [(Just [Just 7])] <*> Four [(Just [Just 9])] -> Four [Just [Just 16]]
instance (Applicative f, Applicative g, Applicative h, Applicative i) => Applicative (Four f g h i) where
  pure :: a -> Four f g h i a
  pure a = Four $ (pure . pure . pure . pure) a -- or Four $ pure $ pure $ pure $ pure a

  (<*>) :: Four f g h i (a -> b) -> Four f g h i a -> Four f g h i b
  (Four f) <*> (Four a) = Four $ (fmap ((<*>) . fmap ((<*>) . fmap (<*>))) f) <*> a

-- |
-- e.g.
-- import Data.Monoid
-- getProduct $ product $ Four (Just (Just (Just ([3,5,7]::[Product Integer]))))      -> 105
-- getProduct $ foldMap (+3) $ Four (Just (Just (Just ([3,5,7]::[Product Integer])))) -> 480
instance (Foldable f, Foldable g, Foldable h, Foldable i) => Foldable (Four f g h i) where
  foldMap :: (Monoid m, Foldable h) => (a -> m) -> Four f g h i a -> m
  foldMap f (Four fghia) = (foldMap . foldMap . foldMap . foldMap) f fghia

-- |
-- e.g.
-- import Data.Functor.Identity
-- traverse (Identity . (*5)) $ Four (Just (Just (Just [3,5,7]))) -> Identity (Four (Just (Just (Just [15,25,35]))))
instance (Traversable f, Traversable g, Traversable h, Traversable i) => Traversable (Four f g h i) where
  traverse :: Applicative fa => (a -> fa b) -> Four f g h i a -> fa (Four f g h i b)
  traverse f (Four fghia) = Four <$> (traverse . traverse . traverse . traverse) f fghia

instance (Eq (f (g (h (i a))))) => EqProp (Four f g h i a) where (=-=) = eq

instance (Arbitrary (f (g (h (i a)))), CoArbitrary (f (g (h (i a))))) => Arbitrary (Four f g h i a) where
  arbitrary = do
    f <- arbitrary
    return $ Four f

-----------------------------------------------------------------------------------
-- | Data structure with 2-layers of structure in it
-- The applicative implementation is the same as the 3-layers structure above but
-- essentially we're lifting the applicative `(<*>)` function, via fmap,
-- so we can apply the function `f` to the value inside a 2-layers of structure
-----------------------------------------------------------------------------------

newtype Compose f g a = Compose { getCompose :: f (g a) }
                      deriving (Eq, Show)

-- |
-- e.g.
-- fmap (*5) $ Compose (Just [7, 11, 12]) -> Compose {getCompose = Just [35,55,60]}
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- | OR ***
--  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

-- |
-- e.g.
-- Compose (Just ([(*5)])) <*> Compose (Just [7, 9, 11])     -> Compose {getCompose = Just [35,45,55]}
-- (+) <$> Compose (Just [7]) <*> Compose (Just [9, 11, 12]) -> Compose {getCompose = Just [16,18,19]}
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a -- or Compose $ pure $ pure a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a

-- | OR ***
--  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
--  (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)

-- |
-- e.g.
-- import Data.Monoid
-- getProduct $ product $ Compose (Just ([5,7,9]::[Product Integer]))      -> 315
-- getProduct $ foldMap (+3) $ Compose (Just ([5,7,9]::[Product Integer])) -> 960
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m, Foldable g) => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- | OR ***
--  foldMap f (Compose fga) = foldMap (foldMap f) fga

-- |
-- e.g.
-- import Data.Functor.Identity
-- traverse (Identity . (*7)) $ Compose (Just [3,5,7]) -> Identity (Compose {getCompose = Just [21,35,49]})
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative fa => (a -> fa b) -> Compose f g a -> fa (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- | OR ***
--  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

instance (Eq (f (g a))) => EqProp (Compose f g a) where (=-=) = eq

instance (Arbitrary (f (g a)), CoArbitrary (f (g a))) => Arbitrary (Compose f g a) where
  arbitrary = do
    f <- arbitrary
    return $ Compose f

-----------------------------------------------------------------------------------

main = do

  putStrLn "\nTesting Applicative, Traversable : One"
  quickBatch $ functor (undefined :: One Maybe (Int, Double, Char))
  quickBatch $ applicative (undefined :: One Maybe (Int, Double, Char))
  quickBatch $ traversable (undefined :: One Maybe (Int, Double, [Int]))

  putStrLn "\nTesting Applicative, Traversable : Three"
  quickBatch $ functor (undefined :: Three Maybe Maybe [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: Three Maybe Maybe [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Three Maybe Maybe [] (Int, Double, [Int]))

  putStrLn "\nTesting Applicative, Traversable : Four"
  quickBatch $ functor (undefined :: Four Maybe Maybe Maybe [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: Four Maybe Maybe Maybe [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Four Maybe Maybe Maybe [] (Int, Double, [Int]))

  putStrLn "\nTesting Applicative, Traversable : Compose"
  quickBatch $ functor (undefined :: Compose Maybe [] (Int, Double, Char))
  quickBatch $ applicative (undefined :: Compose Maybe [] (Int, Double, Char))
  quickBatch $ traversable (undefined :: Compose Maybe [] (Int, Double, [Int]))

