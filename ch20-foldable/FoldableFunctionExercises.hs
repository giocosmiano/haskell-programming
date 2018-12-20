module FoldableExercises where

import Data.Monoid
import Data.Foldable (foldr, foldMap)

{-

class Foldable (t :: * -> *) where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-}
-----------------------------------------------------------------------------------

-- e.g.
-- let xs = ([1,2,3,4,5] :: [Sum Integer])
-- sum'  xs -> Sum {getSum = 15}
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- e.g.
-- let ys = ([1,2,3,4] :: [Sum Integer])
-- sum'' ys -> Sum {getSum = 10}
sum'' :: (Foldable t, Num a) => t a -> a
sum'' = getSum . foldMap Sum

-----------------------------------------------------------------------------------

-- e.g.
-- let x1 = ([1,2,3,4,5] :: [Product Integer])
-- product'  x1 -> Product {getProduct = 120}
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

-- e.g.
-- let x2 = ([1,2,3,4] :: [Product Integer])
-- product'' x2 -> Product {getProduct = 24}
product'' :: (Foldable t, Num a) => t a -> a
product'' = getProduct . foldMap Product

-----------------------------------------------------------------------------------

-- e.g.
-- elem'  2 (Just 3)          -> False
-- elem'  True (Left False)   -> False
-- elem'  True (Left True)    -> False
-- elem'  True (Right False)  -> False
-- elem'  True (Right True)   -> True

-- fmap (elem' 3) [Right 1,Right 2,Right 3, Left 3] -> [False,False,True,False]
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\y b -> y == x || b) False

-- e.g.
-- elem'' 2 (Just 3)          -> False
-- elem'' True (Left False)   -> False
-- elem'' True (Left True)    -> False
-- elem'' True (Right False)  -> False
-- elem'' True (Right True)   -> True

-- fmap (elem'' 3) [Right 1,Right 2,Right 3, Left 3] -> [False,False,True,False]
elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = getAny . foldMap (\y -> Any $ y == x)

-----------------------------------------------------------------------------------

-- e.g.
-- this function returns a Maybe so try these samples
-- using the `Foldable.minimum` function to see the difference

-- minimum'  [10,12,33,5]               -> Just 5
-- minimum'  [Just 2, Just 10, Just 4]  -> Just (Just 2)
-- minimum'  (Just [3,7,10,2])          -> Just [3,7,10,2]
-- minimum'  (Just "Gio")               -> Just "Gio"
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\x y -> go x y) Nothing
  where go x Nothing  = Just x
        go x (Just y) = Just $ min x y

-- e.g.
-- minimum'' [10,12,33,5]               -> Just 5
-- minimum'' [Just 2, Just 10, Just 4]  -> Just (Just 2)
-- minimum'' (Just [3,7,10,2])          -> Just [3,7,10,2]
-- minimum'' (Just "Gio")               -> Just "Gio"
newtype Minimum a = Minimum { getMinimum :: Maybe a }
                  deriving (Eq, Ord, Read, Show)

instance Ord a => Monoid (Minimum a) where
    mempty = Minimum Nothing

    Minimum Nothing `mappend` y = y
    x `mappend` Minimum Nothing = x
    Minimum (Just x) `mappend` Minimum (Just y) = Minimum $ Just $ min x y

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' = getMinimum . foldMap (Minimum . Just)

-----------------------------------------------------------------------------------

-- e.g.
-- this function returns a Maybe so try these samples
-- using the `Foldable.maximum` function to see the difference

-- maximum'  [10,12,33,5]               -> Just 33
-- maximum'  [Just 2, Just 10, Just 4]  -> Just (Just 10)
-- maximum'  (Just [3,7,10,2])          -> Just [3,7,10,2]
-- maximum'  (Just "Gio")               -> Just "Gio"
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\x y -> go x y) Nothing
  where go x Nothing  = Just x
        go x (Just y) = Just $ max x y

-- e.g.
-- maximum'' [10,12,33,5]               -> Just 33
-- maximum'' [Just 2, Just 10, Just 4]  -> Just (Just 10)
-- maximum'' (Just [3,7,10,2])          -> Just [3,7,10,2]
-- maximum'' (Just "Gio")               -> Just "Gio"
newtype Maximum a = Maximum { getMaximum :: Maybe a }
                  deriving (Eq, Ord, Read, Show)

instance Ord a => Monoid (Maximum a) where
    mempty = Maximum Nothing

    Maximum Nothing `mappend` y = y
    x `mappend` Maximum Nothing = x
    Maximum (Just x) `mappend` Maximum (Just y) = Maximum $ Just $ max x y

maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' = getMaximum . foldMap (Maximum . Just)

-----------------------------------------------------------------------------------

-- e.g.
-- null'  (Left 3)                  -> True
-- null'  []                        -> True
-- null'  Nothing                   -> True
-- null'  (1, 2)                    -> False
-- null'  [Just 1, Just 2, Nothing] -> False
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- e.g.
-- null'' (Left 3)                  -> True
-- null'' []                        -> True
-- null'' Nothing                   -> True
-- null'' (1, 2)                    -> False
-- null'' [Just 1, Just 2, Nothing] -> False
newtype Null = Null { getNull :: Bool }
             deriving (Eq, Ord, Read, Show)

instance Monoid Null where
  mempty = Null True
  mappend _ _ = Null False

null'' :: (Foldable t) => t a -> Bool
null'' = getNull . foldMap (\_ -> Null False)

-----------------------------------------------------------------------------------

-- e.g.
-- length'  (Left 3)                  -> 0
-- length'  []                        -> 0
-- length'  Nothing                   -> 0
-- length'  (1, 2)                    -> 1
-- length'  [Just 1, Just 2, Nothing] -> 3
-- length'  [Right 1,Right 2,Right 3, Left 3] -> 4
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ c -> c+1) 0

-- e.g.
-- length'' (Left 3)                  -> 0
-- length'' []                        -> 0
-- length'' Nothing                   -> 0
-- length'' (1, 2)                    -> 1
-- length'' [Just 1, Just 2, Nothing] -> 3
-- length'' [Right 1,Right 2,Right 3, Left 3] -> 4
newtype Length a = Length { getLength :: a }
               deriving (Eq, Ord, Read, Show)

instance (Num a) => Monoid (Length a) where
  mempty = Length 0
  Length x `mappend` Length y = Length $ x + y

length'' :: (Foldable t) => t a -> Int
length'' = getLength . foldMap (\_ -> Length 1)

-----------------------------------------------------------------------------------

-- e.g.
-- toList'  (Left 3)                          -> []
-- toList'  []                                -> []
-- toList'  Nothing                           -> []
-- toList'  (1, 2)                            -> [2]
-- toList'  (Just 3)                          -> [3]
-- toList'  [Just 1, Just 2, Nothing]         -> [Just 1,Just 2,Just 3]
-- toList'  [Right 1,Right 2,Right 3, Left 3] -> [Right 1,Right 2,Right 3,Left 3]
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- e.g.
-- toList'' (Left 3)                          -> []
-- toList'' []                                -> []
-- toList'' Nothing                           -> []
-- toList'' (1, 2)                            -> [2]
-- toList'' (Just 3)                          -> [3]
-- toList'' [Just 1, Just 2, Nothing]         -> [Just 1,Just 2,Just 3]
-- toList'' [Right 1,Right 2,Right 3, Left 3] -> [Right 1,Right 2,Right 3,Left 3]
toList'' :: (Foldable t) => t a -> [a]
toList'' = foldMap (\x -> [x])

-----------------------------------------------------------------------------------

-- fold in terms of using foldMap
-- e.g.
-- fold' ([1,2,3,4,5] :: [Sum Integer])     -> Sum {getSum = 15}
-- fold' ([1,2,3,4,5] :: [Product Integer]) -> Product {getProduct = 120}
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-----------------------------------------------------------------------------------

-- foldMap in terms of using foldr
-- e.g.
-- foldMap' Sum [1, 2, 3, 4]     -> Sum {getSum = 10}
-- foldMap' Product [1, 2, 3, 4] -> Product {getProduct = 24}
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> (f x) <> y) mempty

-----------------------------------------------------------------------------------
