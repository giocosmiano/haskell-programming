module SmallMaybeEitherExercises where

-- e.g.
-- isJust (Just 1) -> True
-- isJust Nothing  -> False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- e.g.
-- isNothing (Just 1) -> False
-- isNothing Nothing  -> True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- e.g.
-- mayybee 0 (+1) Nothing - > 0
-- mayybee 0 (+1) (Just 1) -> 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee _ f (Just x) = f x

-- e.g.
-- fromMaybe 0 Nothing  -> 0
-- fromMaybe 0 (Just 1) -> 1
fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe _ (Just x) = x

-- e.g.
-- listToMaybe [1, 2, 3] -> 1
-- listToMaybe [] -> Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

-- e.g.
-- maybeToList (Just 1) -> [1]
-- maybeToList Nothing  -> []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- e.g.
-- catMaybes [Just 1, Nothing, Just 2] -> [1,2]
-- catMaybes $ take 3 $ repeat Nothing -> []
catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

-- e.g.
-- flipMaybe [Just 1, Just 2, Just 3]  -> Just [1, 2, 3]
-- flipMaybe [Just 1, Nothing, Just 3] -> Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
   | length xs == length ys = Just ys
   | otherwise              = Nothing
   where ys = [x | Just x <- xs]

-- e.g.
-- lefts' [Left 1, Left 2, Left 3] -> [1, 2, 3]
lefts' :: [Either a b] -> [a]
lefts' xs = foldr (:) [] $ [x | Left x <- xs]

-- e.g.
-- rights' [Right 1, Right 2, Right 3] -> [1, 2, 3]
rights' :: [Either a b] -> [b]
rights' xs = foldr (:) [] $ [x | Right x <- xs]

-- e.g.
-- partitionEithers' [Left 1, Left 2, Left 3, Right 'a', Right 'b', Right 'c'] -> ([1,2,3],"abc")
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs =
   let ls = lefts'  xs
       rs = rights' xs
   in  (ls, rs)

-- e.g.
-- eitherMaybe' (+1) (Left 1)  -> Nothing
-- eitherMaybe' (+1) (Right 1) -> Just 2
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  e) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- e.g.
-- either' (+1) (+2) (Left 1)  -> 2
-- either' (+1) (+2) (Right 1) -> 3
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  e) = f e
either' _ g (Right x) = g x

-- e.g.
-- but using either' function just above
-- eitherMaybe'' (+1) (Left 1)  -> Nothing
-- eitherMaybe'' (+1) (Right 1) -> Just 2
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left e)  = Nothing
eitherMaybe'' f (Right x) = Just (either' id f (Right x))
