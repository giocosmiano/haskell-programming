module ZippingExercises where

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- e.g.
-- zip' ['a','b','c','d','e'] [1,2,3] -> [('a',1),('b',2),('c',3)]
-- zipWith' (*) [3,4,5,6] [2,3] -> [6,12]
