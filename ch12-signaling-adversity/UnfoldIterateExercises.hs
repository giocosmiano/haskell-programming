module UnfoldIterateExercises where

-- iterate is about the same pattern as foldr for list
-- foldr f z (x:xs) = f x (foldr f z xs)
-- see -> https://wiki.haskell.org/Fold
-- e.g.
-- take 10 $ myIterate (+1) 0 -> [0,1,2,3,4,5,6,7,8,9]
myIterate :: (a -> a) -> a -> [a]
myIterate f z = go f z
   where go f' z' = z' : go f' (f' z')

iterate' :: (a -> a) -> a -> [a]
iterate' f z = z : iterate' f (f z)

-- e.g.
-- take 10 $ myUnfoldr (\b -> Just (b, b+1)) 0 -> [0,1,2,3,4,5,6,7,8,9]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of
                Nothing      -> []
                Just (a, z') -> a : myUnfoldr f z'

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f z = go f z
   where go f' z' = case f' z' of
                    Nothing       -> []
                    Just (a, z'') -> a : unfoldr' f z''

-- e.g. but using myUnfoldr just above
-- take 10 $ betterIterate (+1) 0 -> [0,1,2,3,4,5,6,7,8,9]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr (\x -> Just (x, f x)) z
