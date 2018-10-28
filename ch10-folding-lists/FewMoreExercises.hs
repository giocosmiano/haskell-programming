module FewMoreExercises where

-- same as Data.List (and)
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- same as Data.List (or)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- same as Data.List (any) e.g.
-- myAny even [1,3,5]     -> False
-- myAny even [1,3,5,2,4] -> True
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if (f a == True) then True else b) False

-- same as Data.List (all) e.g.
-- myAll even [1,3,5,2,4] -> False
-- myAll even [6,8,0,2,4] -> True
myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\a b -> if (f a == False) then False else b) True

-- same as Data.List (reverse) e.g.
-- myReverse [1,2,3,4,5] -> [5,4,3,2,1]
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- same as Data.List (concat) e.g.
-- squish [[1,2,3],[4,5,6],[7,8,9]] -> [1,2,3,4,5,6,7,8,9]
squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

-- e.g.
-- squishMap (\x -> [1, x, 3]) [2] -> [1,2,3]
-- squishMap (\x -> "WO "++[x]++" HOO ") "123" -> "WO 1 HOO WO 2 HOO WO 3 HOO "
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

-- same as Data.List (concat) but using squishMap e.g.
-- squishAgain [[1,2,3],[4,5,6],[7,8,9]] -> [1,2,3,4,5,6,7,8,9]
squishAgain :: [[a]] -> [a]
squishAgain = foldr (\x y -> squishMap (\a -> [a]) x ++ y) []

-- same as Data.List (maximum) e.g.
-- myMaximumBy compare [1,2,3,4,5,4,3,2,1] -> 5
-- myMaximumBy compare [4,3,2,1,5,1,2,3,4] -> 5
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = foldr (\a b -> if (f a b == GT) then a else b) x xs

-- same as Data.List (minimum) e.g.
-- myMinimumBy compare [5,4,3,2,1,2,3,4,5] -> 1
-- myMinimumBy compare [2,3,4,5,1,5,4,3,2] -> 1
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = foldr (\a b -> if (f a b == LT) then a else b) x xs

-- same as Data.List (maximum) using myMaximumBy e.g.
-- myMaximum [1,2,3,4,5,4,3,2,1] -> 5
-- myMaximum [4,3,2,1,5,1,2,3,4] -> 5
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

-- same as Data.List (minimum) using myMinimumBy e.g.
-- myMinimum [5,4,3,2,1,2,3,4,5] -> 1
-- myMinimum [2,3,4,5,1,5,4,3,2] -> 1
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

-- same as Data.List (sort) e.g.
-- sort' [5,4,3,2,1,5,4,3,2,1] -> [1,1,2,2,3,3,4,4,5,5]
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger         = filter (>  x) xs
  in  sort' smallerOrEqual ++ [x] ++ sort' larger

-- quickSort [5,4,3,2,1,5,4,3,2,1] -> [1,1,2,2,3,3,4,4,5,5]
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger         = [a | a <- xs, a >  x]
  in  quickSort smallerOrEqual ++ [x] ++ quickSort larger
