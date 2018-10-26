module FewMoreExercises where

-- same as Data.List (and)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

-- same as Data.List (or)
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- same as Data.List (any) e.g.
-- myAny even [1,3,5]     -> False
-- myAny even [1,3,5,2,4] -> True
myAny :: (a -> Bool) -> [a] -> Bool
myAny _    []   = False
myAny f (x:xs)
  | f x == True = True
  | otherwise   = myAny f xs

-- same as Data.List (all) e.g.
-- myAll even [1,3,5,2,4] -> False
-- myAll even [6,8,0,2,4] -> True
myAll :: (a -> Bool) -> [a] -> Bool
myAll _    []    = True
myAll f (x:xs)
  | f x == False = False
  | otherwise    = myAll f xs

-- same as Data.List (reverse) e.g.
-- myReverse [1,2,3,4,5] -> [5,4,3,2,1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- same as Data.List (concat) e.g.
-- squish [[1,2,3],[4,5,6],[7,8,9]] -> [1,2,3,4,5,6,7,8,9]
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- e.g.
-- squishMap (\x -> [1, x, 3]) [2] -> [1,2,3]
-- squishMap (\x -> "WO "++[x]++" HOO ") "123" -> "WO 1 HOO WO 2 HOO WO 3 HOO "
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- same as Data.List (concat) but using squishMap e.g.
-- squishAgain [[1,2,3],[4,5,6],[7,8,9]] -> [1,2,3,4,5,6,7,8,9]
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap (\a -> [a]) x ++ squishAgain xs

-- same as Data.List (maximum) e.g.
-- myMaximumBy compare [1,2,3,4,5,4,3,2,1] -> 5
-- myMaximumBy compare [4,3,2,1,5,1,2,3,4] -> 5
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs)
  | null xs       == True = x
  | f x (head xs) == GT   = myMaximumBy f $ x : tail xs
  | otherwise             = myMaximumBy f xs

-- same as Data.List (minimum) e.g.
-- myMinimumBy compare [5,4,3,2,1,2,3,4,5] -> 1
-- myMinimumBy compare [2,3,4,5,1,5,4,3,2] -> 1
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs)
  | null xs       == True = x
  | f x (head xs) == LT   = myMinimumBy f $ x : tail xs
  | otherwise             = myMinimumBy f xs

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
