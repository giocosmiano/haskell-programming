module Sample where

{-
 - Euclid's algorithm to find gcd
 -}
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

{-
 - Fibonacci sequence with helper function
 -}
fib :: Int -> Int
fib x = fib' x 0 1
    where fib' y a b
             | y == 0 = a -- previous value
             | y == 1 = b -- next value
             | otherwise = fib' (y-1) b (a+b)

{-
 - Functor fmap for Pair (a, b)
 -}

-- fmap :: (a -> b) -> Pair c a -> Pair c b
-- e.g. getPair $ fmap (*100) (Pair (2, 3))
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- fmap :: (a -> b) -> Pair c a -> Pair c b
-- e.g. getPair' $ fmap (*100) (Pair' (2, 3))
newtype Pair' a b = Pair' { getPair' :: (a, b) }

instance Functor (Pair' c) where
  fmap f (Pair' (x, y)) = Pair' (x, f y)
