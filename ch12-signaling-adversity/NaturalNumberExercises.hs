module NaturalNumberExercises where

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

-- e.g.
-- natToInteger Zero -> 0
-- natToInteger (Succ Zero) -> 1
-- natToInteger (Succ (Succ Zero)) -> 2
-- natToInteger (Succ (Succ (Succ Zero))) -> 3
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

-- e.g.
-- integerToNat (-1) -> Nothing
-- integerToNat 0    -> Just Zero
-- integerToNat 1    -> Just (Succ Zero)
-- integerToNat 2    -> Just (Succ (Succ Zero))
-- integerToNat 3    -> Just (Succ (Succ (Succ Zero)))
integerToNat :: Integer -> Maybe Nat
integerToNat (-1) = Nothing
integerToNat 0    = Just Zero
integerToNat n    = Just (go $ n - 1)
   where go 0 = Succ Zero
         go n = Succ $ go (n - 1)
