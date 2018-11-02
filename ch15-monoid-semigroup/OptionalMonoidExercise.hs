module OptionalMonoidExercise where

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

-- getting a compile error due "Semigroup is a superclass of Monoid since base-4.11.0.0."
-- https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in
-- https://www.reddit.com/r/haskellquestions/comments/93cug0/is_semigroup_a_superclass_of_monoid/
-- therefore, per suggestion implement
-- (<>) in Semigroup AND
-- mappend in Monoid
instance Semigroup  a => Semigroup  (Optional a) where
  Nada <> x = x
  x <> Nada = x
  Only x <> Only y = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Nada `mappend` x = x
  x `mappend` Nada = x
  Only x `mappend` Only y = Only (x `mappend` y)

-- e.g.
-- Only (Sum 1) `mappend` Only (Sum 1) -> Only (Sum {getSum = 2})
-- Only (Product 4) `mappend` Only (Product 2) -> Only (Product {getProduct = 8})
-- Only (Sum 1) `mappend` Nada -> Only (Sum {getSum = 1})
-- Only [1] `mappend` Nada -> Only [1]
-- Nada `mappend` Only (Sum 1) -> Only (Sum {getSum = 1})
