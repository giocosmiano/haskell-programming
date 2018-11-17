module ApplicativeUtilsSample ((<||>)) where

import Control.Applicative (liftA2)

-----------------------------------------------------------------------------------

-- It’s parallel application of the functions against an argument.
-- That application produces two values, so we monoidally combine
-- the two values so that we have a single value to return.
-- We’ve set up an environment so that two (a -> Bool) functions
-- that don’t have an a argument yet can return a result based on
-- those two Bool values when the combined function is eventually
-- applied against an a.

-----------------------------------------------------------------------------------

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

{-

Prelude> :t liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

Prelude> let f 9001 = True; f _ = False
Prelude> let g 42 = True; g _ = False

Prelude> :t f
f :: (Eq a, Num a) => a -> Bool

Prelude> f 42
False

Prelude> f 9001
True

Prelude> g 42
True

Prelude> g 9001
False

Prelude> (\n -> f n || g n) 0
False

Prelude> (\n -> f n || g n) 9001
True

Prelude> :t (\n -> f n || g n)
(\n -> f n || g n) :: (Eq a, Num a) => a -> Bool

Prelude> (f <||> g) 0
False

Prelude> (f <||> g) 9001
True

-}


