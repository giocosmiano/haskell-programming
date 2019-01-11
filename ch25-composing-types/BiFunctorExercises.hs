module BiFunctorExercises where

import Data.Bifunctor

-----------------------------------------------------------------------------------

{-
class Bifunctor p where

{-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-}

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- bimap  (+3) (*5) (Deux 5 7) -> Deux 8 35
-- first  (+3) (Deux 5 7)      -> Deux 8 7
-- second (*5) (Deux 5 7)      -> Deux 5 35
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where

  bimap f g (Deux a b) = Deux (f a) (g b)

  first f (Deux a b) = Deux (f a) b

  second f (Deux a b) = Deux a $ f b

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- bimap  (+3) (*5) (Const 5) -> Const 8
-- first  (+3) (Const 5)      -> Const 8
-- second (*5) (Const 5)      -> Const 5
data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where

  bimap f g (Const a) = Const $ f a

  first f (Const a) = Const $ f a

  second f (Const a) = Const a

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- bimap  (+3) (*5) (Drei 5 7 9) -> Drei 5 10 45
-- first  (+3) (Drei 5 7 9)      -> Drei 5 10 9
-- second (*5) (Drei 5 7 9)      -> Drei 5 7 45
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where

  bimap f g (Drei a b c) = Drei a (f b) (g c)

  first f (Drei a b c) = Drei a (f b) c

  second f (Drei a b c) = Drei a b $ f c

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- bimap  (+3) (*5) (SuperDrei 5 7) -> SuperDrei 5 10
-- first  (+3) (SuperDrei 5 7)      -> SuperDrei 5 10
-- second (*5) (SuperDrei 5 7)      -> SuperDrei 5 7
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where

  bimap f g (SuperDrei a b) = SuperDrei a $ f b

  first f (SuperDrei a b) = SuperDrei a $ f b

  second f (SuperDrei a b) = SuperDrei a b

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- bimap  (+3) (*5) (SemiDrei 5) -> SemiDrei 5
-- first  (+3) (SemiDrei 5)      -> SemiDrei 5
-- second (*5) (SemiDrei 5)      -> SemiDrei 5
data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where

  bimap f g (SemiDrei a) = SemiDrei a

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- bimap  (+3) (*5) (Quadzzz 5 7 9 11) -> Quadzzz 5 7 12 55
-- first  (+3) (Quadzzz 5 7 9 11)      -> Quadzzz 5 7 12 11
-- second (*5) (Quadzzz 5 7 9 11)      -> Quadzzz 5 7 9 55
data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where

  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

  first f (Quadzzz a b c d) = Quadzzz a b (f c) d

  second f (Quadzzz a b c d) = Quadzzz a b c $ f d

-----------------------------------------------------------------------------------

-- |
-- e.g.
-- first  (+3) (Left' 5)  -> Left' 8
-- second (*5) (Right' 7) -> Right' 35
data Either' a b = Left' a
                 | Right' b
                 deriving (Eq, Show)

instance Bifunctor (Either') where

  first f (Left' a) = Left' $ f a

  second f (Right' b) = Right' $ f b

-----------------------------------------------------------------------------------

