module FunctorExercises where

-----------------------------------------------------------------------------------
data Bool = False | True
-- Can't write a Functor on Bool because it is a type constant and NOT a type constructor

-----------------------------------------------------------------------------------
data BoolAndSomethingElse a = False' a
                            | True' a
                            deriving (Eq, Show)

-- e.g.
-- fmap (>2) (False' 2) -> False' False
-- fmap (>1) (True'  2) -> True'  True
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' $ f a
  fmap f (True'  a) = True'  $ f a

-----------------------------------------------------------------------------------
data BoolAndMaybeSomethingElse a = Falsish
                                 | Truish a
                                 deriving (Eq, Show)

-- e.g.
-- fmap (>2) Falsish    -> Falsish
-- fmap (>1) (Truish 3) -> Truish True
instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish $ f a

-----------------------------------------------------------------------------------
newtype Mu f = InF { outF :: f (Mu f) }

--instance Functor Mu where
--  fmap f (InF g) = InF (f . g)

-----------------------------------------------------------------------------------

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

-- e.g.
-- fmap (+1) (First 3)                 -> First 3
-- fmap (++ " world") (Second "hello") -> Second "hello world"
instance Functor (Sum e) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second $ f b

-----------------------------------------------------------------------------------

data Company a c b = DeepBlue a c
                   | Something b
                   deriving (Eq, Show)

-- e.g.
-- fmap (+1) (DeepBlue 5 2)               -> DeepBlue 5 2
-- fmap (++ " world") (Something "hello") -> Something "hello world"
instance Functor (Company e e') where
  fmap _ (DeepBlue a c) = DeepBlue a c
  fmap f (Something b)  = Something $ f b

-----------------------------------------------------------------------------------

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

-- e.g.
-- fmap (+1) (L 1 2 3) -> L 2 2 4
-- fmap (+1) (R 1 2 3) -> R 1 3 3
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-----------------------------------------------------------------------------------

data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)

-- e.g.
-- fmap (+1) Finance   -> Finance
-- fmap (+1) $ Desk 3  -> Desk 3
-- fmap (+1) $ Bloor 3 -> Bloor 4
instance Functor (Quant x) where
  fmap _  Finance  = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor x) = Bloor $ f x

-----------------------------------------------------------------------------------

data K a b = K a deriving (Eq, Show)

-- e.g.
-- fmap (+2) $ K 5 -> K 5
instance Functor (K x) where
  fmap _ (K x) = K x
