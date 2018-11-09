{-# LANGUAGE FlexibleInstances #-}

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

-----------------------------------------------------------------------------------

newtype Flip f a b = Flip (f b a)
                   deriving (Eq, Show)

-- e.g.
-- fmap (++ " world") (Flip (K "hello")) -> Flip (K "hello world")
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-----------------------------------------------------------------------------------

data EvilGoateeConst a b = GoatyConst b
                         deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ GoatyConst 3 -> GoatyConst 4
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

-----------------------------------------------------------------------------------

data LiftItOut f a = LiftItOut (f a)
                   deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ LiftItOut (Just 1)  -> LiftItOut (Just 2)
-- fmap (+1) $ LiftItOut [1, 2, 3] -> LiftItOut [2,3,4]
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-----------------------------------------------------------------------------------

data Parappa f g a = DaWrappa (f a) (g a)
                   deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ DaWrappa (Just 3) (Just 5) -> DaWrappa (Just 4) (Just 6)
-- fmap (+1) $ DaWrappa [4,5,6] [1,2,3]   -> DaWrappa [5,6,7] [2,3,4]
instance Functor f => Functor (Parappa f f) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-----------------------------------------------------------------------------------

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
                       deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ IgnoringSomething (Just "hello") (Just 3) -> IgnoringSomething (Just "hello") (Just 6)
-- fmap (+1) $ IgnoringSomething "world" [1,2,3]         -> IgnoringSomething "world" [2,3,4]
instance Functor f => Functor (IgnoreOne f f a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-----------------------------------------------------------------------------------

data Notorious g o a t = Notorious (g o) (g a) (g t)
                       deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Notorious (Just "hello") (Just "world") (Just 2) -> Notorious (Just "hello") (Just "world") (Just 3)
-- fmap (+1) $ Notorious "hello" "world" [1,2]                  -> Notorious "hello" "world" [2,3]
instance Functor f => Functor (Notorious f a b) where
  fmap f (Notorious fa ga ha) = Notorious fa ga (fmap f ha)

-----------------------------------------------------------------------------------

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Cons 1 (Cons 2 (Cons 3 Nil)) -> Cons 2 (Cons 3 (Cons 4 Nil))
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-----------------------------------------------------------------------------------

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Eq, Show)

-- e.g.
-- fmap (+1) NoGoat                                     -> NoGoat
-- fmap (+1) $ OneGoat 3                                -> OneGoat 4
-- fmap (+1) $ MoreGoats (OneGoat 3) NoGoat (OneGoat 5) -> MoreGoats (OneGoat 4) NoGoat (OneGoat 6)
instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat $ f a
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-----------------------------------------------------------------------------------

data TalkToMe' a = Halt'
                 | Print' String a
                 deriving (Show)

-- e.g.
-- fmap (+1) Halt'              -> Halt'
-- fmap (+1) $ Print' "hello" 3 -> Print' "hello" 4
instance Functor TalkToMe' where
  fmap _ Halt'                 = Halt'
  fmap f (Print' x a)          = Print' x $ f a

-----------------------------------------------------------------------------------

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

-- Prelude> :t read
-- read :: Read a => String -> a

-- e.g.
-- fmap (+1) Halt                  -> Halt
-- fmap (+1) $ Print "hello" 3     -> Print "hello" 4
-- fmap (+1) $ Read strToInt "123" ->
instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x a) = Print x $ f a
  fmap f (Read g)    = Read $ fmap f g

strToInt :: String -> Int
strToInt x = read x :: Int

