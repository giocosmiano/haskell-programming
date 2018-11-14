{-# LANGUAGE FlexibleInstances #-}

module FunctorExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Gen (oneof)

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

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ False' a,
           return $ True'  a]

instance (Eq a) => EqProp (BoolAndSomethingElse a) where (=-=) = eq

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

instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Falsish,
           return $ Truish a]

instance (Eq a) => EqProp (BoolAndMaybeSomethingElse a) where (=-=) = eq

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First  a,
           return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

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

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a c b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    oneof [return $ DeepBlue a c,
           return $ Something b]

instance (Eq a, Eq b, Eq c) => EqProp (Company a c b) where (=-=) = eq

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (More b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ L a b a,
           return $ R b a b]

instance (Eq a, Eq b) => EqProp (More b a) where (=-=) = eq

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Finance,
           return $ Desk  a,
           return $ Bloor b]

instance (Eq a, Eq b) => EqProp (Quant a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data K a b = K a deriving (Eq, Show)

-- e.g.
-- fmap (+2) $ K 5 -> K 5
instance Functor (K x) where
  fmap _ (K x) = K x

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return (K a)

instance (Eq a) => EqProp (K a b) where (=-=) = eq

-----------------------------------------------------------------------------------

newtype Flip f a b = Flip (f b a)
                   deriving (Eq, Show)

-- e.g.
-- fmap (++ " world") (Flip (K "hello")) -> Flip (K "hello world")
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

--instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
--  arbitrary = do
--    a <- arbitrary
--    b <- arbitrary
--    return (Flip (K b) a b)
--
--instance (Eq a, Eq b) => EqProp (Flip K a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data EvilGoateeConst a b = GoatyConst b
                         deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ GoatyConst 3 -> GoatyConst 4
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return (GoatyConst b)

instance (Eq b) => EqProp (EvilGoateeConst a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data LiftItOut f a = LiftItOut (f a)
                   deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ LiftItOut (Just 1)  -> LiftItOut (Just 2)
-- fmap (+1) $ LiftItOut [1, 2, 3] -> LiftItOut [2,3,4]
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

instance (Arbitrary a) => Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    a <- arbitrary
    return (LiftItOut (Just a))

instance (Eq a) => EqProp (LiftItOut Maybe a) where (=-=) = eq

-----------------------------------------------------------------------------------

data Parappa f g a = DaWrappa (f a) (g a)
                   deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ DaWrappa (Just 3) (Just 5) -> DaWrappa (Just 4) (Just 6)
-- fmap (+1) $ DaWrappa [4,5,6] [1,2,3]   -> DaWrappa [5,6,7] [2,3,4]
instance Functor f => Functor (Parappa f f) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance (Arbitrary a) => Arbitrary (Parappa Maybe Maybe a) where
  arbitrary = do
    a <- arbitrary
    return (DaWrappa (Just a) (Just a))

instance (Eq a) => EqProp (Parappa Maybe Maybe a) where (=-=) = eq

-----------------------------------------------------------------------------------

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
                       deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ IgnoringSomething (Just "hello") (Just 3) -> IgnoringSomething (Just "hello") (Just 6)
-- fmap (+1) $ IgnoringSomething "world" [1,2,3]         -> IgnoringSomething "world" [2,3,4]
instance Functor f => Functor (IgnoreOne f f a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe Maybe a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (IgnoringSomething (Just a) (Just b))

instance (Eq a, Eq b) => EqProp (IgnoreOne Maybe Maybe a b) where (=-=) = eq

-----------------------------------------------------------------------------------

data Notorious g o a t = Notorious (g o) (g a) (g t)
                       deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Notorious (Just "hello") (Just "world") (Just 2) -> Notorious (Just "hello") (Just "world") (Just 3)
-- fmap (+1) $ Notorious "hello" "world" [1,2]                  -> Notorious "hello" "world" [2,3]
instance Functor f => Functor (Notorious f a b) where
  fmap f (Notorious fa ga ha) = Notorious fa ga (fmap f ha)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Notorious Maybe a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Notorious (Just a) (Just b) (Just c))

instance (Eq a, Eq b, Eq c) => EqProp (Notorious Maybe a b c) where (=-=) = eq

-----------------------------------------------------------------------------------

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

-- e.g.
-- fmap (+1) $ Cons 1 (Cons 2 (Cons 3 Nil)) -> Cons 2 (Cons 3 (Cons 4 Nil))
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Nil,
           return $ Cons a Nil]

instance (Eq a) => EqProp (List a) where (=-=) = eq

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
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat a)       = OneGoat $ f a
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ NoGoat,
           return $ OneGoat a,
           return $ MoreGoats (OneGoat a) NoGoat (OneGoat a)]

instance (Eq a) => EqProp (GoatLord a) where (=-=) = eq

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
-- fmap (+1) $ Read strToInt "123" -> ???
instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x a) = Print x $ f a
  fmap f (Read g)    = Read $ fmap f g

strToInt :: String -> Int
strToInt x = read x :: Int

-----------------------------------------------------------------------------------

main = do
  putStrLn "\nquickBatch BoolAndSomethingElse"
  quickBatch $ functor (undefined :: BoolAndSomethingElse (Int, Double, Char))

  putStrLn "\nquickBatch BoolAndMaybeSomethingElse"
  quickBatch $ functor (undefined :: BoolAndMaybeSomethingElse (Int, Double, Char))

  putStrLn "\nquickBatch Sum"
  quickBatch $ functor (undefined :: Sum (Maybe String) (Int, Double, Char))

  putStrLn "\nquickBatch Company"
  quickBatch $ functor (undefined :: Company (Maybe String) [String] (Int, Double, Char))

  putStrLn "\nquickBatch More"
  quickBatch $ functor (undefined :: More [String] (Int, Double, Char))

  putStrLn "\nquickBatch Quant"
  quickBatch $ functor (undefined :: Quant (Maybe String) (Int, Double, Char))

  putStrLn "\nquickBatch K"
  quickBatch $ functor (undefined :: K (String, Int, Double, Char) (Int, Double, Char))

  putStrLn "\nquickBatch EvilGoateeConst"
  quickBatch $ functor (undefined :: EvilGoateeConst (String, Int, Double, Char) (Int, Double, Char))

  putStrLn "\nquickBatch LiftItOut"
  quickBatch $ functor (undefined :: LiftItOut Maybe (Int, Double, Char))

  putStrLn "\nquickBatch Parappa"
  quickBatch $ functor (undefined :: Parappa Maybe Maybe (Int, Double, Char))

  putStrLn "\nquickBatch IgnoreOne"
  quickBatch $ functor (undefined :: IgnoreOne Maybe Maybe Int (Int, Double, Char))

  putStrLn "\nquickBatch Notorious"
  quickBatch $ functor (undefined :: Notorious Maybe String Int (Int, Double, Char))

  putStrLn "\nquickBatch List"
  quickBatch $ functor (undefined :: List (Int, Double, Char))

  putStrLn "\nquickBatch GoatLord"
  quickBatch $ functor (undefined :: GoatLord (Int, Double, Char))
