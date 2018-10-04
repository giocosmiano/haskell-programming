module EqInstances where

-- exercise # 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn v == TisAn v' = v == v'

-- exercise # 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'

-- exercise # 3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  TisAnInt v   == TisAnInt v'   = v == v'
  TisAString v == TisAString v' = v == v'

-- exercise # 4
data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

-- exercise # 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = x == x' && y == y'

-- exercise # 6
data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  ThisOne v == ThisOne v' = v == v'
  ThatOne v == ThatOne v' = v == v'

-- exercise # 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello v   == Hello v'   = v == v'
  Goodbye v == Goodbye v' = v == v'

