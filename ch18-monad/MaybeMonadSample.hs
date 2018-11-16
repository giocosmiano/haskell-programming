module MaybeMonadSample where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

-- like list comprehension (see chapter 9)
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []

-- e.g.
-- twiceWhenEven  [1..3] -> [1,4,4,9]
-- twiceWhenEven' [1..3] -> [4,4]

-----------------------------------------------------------------------------------
-- Maybe Monad
-----------------------------------------------------------------------------------

{-

(>>=) :: Monad m => m     a -> (a -> m b)     -> m     b
(>>=) ::            Maybe a -> (a -> Maybe b) -> Maybe b

-- same as pure
return :: Monad m => a -> m     a
return ::            a -> Maybe a

instance Monad Maybe where
  return x = Just x

  Nothing  >>= _ = Nothing
  (Just x) >>= k = k x

-}

-----------------------------------------------------------------------------------

data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in  if n == "Bess" && w > 499
      then Nothing
      else Just c

-- using stacked up nested case analysis
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

-- using do notation
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- using stacked up nested lambdas
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty ->
      weightCheck (Cow nammy agey weighty)

-- e.g.
-- mkSphericalCow "Bess" (-5) 500 -> Nothing
-- mkSphericalCow "Bess"   5  499 -> Just (Cow {name = "Bess", age = 5, weight = 499})

-----------------------------------------------------------------------------------

-- With the Maybe Applicative, each Maybe computation fails
-- or succeeds independently of each other. We’re lifting
-- functions that are also Just or Nothing over Maybe values
doSomething = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)

-- With the Maybe Monad, computations contributing to the
-- final result can choose to return Nothing based on previous
-- computations
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

doSomething'' n = do
  f n >>=
    \a ->
      g n >>=
        \b ->
          h n >>=
            \c ->
              pure (a, b, c)

-- e.g.
-- doSomething   5 -> (Just 5,Nothing,Just "101915")
-- doSomething'  5 -> Nothing
-- doSomething'' 5 -> Nothing

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

-----------------------------------------------------------------------------------
-- Maybe Laws
-----------------------------------------------------------------------------------

{-

1) Identity laws

   a. Right identity
      m >>= return = m

   b. Left identity
      return x >>= f = f x

2) Associativity Law

  (m >>= f) >>= g = m >>= (\x -> f x >>= g)

-}
