module EitherMonadExercise where

import Control.Monad (ap)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Gen (oneof)

-----------------------------------------------------------------------------------
-- Either Monad
-----------------------------------------------------------------------------------

{-

(>>=) :: Monad m => m        a -> (a -> m        b) -> m        b
(>>=) ::            Either e a -> (a -> Either e b) -> Either e b

-- same as pure
return :: Monad m => a -> m        a
return ::            a -> Either e a

instance Monad (Either e) where
  return x = Right x

  (Left e)  >>= _ = Left e
  (Right x) >>= k = k x

-}

-----------------------------------------------------------------------------------

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
         founded :: Founded
       , programmers :: Coders
       } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders
                  deriving (Eq, Show)
                  
validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
  then Left  $ TooManyCodersForYears founded programmers
  else Right $ Shop founded programmers

{-

Note that Either Monad always short-circuits on the first thing to
have failed.

It *must* because in the Monad, later values can depend
on previous ones.

Note that Either Monad is unlike Applicative Validation wherein the
error part is a Monoid and thus accumulates

-}

-- e.g.
-- mkSoftware 30 2 -> Right (Shop {founded = 30, programmers = 2})
-- mkSoftware 30 5 -> Left (TooManyCodersForYears 30 5)

-----------------------------------------------------------------------------------
-- There's *NO* Validation Monad, unlike Applicative Validation
-----------------------------------------------------------------------------------

{-

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
ap    :: Monad m       => m (a -> b) -> m a -> m b

So, essentially -> <*> == ap

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m m' = do
  x <- m
  x' <- m'
  return (x x')

-----------------------------------------------------------------------------------
    The problem is you can’t make a Monad for Validation that
    accumulates the errors like the Applicative does. Instead, any
    Monad instance for Validation would be identical to Either’s Monad
    instance.
-----------------------------------------------------------------------------------

-}

-----------------------------------------------------------------------------------

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  (First x)  <*> _          = First x
  _          <*> (First x)  = First x
  (Second f) <*> (Second x) = Second $ f x

instance Monad (Sum a) where
  return = pure
  (First x)  >>= _ = First x
  (Second x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a,
           return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-----------------------------------------------------------------------------------

main = do
  putStrLn "\nTesting Functor : Sum"
  quickBatch $ functor (undefined :: Sum [String] (Int, Double, Char))

  putStrLn "\nTesting Applicative : Sum"
  quickBatch $ applicative (undefined :: Sum [String] (Int, Double, Char))

  putStrLn "\nTesting Monad : Sum"
  quickBatch $ monad (undefined :: Sum [String] (Int, Double, Char))
