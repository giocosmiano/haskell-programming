module KleisliCompositionSample where

import Control.Monad ((>=>))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------------------------

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

-----------------------------------------------------------------------------------

{-

  (>>=) :: Monad m => m a -> (a -> m b) -> m b

  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

  join  :: Monad m => m (m a) -> m a

-----------------------------------------------------------------------------------

  >=> (or fish operator) really looks like a `flip (.)`, a function composition flipped
       (.) :: (b -> c) -> (a -> b) -> a -> c
  flip (.) :: (a -> b) -> (b -> c) -> a -> c

                        f             g
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

f >=> g =
  \a ->
    let mb =   f a  -- applying `f` to `a` resulting to a Functor value `mb`
    in  mb >>= g

-----------------------------------------------------------------------------------

  >>= (or bind) really looks like a `flip fmap`, a Functor flipped
       fmap :: Functor f => (a -> b) -> f a -> f b
  flip fmap :: Functor f => f a -> (a -> b) -> f b

(>>=) :: Monad m => m a -> (a -> m b) -> m b

mb >>= g = join $ fmap g mb

-}
