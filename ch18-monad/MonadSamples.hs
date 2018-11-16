{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadSamples where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


{-

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

fmap :: Functor f     =>   (a -> b) -> f a        -> f b
<*>  :: Applicative f => f (a -> b) -> f a        -> f b
>>=  :: Monad f       => f a        -> (a -> f b) -> f b

-- Functor f => Applicative f => Monad f
-- fmap using monadic operation
fmap f xs = xs >>= return . f

-- e.g.
-- fmap (+1) [1..3]          -> [2,3,4]
-- [1,2,3] >>= return . (+1) -> [2,3,4]

-----------------------------------------------------------------------------------
>>= vs `bind`
-----------------------------------------------------------------------------------

import Control.Monad (join)
join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

-- using bind
-- bind (\x -> [x,1]) [4,5,6]        -> [4,1,5,1,6,1]
--
-- using >>=
-- [4,5,6] >>= (\x -> [x,1])         -> [4,1,5,1,6,1]
--
-- using join/fmap
-- join $ fmap (\x -> [x,1]) [4,5,6] -> [4,1,5,1,6,1]

-----------------------------------------------------------------------------------
liftA vs liftM
-----------------------------------------------------------------------------------
liftA :: Applicative f  => (a  -> b) -> f a  -> f b
liftM :: Monad m        => (a1 -> r) -> m a1 -> m r

liftA2 :: Applicative f => (a  -> b  -> c) -> f a  -> f b  -> f c
liftM2 :: Monad m       => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

liftA3 :: Applicative f => (a  -> b  -> c  -> d) -> f a  -> f b  -> f c  -> f d
liftM3 :: Monad m       => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r

-- e.g.
-- liftA2 (,) (Just 3) (Just 5) -> Just (3,5)
-- liftM2 (,) (Just 3) (Just 5) -> Just (3,5)

-- e.g.
-- zipWith is liftA2 or liftM2 specialized to lists
--
-- zipWith ::  (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith (+) [3, 4] [5, 6] -> [8,9,9,10]
-- liftA2  (+) [3, 4] [5, 6] -> [8,9,9,10]
-- liftM2  (+) [3, 4] [5, 6] -> [8,9,9,10]

--
-- zipWith3/liftM3 differ differing behavior has
-- to do with which list monoid is being used
--
-- zipWith3 ::  (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- zipWith3 (,,) [1, 2] [3] [5, 6] -> [(1,3,5)]
-- liftM3   (,,) [1, 2] [3] [5, 6] -> [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]

-----------------------------------------------------------------------------------
sequencing functions
-----------------------------------------------------------------------------------

(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m       => m a -> m b -> m b

import Control.Applicative ((*>))
import Control.Monad       ((>>), join)

-- e.g.
-- putStrLn "Hello, " *> putStrLn "World!"
-- putStrLn "Hello, " >> putStrLn "World!"

-- e.g.
-- join $ putStrLn <$> getLine
-}

-----------------------------------------------------------------------------------

