### Definition

***Reader kind***
```haskell
Prelude> :k (->)
(->) :: TYPE q -> TYPE r -> *
```

***Functor Reader***
```haskell
data (->) a b

instance Functor ((->) r) where

  fmap = (.)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
  
OR
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

-- visualizing the Functor pattern for Reader
-- (<$>) :: (a -> b) ->     f a  ->     f b
-- (<$>) :: (a -> b) -> (r -> a) -> (r -> b)
```

***Function Composition***
```haskell
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- e.g.
-- \r -> f (ra r) -- Functor Reader
-- \x -> f (g  x) -- function composition
```

***Applicative Reader***
```haskell
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a
  
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \x -> rab x (ra x)

-- visualizing the Applicative pattern for Reader
-- pure :: a ->     f a
-- pure :: a -> (r -> a)

-- (<*>) ::    f (a -> b) ->     f a  ->     f b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
```

***Monad Reader***
```haskell
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \x -> aRb (ra x) x

-- visualizing the Monad pattern for Reader
-- return = pure
-- OR
-- return :: a ->     m a
-- return :: a -> (r -> a)

-- (>>=) ::     m a  -> (a -> (   m b)) ->     m b
-- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
```


