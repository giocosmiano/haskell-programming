### Definition
```haskell
class Applicative m => Monad m where

  return :: a -> m a

  >>=    :: m a -> (a -> m b) -> m b

  >>     :: m a -> m b -> m b
```

***Monad vs Applicative vs Functor vs Function Application***
```haskell
  (>>=) :: Monad f       => f a        -> (a -> f b) -> f b
  (<*>) :: Applicative f => f (a -> b) -> f a        -> f b
  (<$>) :: Functor f     =>   (a -> b) -> f a        -> f b

  (>>=) :: f  a       ->  (a -> f b) -> f b -- monad
  (<*>) :: f (a -> b) -> f a         -> f b -- applicative
  (<$>) ::   (a -> b) -> f a         -> f b -- functor
   ($)  ::   (a -> b) ->   a         ->   b -- function application
```

***liftA vs liftM***
```haskell
liftA :: Applicative f  => (a  -> b) -> f a  -> f b
liftM :: Monad m        => (a1 -> r) -> m a1 -> m r

liftA2 :: Applicative f => (a  -> b  -> c) -> f a  -> f b  -> f c
liftM2 :: Monad m       => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

liftA3 :: Applicative f => (a  -> b  -> c  -> d) -> f a  -> f b  -> f c  -> f d
liftM3 :: Monad m       => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
```

***fmap using monadic operation***
```haskell
fmap f xs = xs >>= return . f

-- e.g.
-- fmap (+1) [1..3]          -> [2,3,4]
-- [1,2,3] >>= return . (+1) -> [2,3,4]
```

## Laws

### Identity Law

***left identity***
```haskell
return x >>= f = f x
```

***right identity***
```haskell
m >>= return = m
```

### Associativity Law

```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

## Kleisli Composition
```haskell
  (>>=) :: Monad m => m a -> (a -> m b) -> m b              -- bind

  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c  -- fish operator

  join  :: Monad m => m (m a) -> m a

```

### fish operator (>=>) ###
```haskell
  >=> (or fish operator) really looks like a `flip (.)`, a function composition flipped
       (.) :: (b -> c) -> (a -> b) -> a -> c
  flip (.) :: (a -> b) -> (b -> c) -> a -> c

                          f             g
  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

  f >=> g =
    \a ->
      let mb =   f a  -- applying `f` to `a` resulting to a Functor value `mb`
      in  mb >>= g
```

### bind (>>=) ###
```haskell
  >>= (or bind) really looks like a `flip fmap`, a Functor flipped
       fmap :: Functor f => (a -> b) -> f a -> f b
  flip fmap :: Functor f => f a -> (a -> b) -> f b

  (>>=) :: Monad m => m a -> (a -> m b) -> m b

  mb >>= g = join $ fmap g mb
```
