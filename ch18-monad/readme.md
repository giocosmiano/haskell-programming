### Definition
 - Monad is an `Applicative Functor` with some unique features that make it a bit more powerful than
   either alone. A `Functor` maps a function over some structure; an `Applicative` maps a function
   that is contained in some structure over some other structure and then combines the two layers of
   structure like `mappend`.
   
 - Think of `Monads` as another way of applying functions over structure, with the ability of the function
   to alter the structure, something we’ve not seen in `Functor` and `Applicative`. `Monad` can inject more
   structure. However, it has the ability to flatten those two layers of structure into one is what
   makes `Monad` special. And it’s by putting that `join` function together with the mapping function
   that we get `bind`, also known as `>>=`.

 - The `Monad` type class is essentially a **generalized structure manipulation with some laws** to make
   it sensible. Just like `Functor` and `Applicative`.

```haskell
-- Sample Monad with added structure then flatten implicitly by `join`  
Prelude> [6,7,8] >>= \x -> [(x*5, x+3)]
[(30,9),(35,10),(40,11)]

-- as oppose to Functor that only applies the function to values in the structure
-- while leaving the structure intact
Prelude> fmap (\x -> [(x*5, x+3)]) [6,7,8]
[[(30,9)],[(35,10)],[(40,11)]]
```

### Monad class
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
