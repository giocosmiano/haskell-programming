### Definition
```haskell
class Functor f where

  fmap :: (a -> b) -> f a -> f b
```

***<$> is an alias of fmap***
```haskell
  fmap  :: Functor f => (a -> b) -> f a -> f b
  (<$>) :: Functor f => (a -> b) -> f a -> f b

   ($)  ::              (a -> b) ->   a ->   b -- function application
```

### Identity Law
```haskell
fmap id == id
```

### Composition Law

```haskell
fmap (f . g) == fmap f . fmap g
```

### Sample Functors

```haskell
type E e = Either e
type C e = Constant e
type I   = Identity

fmap :: (a -> b) -> f     a -> f     b
     :: (a -> b) -> [ ]   a -> [ ]   b
     :: (a -> b) -> Maybe a -> Maybe b
     :: (a -> b) -> E e   a -> E e   b  -- `e` is part of the structure thus will only fmap the `Right` in Either
     :: (a -> b) -> (e,   a)-> (e,   b) -- `e` is part of the structure thus will only fmap the `snd` in tuple
     :: (a -> b) -> I     a -> I     b  -- Data.Functor.Identity
     :: (a -> b) -> C e   a -> C e   b  -- Data.Functor.Const, will ignore the mapping
```



