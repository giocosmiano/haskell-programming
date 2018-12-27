### Definition
```haskell
class Foldable (t :: * -> *) where

  {-# MINIMAL foldMap | foldr #-}

  fold :: Monoid m => t m -> m

  foldMap :: Monoid m => (a -> m) -> t a -> m
```

### Foldable Identity

```haskell
newtype Identity a = Identity a

instance Foldable Identity where

  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x
```

### Foldable Maybe
 - we need to assert a type that has a Monoid for this to work

```haskell
data Optional a = Nada | Yep a

instance Foldable Optional where

  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- e.g.
-- foldMap (+1) Nada :: Sum Int     -> Sum {getSum = 0}
-- foldMap (+1) Nada :: Product Int -> Product {getProduct = 1}
-- foldMap (+1) (Just 1) :: Sum Int -> Sum {getSum = 2}
```


