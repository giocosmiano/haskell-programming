### Definition
```haskell
class Monoid m where

  mempty :: m

  mappend :: m -> m -> m

  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

### Identity Law

***left identity***
```haskell
mappend mempty x = x
```

***right identity***
```haskell
mappend x mempty = x
```

### Associativity Law

```haskell
mappend x (mappend y z) = mappend (mappend x y) z
```


