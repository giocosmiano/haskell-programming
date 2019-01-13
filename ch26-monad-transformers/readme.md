### Monad Transformers
 - `Monad transformer` is a type constructor that takes a `Monad` as an argument and returns a `Monad` as a result.
   The fundamental problem with composing two `monads` lies in the impossibility of joining two unknown `monads`.
   In order to make that `join` happen, we need to reduce the polymorphism and get concrete information about one of the
   `monads` that we’re working with. The other `monad` remains polymorphic as a variable type argument to our type
   constructor. Transformers help us make a `monad` out of multiple (2, 3, 4...) types that each have a `Monad` instance
   by wrapping around existing `monads` that provide each bit of wanted functionality.

 - Transformers are bearers of single-type concrete information that let you create ever-bigger `monads` in a sense.
   Nesting such as `(Monad m) => m (m a)` is addressed by `join` already. We use transformers when we want
   a `>>=` operation over `𝑓` and `𝑔` of different types (but both have `Monad` instances). We have to create
   new types called `monad transformers` and write `Monad` instances for those types to have a way of dealing with the
   extra structure generated.

### MaybeT Monad Transformer
```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

#### MaybeT Functor
```haskell
instance (Functor m) => Functor (MaybeT m) where

  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b

  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
```
  - The `ma` in argument `(MaybeT ma)` has a structure of `m (Maybe a)`
  
  - Need to `lift` function `f` twice over because value `a` is 2-layers deep, inside `m` then inside `Maybe`
    in structure `m (Maybe a)` of type `MaybeT m a`

#### MaybeT Applicative
```haskell
instance (Applicative m) => Applicative (MaybeT m) where

  pure ma = MaybeT $ (pure . pure) ma

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b

  (MaybeT maf) <*> (MaybeT ma) = MaybeT $ (fmap (<*>) maf) <*> ma
```
  - Same as `Functor`

    - `lift` the applicative function `(<*>)` from `maf` over to get the function `(a -> b)` inside `MaybeT m (a -> b)`

    - then apply `<*>` the function `(a -> b)` to value `a` which is inside the `ma` that has a structure of `m (Maybe a)`

#### MaybeT Monad
```haskell
instance (Monad m) => Monad (MaybeT m) where

  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just x  -> runMaybeT (f x)
```
