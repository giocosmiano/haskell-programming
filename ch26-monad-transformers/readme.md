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
  
  - `lift` function `f` twice over because value `a`, in `ma`, is 2-layers deep, inside `m` then inside
    `Maybe` of structure `MaybeT m (Maybe a)`

#### MaybeT Applicative
```haskell
instance (Applicative m) => Applicative (MaybeT m) where

  pure :: Applicative m => a -> MaybeT m a
  pure ma = MaybeT $ (pure . pure) ma

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT maf) <*> (MaybeT ma) = MaybeT $ (fmap (<*>) maf) <*> ma
```
  - Same as `Functor`,
  
    - `lift` the applicative function `(<*>)` from `maf` over because value `a`, in `ma`, is 2-layers deep,
      inside `m` then inside `Maybe` of structure `MaybeT m (Maybe a)`

    - Then apply the result of that lifted applicative `<*>` function, with `<*>` to `ma`

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
  - Again, `ma` in argument `(MaybeT ma)` has a structure of `m (Maybe a)` therefore we need to extract `(Maybe a)` from
   `m (Maybe a)` via `v <- ma`

  - Perform case analysis on `Maybe a` before applying function `f`, which will result to `(MaybeT m b)`

  - Because the result of applying function `f` is `(MaybeT m b)`, see the signature of `(>>=)`

    - We can't return the result back to `MaybeT $` as it will result to `MaybeT (MaybeT m b)`

    - Therefore, we need to perform `runMaybeT` to extract `(m b)` out of `(MaybeT m b)` and return it as argument to `MaybeT $`


