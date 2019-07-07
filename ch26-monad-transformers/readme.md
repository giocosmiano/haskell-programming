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

### Ordinary Type from a Transformer

  - `Identity` works fine recovering the non-transformer variant of each type as the `Identity` type is acting as a bit
    of do-nothing structural paste for filling in the gap

  - However, this is less common because the type is already there. If you’re writing something with, say, `scotty`,
    where a `ReaderT` is part of the environment, you can’t easily retrieve the `Reader` type out of that because
    `Reader` is not a type that exists on its own and you can’t modify that `ReaderT` without essentially rewriting
    all of `scotty`. You might then have a situation where what you’re doing only needs a `Reader`, not a `ReaderT`,
    so you could use `(ReaderT Identity)` to be compatible with `scotty` without having to rewrite everything but
    still being able to keep your own code a bit tighter and simpler.
  
```haskell
type MyIdentity a = IdentityT Identity a
type Maybe      a = MaybeT    Identity a
type Either   e a = EitherT e Identity a
type Reader   r a = ReaderT r Identity a
type State    s a = StateT  s Identity a
```

```haskell
Haskell λ > runMaybeT $ (+1) <$> MaybeT (Identity (Just 1))
Identity {runIdentity = Just 2}

Haskell λ > runMaybeT $ (+1) <$> MaybeT (Identity Nothing)
Identity {runIdentity = Nothing}
```    

### Lexically Inner is Structurally Outer

  - One of the trickier parts of `monad transformers` is that the lexical representation of the types will violate
    the intuitions with respect to the relationship it has with the structure of our values.

  - A necessary byproduct of how transformers work is that the additional structure `m` is always wrapped around
    our value. One thing to note is that it’s only wrapped around things we can have, not things we need, such
    as with `ReaderT`. The consequence of this is that a series of `monad transformers` in a type will begin with
    the `innermost type` structurally speaking.

```haskell
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

outerInner :: MaybeT (ExceptT String (StateT String (ReaderT () IO))) Int
outerInner = return 1

Haskell λ > (runReaderT $ ((runStateT . runExceptT . runMaybeT $ outerInner) "abc")) ()
(Right (Just 1),"abc")

Haskell λ > (runReaderT $ ((runStateT . runExceptT . runMaybeT $ return 1) "xyz")) ()
(Right (Just 1),"xyz")
```

```haskell
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

outerInner' :: MaybeT (ExceptT String (ReaderT String (StateT String IO))) Int
outerInner' = return 1

Haskell λ > (runStateT $ ((runReaderT . runExceptT . runMaybeT $ outerInner') "hello")) "world"
(Right (Just 1),"world")

Haskell λ > (runStateT $ ((runReaderT . runExceptT . runMaybeT $ return 1) "hello")) "world"
(Right (Just 1),"world")
```

### MonadTrans

  - We often want to lift functions into a larger context. We’ve been doing this for a while with `Functor`, which lifts a function
    into a context and applies it to the value inside. The facility to do this also undergirds `Applicative`, `Monad`, and `Traversable`.

```haskell
fmap  :: Functor f     => (a -> b) -> f a -> f b
liftA :: Applicative f => (a -> b) -> f a -> f b
liftM :: Monad m       => (a -> r) -> m a -> m r
```

  - `liftA` and `liftM` are lifting, just as `fmap` does, a function into some larger context. The underlying structure of the
    bind function from `Monad` is also a lifting function — `fmap` again! — composed with the crucial `join` function.
    
  - The idea, in some cases, is we want something that does as much lifting as is necessary to reach some `(structurally)` **outermost**
    position in a stack of `monad transformers`. `Monad transformers` can be nested in order to compose various effects into one monster
    function, but to manage those stacks, we need to `lift` more.

  - `MonadTrans` is about lifting actions in some `Monad` over a transformer type which wraps itself in the original `Monad`.

```haskell
Haskell λ > :t lift
lift :: (Monad m, MonadTrans t) => m a -> t m a
```

```haskell
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a
```

  - `Lift` a computation from the argument monad to the constructed monad. Here the 𝑡 is a (constructed) monad transformer type that has
   an instance of MonadTrans defined.

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just
```

### MonadIO

  - `MonadIO` is a different design than `MonadTrans` because rather than lifting through one layer at a time, `MonadIO` is intended
    to keep lifting your `IO action` until it is lifted over all structure embedded in the **outermost** `IO type`.
    
  - We don’t have to `lift` multiple times trying to reach a **base (outermost)** `Monad` that happens to be `IO`, because we have `liftIO`.

```haskell
Haskell λ > :t liftIO
liftIO :: MonadIO m => IO a -> m a
```

```haskell
class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a
```

```haskell
liftIO :: IO a -> ExceptT e IO a
liftIO :: IO a -> ReaderT r IO a
liftIO :: IO a -> StateT  s IO a
liftIO :: IO a -> StateT  s (ReaderT r IO) a
liftIO :: IO a -> ExceptT e (StateT s (ReaderT r IO)) a
```

### Referenced frameworks/libraries in the chapter
 - [scotty - Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp](https://hackage.haskell.org/package/scotty)
 - [pipes - Compositional pipelines](https://hackage.haskell.org/package/pipes)
 - [conduit - Streaming data processing library](https://hackage.haskell.org/package/conduit)

### For further reading
 - [Using Control.Arrow](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html)
   - [Practical Arrow Usage](http://tuttlem.github.io/2014/07/26/practical-arrow-usage.html)
   - [Understanding Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
   - [Arrow Tutorial - Haskell Wiki Books](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
   - [Arrow Tutorial - Haskell](https://wiki.haskell.org/Arrow_tutorial)
   - [Arrow Tutorial - School of Haskell](https://www.schoolofhaskell.com/user/peter/arrow-tutorial)
 - [Streaming logging - by Gabriel Gonzalez](http://www.haskellforall.com/2014/02/streaming-logging.html)
 - [Amb](https://wiki.haskell.org/Amb)
   - [McCarthy's Ambiguous Operator](http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2005/10/11/amb-operator/)
 - [A Simple Reader Monad Example](https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html)
 - [Reader Monad Transformer - by Carlo Hamalainen](https://carlo-hamalainen.net/2014/03/05/note-to-self-reader-monad-transformer/)
 - [The ReaderT Design Pattern - by FPComplete](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)
 - Further reading on Scotty
   - [Building a JSON REST API in Haskell](https://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/)
   - [Read you a Scotty](http://devanla.com/read-you-a-scotty.html)
   - [Using Monads](http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html)
   - [Build a JSON API](http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html)

