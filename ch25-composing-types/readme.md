### Composing Types
 - `Functors` and `Applicatives` are both closed under composition, which means we can compose two `functors`
   (or two `applicatives`) and return another `functor` (or `applicative`, as the case may be). This is not true
   of `monads`, however; when we compose two `monads`, the result is **`NOT`** necessarily another `monad`.

 - However, there are times when composing `monads` is desirable. Different `monads` allow us to work with different effects.
   Composing `monads` allows us to build up computations with multiple effects. By stacking, for example, a `Maybe monad` with
   an `IO`, we can be performing `IO actions` while also building up computations that have a possibility of failure, handled
   by the `Maybe monad`.

 - A `monad transformer` is a variant of an ordinary type that takes an additional type argument which is assumed
   to have a `Monad` instance. For example, `MaybeT` is the transformer variant of the `Maybe` type. The transformer
   variant of a type gives us a `Monad` instance that binds over both bits of structure. This allows us to compose
   `monads` and combine their effects.

***Quick note on newtype***
 - While `monad transformer` types could be written using the data keyword, they are most commonly written as `newtypes`
   to avoid unnecessary overhead, as the underlying representation is identical to the type they contain.

 - The important thing is that `monad transformers` are never sum or product types; they are always a means of
   wrapping one extra layer of (`monadic`) structure around a type, so there is never a reason they couldn’t be newtypes.

### Compose type - a structure with 2-layers of structures in it
 - `Compose` type should look to you much like function composition, but in this case, the `𝑓` and `𝑔` represent
   type constructors, **`NOT`** term-level functions.
   - e.g. `Compose [Just (1 :: Int), Nothing]`
     - f ~ []
     - g ~ Maybe
     - a ~ Int

 ```haskell
newtype Compose f g a = Compose { getCompose :: f (g a) }
                      deriving (Eq, Show)
```

 ```haskell
Prelude> :i (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c

Prelude> :k Compose
Compose :: (* -> *) -> (* -> *) -> * -> *

Prelude> :t Compose [Just (1 :: Int), Nothing]
Compose [Just (1 :: Int), Nothing] :: Compose [] Maybe Int
```

### For further reading
 - [All About Monads](https://wiki.haskell.org/All_About_Monads)
 - [Real World Haskell - by Bryan O'Sullivan](http://book.realworldhaskell.org/read/)
 - [Real World Haskell - Monad Transformers](http://book.realworldhaskell.org/read/monad-transformers.html)
 - [What I Wish I Knew When Learning Haskell - by Stephen Diehl](http://dev.stephendiehl.com/hask/#monads)
