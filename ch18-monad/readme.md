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

Prelude> (Just 7) >>= \x -> Just (x*5, x+3)
Just (35,10)

-- as oppose to Functor that only applies the function to values in the structure
-- while leaving the structure intact
Prelude> fmap (\x -> [(x*5, x+3)]) [6,7,8]
[[(30,9)],[(35,10)],[(40,11)]]

Prelude> fmap (\x -> Just (x*5, x+3)) (Just 7)
Just (Just (35,10))

-- on the other hand, this will throw an error because the types are different, [] vs Maybe
Prelude> [6,7,8] >>= \x -> Just (x*5, x+3)
<interactive>:15:19: error:
    Couldn't match type 'Maybe' with '[]'
    Expected type: [(b, b)]
      Actual type: Maybe (b, b)

Prelude> (Just 7) >>= \x -> [(x*5, x+3)]
<interactive>:16:20: error:
    Couldn't match type '[]' with 'Maybe'
    Expected type: Maybe (b, b)
      Actual type: [(b, b)]

-- to fix the error, use the `return` to put the results back to its original structure
Prelude> [6,7,8] >>= return . \x -> Just (x*5, x+3)
[Just (30,9),Just (35,10),Just (40,11)]

Prelude> (Just 7) >>= return . \x -> [(x*5, x+3)]
Just [(35,10)]
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

### For further reading
 - [All About Monads](https://wiki.haskell.org/All_About_Monads)
 - [What a Monad is not](https://wiki.haskell.org/What_a_Monad_is_not)
 - [Real World Haskell - by Bryan O'Sullivan](http://book.realworldhaskell.org/read/)
 - [Real World Haskell - Monads](http://book.realworldhaskell.org/read/monads.html)
 - [Real World Haskell - Programming with Monads](http://book.realworldhaskell.org/read/programming-with-monads.html)
 - [How to desugar Haskell code - by Gabriel Gonzalez](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html)
 - [Monads Made Difficult - by Stephen Diehl](http://www.stephendiehl.com/posts/monads.html)
 - [What I Wish I Knew When Learning Haskell - by Stephen Diehl](http://dev.stephendiehl.com/hask/#monads)
