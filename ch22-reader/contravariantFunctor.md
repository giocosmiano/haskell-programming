###Reader Functor
```haskell
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
```

###Function Composition/Reader Functor
```haskell
Haskell λ > :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)
-- OR compose f g = f . g

-- (<$>) :: (a -> b) ->     f a  ->     f b
-- (<$>) :: (a -> b) -> (r -> a) -> (r -> b)
-- (.)   :: (b -> c) -> (a -> b) -> (a -> c)
```

***If we have a function `a -> b` and we need a function of `a -> c` then we need to change our result types,
so we need an `adaptor` function `b -> c` to get there***

###Contravariant Composition/Contravariant Functor
```haskell
Haskell λ > :t flip (.)
flip (.) :: (a -> b) -> (b -> c) -> a -> c

contraCompose :: (a -> b) -> (b -> c) -> (a -> c)
contraCompose f g = \x -> g (f x)
-- OR contraCompose f g = g . f

-- contramap     :: (a -> b) ->     f a  ->     f b
-- contramap     :: (a -> b) -> (a -> r) -> (b -> r)
-- contraCompose :: (a -> b) -> (b -> c) -> (a -> c)
```

***If we have a function `a -> c` and we need a function `b -> c`, we have to change the input type.
We need a function from `a -> b` to get there.***

###Covariant vs Contravariant
***With covariant composition, we wanted to change the result type and did so by passing it to an adaptor function.
With Contravariant, we want to change the input type by pre-processing the input before passing it off to our original function.***

- [Contravariant Functor](https://typeclasses.com/contravariance)
- [ProFunctor](https://typeclasses.com/profunctors)



