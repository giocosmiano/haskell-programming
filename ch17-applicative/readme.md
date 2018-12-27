### Definition
```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

***Applicative vs Functor vs Function Application***
```haskell
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  fmap  :: Functor f     =>   (a -> b) -> f a -> f b

  (<*>) :: f (a -> b) -> f a -> f b -- applicative
  (<$>) ::   (a -> b) -> f a -> f b -- functor
   ($)  ::   (a -> b) ->   a ->   b -- function application
```

***Applicative are monoidal functors***
```haskell
  mappend :: Monoid a => a -> a -> a

  mappend :: f          -> f   -> f 
  (<*>)   :: f (a -> b) -> f a -> f b
  (<$>)   ::   (a -> b) -> f a -> f b
```

***liftA is just an fmap but only with Applicative while liftA2/A3 involves with more arguments***
```haskell
  liftA  :: Applicative f => (a -> b)           -> f a -> f b
  liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
  liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

## Laws

### 1. Identity Law
```haskell
pure id <*> v = v
```

### 2. Composition Law
 - It is the law stating that the result of composing our functions first and then applying them and the result
   of applying the functions first then composing them should be the same. We’re using the composition operator
   as a prefix instead of the more usual infix, and using pure in order to embed that operator into the appropriate
   structure so that it can work with apply.

```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

### 3. Homomorphism Law
 - Homomorphism is a structure-preserving map between two algebraic structures. The effect of applying a function
   that is embedded in some structure to a value that is embedded in some structure should be the same as applying
   a function to a value without affecting any outside structure
   
```haskell
pure f <*> pure x = pure (f x)
```

### 4. Interchange Law
 - To the left of <*> must always be a function embedded in some structure. In the definition, `u` represents
   a function embedded in some structure
 - To the right side of the definition, by sectioning the $ function application operator with the `y`,
   we create an environment in which the `y` is there, awaiting a function to apply to it
      
```haskell
u <*> pure y = pure ($ y) <*> u
```

