### Definition
```haskell
class (Functor t, Foldable t) => Traversable t where

  {-# MINIMAL traverse | sequenceA #-}

  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
```

## `traverse`
 - is mapping a function over some embedded value(s), like `fmap`, but similar to `flip bind`, that function is itself
   generating more structure. However, unlike `flip bind`, that structure can be of a different type than the
   structure we lifted over to apply the function. And at the end, it will flip the two structures around,
   as sequenceA did.
```haskell
Prelude> import Data.Functor.Identity
Prelude> traverse (Identity . (+1)) [1, 2, 3]
Identity [2,3,4]
Prelude> runIdentity $ traverse (Identity . (+1)) [1, 2, 3]
[2,3,4]
```

## `sequenceA`
 - is flipping two contexts or structures. It doesn’t by itself allow you to apply any function to the a value
   inside the structure; it only flips the layers of structure around.
```haskell
Prelude> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
```

## `traverse` Laws

***1. Naturality***
 - This law tells us that function composition behaves in unsurprising ways with respect to a traversed function.
   Since a traversed function f is generating the structure that appears on the “outside” of the traverse operation,
   there’s no reason we shouldn’t be able to float a function over the structure into the traversal itself.

```haskell
t . traverse f = traverse (t . f)
```

***2. Identity***
 - This law says that traversing the data constructor of the Identity type over a value will produce the same result
   as just putting the value in Identity. This tells us Identity represents a structural identity for traversing data.
   This is another way of saying that a Traversable instance cannot add or inject any structure or effects.
```haskell
traverse Identity = Identity
```
***3. Composition***
 - This law demonstrates how we can collapse sequential traversals into a single traversal, by taking advantage of
   the Compose datatype, which combines structure.
```haskell
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
```

## `sequenceA` Laws

***1. Naturality***
```haskell
t . sequenceA = sequenceA . fmap t
```

***2. Identity***
```haskell
sequenceA . fmap Identity = Identity
```

***3. Composition***
```haskell
sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
```

### Traversable Either

```haskell
data Either a b = Left a | Right b
                deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left  e <*> _ = Left e
  Right f <*> r = fmap f r

instance Foldable (Either a) where
  foldMap _ (Left _)  = mempty
  foldMap f (Right y) = f y

  foldr _ z (Left _)  = z
  foldr f z (Right y) = f y z

instance Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right y) = Right <$> f y
```

### Traversable tuple
```haskell
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z

instance Traversable ((,) a) where
  traverse f (x, y) = (,) x <$> f y
```


