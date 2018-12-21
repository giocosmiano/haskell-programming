module TraversableConcepts where

{-

-----------------------------------------------------------------------------------
SO, WHAT'S TRAVERSABLE FOR???
-----------------------------------------------------------------------------------
In a literal sense, anytime you need to flip two type constructors
around, or map something and then flip them around, that’s probably Traversable

Prelude> f = undefined :: a -> Maybe b
Prelude> xs = undefined :: [a]
Prelude> :t map f xs
map f xs :: [Maybe b]

But what if we want a value of type Maybe [b], instead of [Maybe b]?

Prelude> :t sequenceA $ map f xs
sequenceA $ map f xs :: Maybe [a]

It’s usually better to use traverse whenever we see a sequence
or sequenceA combined with a map or fmap

Prelude> :t traverse f xs
traverse f xs :: Maybe [b]

-----------------------------------------------------------------------------------
another sample
-----------------------------------------------------------------------------------

`traverse` maps each element of a structure to an action,
evaluates the actions from left to right, and collects the results.
If you find yourself with a value that has a type like [IO a], it’s
possible you made a mistake and used `fmap` where you wanted
`traverse`

myData :: [String]
myFunc :: String -> IO Record

wrong :: [IO Record]
wrong = fmap myFunc myData

right :: IO [Record]
right = traverse myFunc myData

-----------------------------------------------------------------------------------
Traversable type class
-----------------------------------------------------------------------------------

{-# MINIMAL traverse | sequenceA #-}

class (Functor t, Foldable t) => Traversable t where

  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

-----------------------------------------------------------------------------------
function references
-----------------------------------------------------------------------------------
  fmap ::     (a ->   b) -> f a        -> f b
  traverse :: (a -> f b) -> t a        -> f (t b)
  (=<<) ::    (a -> m b) -> m a        -> m b  -- flip bind
  (>>=) ::          f a  -> (a -> f b) -> f b  -- bind

  (<*>) :: f (a -> b) -> f a -> f b
  (<$>) ::   (a -> b) -> f a -> f b
   ($)  ::   (a -> b) ->   a ->   b

-- mapM is traverse
  mapM :: Monad m           => (a -> m b) -> [a] -> m [b]
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

  sequence :: Monad m                         =>   [m a] -> m  [a]
  sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)

-----------------------------------------------------------------------------------
-- Either
-----------------------------------------------------------------------------------
data Either a b = Left a | Right b
                deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left x) = Left x
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

-----------------------------------------------------------------------------------
-- Tuple
-----------------------------------------------------------------------------------
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

-}
