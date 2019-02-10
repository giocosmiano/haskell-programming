### When things go wrong
 - Love this quote - [It is easier to write an incorrect program than understand a correct one - Alan Perlis](http://www.cs.yale.edu/homes/perlis-alan/quotes.html)
 
### Exception class and methods
 - Ordinarily, the `forall` quantifies variables universally, as we might guess from the word all. However, the `SomeException` type
   constructor doesn’t take an argument; the type variable `e` is a parameter of the data constructor. It takes an `e` and results in
   a `SomeException`. Moving the quantifier to the data constructor limits the scope of its application, and changes the meaning from
   `for all e` to `there exists some e`. That is existential quantification. It means that any type that implements the `Exception`
   class can be that `e` and be subsumed under the `SomeException` type.

 - Usually when `type constructors` are parameterized, they are universally quantified. Arguments have to be supplied to satisfy them.
   Our `Maybe a` type is a sort of function waiting for an argument to be supplied to be a fully realized type. But when we
   existentially quantify a type, as with `SomeException`, we can’t do much with that polymorphic type variable in its `data constructor`.
   We can’t concretize it. Other than adding constraints, we can’t know anything about it. It must remain polymorphic, and we can cram
   any value of any type that implements its constraint into that role. It’s like a polymorphic parasite just hanging out on your type.
   
```haskell
data SomeException =
  forall e . Exception e => SomeException e

data SomeException where
  SomeException :: Exception e => e -> SomeException

class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
```

```haskell
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator

instance Exception ArithException
```
 
### Typeable
 - The `Typeable` type class lives in the `Data.Typeable` module. `Typeable` exists to permit types to be known at runtime, allowing for a
   sort of dynamic typechecking. It allows us to learn the type of a value at runtime and also to compare the types of two values and
   check that they are the same. `Typeable` is particularly useful when we have code that needs to allow various types to be passed to it
   but needs to enforce or trigger on specific types.

```haskell
Haskell λ > :t cast
cast :: (Typeable b, Typeable a) => a -> Maybe b

Haskell λ > :t catch
catch :: Exception e => IO a -> (e -> IO a) -> IO a

Haskell λ > :t try
try :: Exception e => IO a -> IO (Either e a)
```

### throw vs throwIO
 - Partiality in the form of throwing an exception can be thought of as an effect. The conventional way to throw an exception is to use
   `throwIO`, which has `IO` in its result. This is the same thing as `throw`, but `throwIO` embeds the exception in `IO`. We always handle
   exceptions in `IO`. Handling exceptions must be done in `IO` even if they were thrown without an `IO` type. We almost never want `throw`
   as it throws exceptions without any warning in the type, even `IO`.

```haskell
Haskell λ > :t throw
throw :: Exception e => e -> a

Haskell λ > :t throwIO
throwIO :: Exception e => e -> IO a
```

### Referenced frameworks/libraries
 - [twitter-conduit - Twitter API package with conduit interface and Streaming API support](http://hackage.haskell.org/package/twitter-conduit)

### For further reading
 - [A Beginner's Guide to Exceptions in Haskell - By Erin Swenson-Healey](https://www.youtube.com/watch?v=PWS0Whf6-wc)   
 - [Ch08 - Overlapping Input/Output - Parallel and Concurrent Programming In Haskell - by Simon Marlow](https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch08.html)
 - [Ch09 - Cancellation and Timeouts - Parallel and Concurrent Programming In Haskell - by Simon Marlow](https://www.safaribooksonline.com/library/view/parallel-and-concurrent/9781449335939/ch09.html)
 - [Extensible Dynamically-Typed Hierarchy of Exceptions - By Simon Marlow](https://simonmar.github.io/bib/papers/ext-exceptions.pdf)   
