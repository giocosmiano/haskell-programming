### This is my personal repository learning `Haskell` from [Haskell Programming](http://haskellbook.com/)

 - First, this is my personal journey learning `Haskell` therefore any mistakes on concepts and/or chapter exercises are my own.

 - Second, I came to this `Haskell` journey because of my curiosity in FP that began mid-2016 when I was working on a UI story/task in
   [Angular](https://angular.io/) and used these JS libraries, [RamdaJS](https://ramdajs.com/), [RxJS](http://reactivex.io/) and
   [ReduxJS](https://redux.js.org/), that are primarily developed with FP concepts in mind, such as immutability, composition, high-order
   function etc.

   Since then my `Haskell` journey has been on-and-off, until the fall season of 2018 that I picked up this book to learn. I can say that
   the authors did an excellent job writing this, shout-out to them. I'm now able to demystify `Haskell`, bit by bit, while having fun
   working on chapter exercises as the authors made me think, connect the dots and perform [diagram chasing](https://en.wikipedia.org/wiki/Commutative_diagram).

### Notes about my journey in working through chapter exercises

 - Functional programming is a function
   - of data transformation, in a [pure manner](https://wiki.haskell.org/Pure)
   - [composition](https://wiki.haskell.org/Function_composition)
   - that is [lazy](https://wiki.haskell.org/Lazy_evaluation)

 - Function, by default, is curried meaning it only accepts 1-argument and returns either

   - the reduced value

   - OR another function that will be further applied, and so on, which will eventually reduced into a value   

 - Functions, [when needed](https://en.wikibooks.org/wiki/Haskell/Laziness), are evaluated and eventually reduced into a
   value called [Beta reduction](https://wiki.haskell.org/Beta_reduction) 

 - Functions are data because [when needed](https://en.wikibooks.org/wiki/Haskell/Laziness) they will eventually get evaluated
   and reduced into a value.  

 - Tools for Building Haskell Projects/Packages
   - [GHC - Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
     - [Use version 8.0.2](https://www.haskell.org/ghc/download_ghc_8_0_2.html)
   - [Hackage - Haskell community's central package archive of open source software](http://hackage.haskell.org/)
   - [Stackage - Stable source of Haskell packages](https://www.stackage.org/)
   - [Stack - Cross-platform program for developing Haskell projects](https://www.haskellstack.org/)
   - [Cabal - System for building and packaging Haskell libraries and programs](https://www.haskell.org/cabal/)

 - Use these resources
   - [Haskell Language Documentation](https://haskell-lang.org/documentation)   
   - [Haskell Documentation](https://www.haskell.org/documentation)
   - [Hoogle](https://www.haskell.org/hoogle/)
   - [GHC User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)

### List of reminders to myself

 - Always come back to this pattern `f x = y` when you're lost

   - on where to apply the function to a value

   - or how many times to `lift` the function over multi-layered stack of `monads`.

     - i.e. `ComposeType`, `outerInner` and [Scotty's Web - ActionT](http://hackage.haskell.org/package/scotty-0.11.3/docs/Web-Scotty-Internal-Types.html#t:ActionT)

```haskell
newtype ComposeType f g h a = ComposeType { getComposeType :: f (g (h a)) } deriving (Eq, Show)

outerInner :: MaybeT (ExceptT String (ReaderT String (StateT String IO))) Int

newtype ActionT e m a =
  ActionT
  { runAM
    :: ExceptT
         (ActionError e)
         (ReaderT ActionEnv
           (StateT ScottyResponse m))
         a
  }
  deriving ( Functor, Applicative, MonadIO )
```

 - When transforming a structure, **NOT the value inside it**, such as List-to-Maybe, use
   [Natural Transformation](https://wiki.haskell.org/Category_theory/Natural_transformation) i.e.

```haskell
{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a
```
 
 - Lifting the `function` to the base/outermost `IO` monad via [liftIO](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers#monadio)
   **or** [lift, lift, lift](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers#monadtrans)

```haskell
Haskell λ > :t liftIO
liftIO :: MonadIO m => IO a -> m a

Haskell λ > :t lift
lift :: (Monad m, MonadTrans t) => m a -> t m a
```
 - Use **_`:t`_**, **_`:k`_**, **_`:i`_** for details

 - Use language pragma [`{-# LANGUAGE InstanceSigs #-}`](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#ghc-flag--XInstanceSigs)
   to have a clear vision of type signatures. i.e.

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: Applicative m => a -> MaybeT m a
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b

instance (Monad m) => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b

instance (Foldable m) => Foldable (MaybeT m) where
  foldMap :: (Monoid mn, Foldable m) => (a -> mn) -> MaybeT m a -> mn

instance (Traversable m) => Traversable (MaybeT m) where
  traverse :: Applicative fa => (a -> fa b) -> MaybeT m a -> fa (MaybeT m b)
```

### Conclusion to my `Haskell` journey

 - Finally, after months of reading, more-and-more readings, lots of trial-and-error in `GHCi`, and countless nights/weekends working through
   chapter exercises, I can now celebrate completing this massive technical book. I don't consider myself reaching the
   [ivory tower](https://www.youtube.com/watch?v=TkBOozeNtiM) nor [achieved some programming enlightenment](http://www.haskellforall.com/2017/10/advice-for-haskell-beginners.html)
   but rather this experience gave me substantial knowledge to start the next chapter of my journey towards working in real-world `Haskell` applications. 

#### Chapter 1 - All You Need is Lambda
 - My background is primarily OOP, and have a very good understanding of FP working in [Scala](https://www.scala-lang.org/) and
   [ES6 high order functions](https://eloquentjavascript.net/05_higher_order.html) but this chapter provides
   a good foundational details on [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

```haskell
Haskell λ > let addAndMultiply = \x -> \y -> \z -> x + y * z

Haskell λ > addAndMultiply 3 5 7
38
```

#### [Chapter 2 - Hello, Haskell!](https://github.com/giocosmiano/haskell-programming/tree/master/ch02-hello-haskell)
 - Introduction to the following `Haskell` subjects
   - Function and evaluation
   - Infix operators
   - Associativity and precedence
   - Expression components such as `let` and `where`

#### Chapter 3 - Strings
 - Printing strings
 - Concatenation functions

#### [Chapter 4 - Basic data types](https://github.com/giocosmiano/haskell-programming/tree/master/ch04-basic-datatypes)
 - Built-in data types
 - Integral vs Int, Integer and Word
 - Bool and comparing values
 - Tuples and Lists

#### [Chapter 5 - Types](https://github.com/giocosmiano/haskell-programming/tree/master/ch05-types)
 - [Currying](https://wiki.haskell.org/Currying)
 - [Polymorphism](https://wiki.haskell.org/Polymorphism)
 - [Type inference](https://wiki.haskell.org/Type_inference)

#### [Chapter 6 - Type classes](https://github.com/giocosmiano/haskell-programming/tree/master/ch06-typeclasses)
 - In OOP, such as Java, this is similar to defining `interface`. `Haskell` data types doesn't have any function
   associated with it unlike Java wherein functions are automatically inherited from `java.lang.Object`.
   - i.e. [java.lang.Object](https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html) class has toString, equals, hashCode etc. 
 - Type classes Eq, Num, Ord, Enum, Show, Read
 - Type class inheritance

#### [Chapter 7 - More functional patterns](https://github.com/giocosmiano/haskell-programming/tree/master/ch07-more-functional-patterns)
 - [Anonymous functions](https://wiki.haskell.org/Anonymous_function)
 - [Pattern matching](https://en.wikibooks.org/wiki/Haskell/Pattern_matching)
 - [Case expressions](https://en.wikibooks.org/wiki/Haskell/Control_structures)
 - [Guards](https://www.haskell.org/tutorial/patterns.html)
 - [Function composition](https://wiki.haskell.org/Function_composition)
 - [Point-free style](https://wiki.haskell.org/Pointfree)

#### [Chapter 8 - Recursion](https://github.com/giocosmiano/haskell-programming/tree/master/ch08-recursion)
 - [Bottom](https://en.wikipedia.org/wiki/Bottom_type)
 - Factorial! and Fibonacci numbers

#### [Chapter 9 - Lists](https://github.com/giocosmiano/haskell-programming/tree/master/ch09-lists)
 - Pattern matching on lists
 - [List comprehensions](https://wiki.haskell.org/List_comprehension)
 - Spines and non-strict evaluation
 - Zipping lists

#### [Chapter 10 - Folding lists](https://github.com/giocosmiano/haskell-programming/tree/master/ch10-folding-lists)
 - Recursive patterns
 - [foldr vs foldl](https://wiki.haskell.org/Fold)
   - This chapter helped a lot as it preps me for [Chapter 20 - Foldable](https://github.com/giocosmiano/haskell-programming#chapter-20---foldable)
     and [Chapter 21 - Traversable](https://github.com/giocosmiano/haskell-programming#chapter-21---traversable)

#### [Chapter 11 - Algebraic data types](https://github.com/giocosmiano/haskell-programming/tree/master/ch11-algebraic-datatypes)
 - [Algebraic datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type)
 - Data and type constructors
 - Type constructors and kinds
 - Data constructors and values
 - Data constructor arities
 - newtype, Sum types, Product types
 - Function type is exponential
 - Higher-kinded data types

#### [Chapter 12 - Signaling adversity](https://github.com/giocosmiano/haskell-programming/tree/master/ch12-signaling-adversity)
 - Nothing, or Just Maybe
 - Either left or right, but not both
 - higher-kindedness

#### [Chapter 13 - Building projects](https://github.com/giocosmiano/haskell-programming/tree/master/ch13-building-projects)
 - Modules
 - Making packages with [Stack](https://docs.haskellstack.org)
 - Building a project

#### [Chapter 14 - Testing](https://github.com/giocosmiano/haskell-programming/tree/master/ch14-testing)
 - [QuickCheck](http://hackage.haskell.org/package/QuickCheck)
 - Arbitrary instances

#### [Chapter 15 - Monoid, Semigroup](https://github.com/giocosmiano/haskell-programming/tree/master/ch15-monoid-semigroup)
 - Algebra and Laws

 - use of `newtype`
   - To signal intent: using newtype makes it clear that you only intend for it to be a
     wrapper for the underlying type. The newtype cannot eventually grow into a more
     complicated sum or product type, while a normal datatype can

   - To improve type safety: avoid mixing up many values of the same representation,
     such as Text or Integer

   - To add different type class instances to a type that is otherwise unchanged
     representationally, such as with Sum and Product

```haskell
Haskell λ > import Data.Monoid

Haskell λ > Sum 3 <> Sum 5 <> Sum 7
Sum {getSum = 15}

Haskell λ > Product 3 <> Product 5 <> Product 7
Product {getProduct = 105}
```

#### [Chapter 16 - Functor](https://github.com/giocosmiano/haskell-programming/tree/master/ch16-functor)
 - Laws

 - IO Functor

 - Functors are unique to a datatype

 - Functor is a way to apply a function over or around some structure that
   we don’t want to alter. That is, we want to apply the function to the value
   that is “inside” some structure and leave the structure alone.

```haskell
Haskell λ > newtype MyName = MyName { getMyName :: String } deriving (Eq, Show)

Haskell λ > MyName <$> Just "gio"
Just (MyName {getMyName = "gio"})
```

```haskell
Haskell λ > fmap (const 3)  [Just "hello world"]
[3]

Haskell λ > (fmap . fmap) (const 3)  [Just "hello world"]
[Just 3]

Haskell λ > (fmap . fmap . fmap) (const 3)  [Just "hello world"]
[Just [3,3,3,3,3,3,3,3,3,3,3]]
```

#### [Chapter 17 - Applicative](https://github.com/giocosmiano/haskell-programming/tree/master/ch17-applicative)
 - Laws

 - Functor vs Applicative

 - ZipList Monoid

 - Applicatives are monoidal functors. The Applicative type class allows for function
   application lifted over structure (like Functor). But with Applicative the function
   we’re applying is also embedded in some structure. Because the function and the value
   it’s being applied to both have structure, we have to smash those structures together.
   So, Applicative involves monoids, like `mappend`, and functors.

```haskell
Haskell λ > (*) <$> Just 3 <*> Just 5  
Just 15

Haskell λ > Just (*3) <*> Just 5 
Just 15
```

#### [Chapter 18 - Monad](https://github.com/giocosmiano/haskell-programming/tree/master/ch18-monad)
 - Laws

 - `do` syntax and monads

 - Application and composition

 - Think of `Monads` as another way of applying functions over structure, with the ability of the function
   to alter the structure, something we’ve not seen in `Functor` and `Applicative`. `Monad` can inject more
   structure. However, it has the ability to flatten those two layers of structure into one is what
   makes `Monad` special. And it’s by putting that `join` function together with the mapping function
   that we get `bind`, also known as `>>=`.

 - The `Monad` type class is essentially a **generalized structure manipulation with some laws** to make
   it sensible. Just like `Functor` and `Applicative`.

 - Sample `Monad` that uses `join` implicitly to flatten the structure within structure
```haskell
Haskell λ > getLine >>= putStrLn
hello world
hello world

Haskell λ > :t getLine >>= putStrLn
getLine >>= putStrLn :: IO ()
```

 - As oppose to a `Functor`
```haskell
Haskell λ > putStrLn <$> getLine
hello world

Haskell λ > :t putStrLn <$> getLine
putStrLn <$> getLine :: IO (IO ())
```

 - To turn the `Functor` into a `Monad`, use the `join` explicitly to flatten the structure within structure
```haskell
Haskell λ > import Control.Monad

Haskell λ > join $ putStrLn <$> getLine
hello world
hello world

Haskell λ > :t join $ putStrLn <$> getLine
join $ putStrLn <$> getLine :: IO ()
```

#### [Chapter 19 - Applying structure](https://github.com/giocosmiano/haskell-programming/tree/master/ch19-applying-structure)
 - Monoid
 - Functor
 - Applicative
 - Monad

#### [Chapter 20 - Foldable](https://github.com/giocosmiano/haskell-programming/tree/master/ch20-foldable)
 - Revenge of the monoids

 - Foldable is a type class of data structures that can be folded to a summary value.

```haskell
Haskell λ > import Data.Functor.Constant

Haskell λ > foldr (*) 3 (Constant 5)
3

Haskell λ > foldr (*) 1 [1,2,3,4,5]
120
```

#### [Chapter 21 - Traversable](https://github.com/giocosmiano/haskell-programming/tree/master/ch21-traversable)
 - Laws

 - traverse, sequenceA

 - Traversable allows you to transform elements inside the structure like a `Functor`, producing `Applicative` effects
   along the way, and lift those potentially multiple instances of `Applicative` structure outside of the traversable
   structure. It is commonly described as a way to traverse a data structure, mapping a function inside a structure
   while accumulating the applicative contexts along the way.

 - `traverse` is mapping a function over some embedded value(s), like `fmap`, but similar to `flip bind`, that function is itself
   generating more structure. However, unlike `flip bind`, that structure can be of a different type than the
   structure we lifted over to apply the function. And at the end, it will flip the two structures around,
   as sequenceA did.
```haskell
Haskell λ > import Data.Functor.Identity

Haskell λ > traverse (Identity . (+1)) [1, 2, 3]
Identity [2,3,4]

Haskell λ > runIdentity $ traverse (Identity . (+1)) [1, 2, 3]
[2,3,4]
```

 - `sequenceA` is flipping two contexts or structures. It doesn’t by itself allow you to apply any function to the a value
   inside the structure; it only flips the layers of structure around.
```haskell
Haskell λ > sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
```

 - ~~On the [SkiFreeExercises](https://github.com/giocosmiano/haskell-programming/blob/master/ch21-traversable/SkiFreeExercises.hs),
   I'm able to write the `Functor`, `Applicative`, `Foldable` and `Traversable` instance of `S` structure, where `fa` is also a
   functorial structure. The `Monad` instance, however, is failing on the `right identity law` running the `quickBatch` check from
   [quickCheck](http://hackage.haskell.org/package/QuickCheck). I still have to figure out how-to fix this correctly.~~
```haskell
data S fa a = S (fa a) a deriving (Eq, Show)

-- TODO: fix the implementation of `monad` S
instance (Monad fa) => Monad (S fa) where
  return = pure
--  (S fa a) >>= f = S (fa >>= f) (f a)
  (S fa a) >>= f =
    let S fa' a' = f a
    in  S fa' a'
```

```haskell
monad laws:
  left  identity: +++ OK, passed 500 tests.
  right identity: *** Failed! Falsifiable (after 1 test): S [] 0
  associativity:  +++ OK, passed 500 tests.
```

 - After finishing [Chapter 25 - Composing types](https://github.com/giocosmiano/haskell-programming#chapter-25---composing-types), it is
   **NOT** possible to compose `monad` but rather create a `monad transformer`.

   - I've updated [SkiFreeExercises](https://github.com/giocosmiano/haskell-programming/blob/master/ch21-traversable/SkiFreeExercises.hs)
     implementing `ST monad transformer` but still figuring out how to make `ST` monadic structure into `S` structure. I'll come back to this
     once I'm finished with [Chapter 26 - Monad Transfomers](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers)     

```haskell
data S fa a = S (fa a) a deriving (Eq, Show)

newtype ST fa a = ST { runST :: fa a } deriving (Eq, Show)
```

#### [Chapter 22 - Reader](https://github.com/giocosmiano/haskell-programming/tree/master/ch22-reader)
 - Breaking down the Functor of functions

 - Functions have an Applicative too

 - Monad of functions

 - Reader is a way of stringing functions together when all those functions are awaiting one input
   from a shared environment. The important intuition is that it’s another way of abstracting out
   function application and gives us a way to do computation in terms of an argument that hasn’t been
   supplied yet. We use this most often when we have a constant value that we will obtain from somewhere
   outside our program that will be an argument to a whole bunch of functions. Using `Reader` allows us
   to avoid passing that argument around explicitly.
```haskell
Haskell λ > (+) <$> (+3) <*> (*5) $ 7
45

Haskell λ > (+) <$> (runReader $ Reader (+3)) <*> (runReader $ Reader (*5)) $ 7
45
```

#### [Chapter 23 - State](https://github.com/giocosmiano/haskell-programming/tree/master/ch23-state)
 - `State` newtype

 - Random numbers

 - The `State` type in Haskell is a means of expressing state that may change in the course of evaluating code
   without resort to mutation. The monadic interface for State is more of a convenience than a strict
   necessity for working with State.

```haskell
Haskell λ > import Control.Monad.State

Haskell λ > (runState $ get >> put 5 >> return 9 >> modify (+3) >> return 12 >> modify (*5) >> return 9001) 3
(9001,40)
```

#### [Chapter 24 - Parser combinators](https://github.com/giocosmiano/haskell-programming/tree/master/ch24-parser-combinators)
 - Parser

 - Parser combinator

 - Haskell’s parsing ecosystem

 - Marshalling from an AST to a datatype

 - The exercises on this chapter has a very substantial resources that really jogged my brain. 

#### [Chapter 25 - Composing types](https://github.com/giocosmiano/haskell-programming/tree/master/ch25-composing-types)
 - Composing types

   - Essentially, a structure that has multi-layered structures in it

   - `Functors` and `Applicatives` are both closed under composition, which means we can compose two `functors`
     (or two `applicatives`) and return another `functor` (or `applicative`, as the case may be). This is not true
     of `monads`, however; when we compose two `monads`, the result is **`NOT`** necessarily another `monad`.

```haskell
newtype ComposeType f g h a = ComposeType { getComposeType :: f (g (h a)) } deriving (Eq, Show)
```

 - Monad Transformers

   - Because we can't compose two `monads` together and create a new `Monad`, we need `Monad Transformer` to reduce
     the polymorphism and get concrete information about one of the `monads` that we’re working with, to make the
     `join` happen   

 - IdentityT

 - Good chapter in preparation for `Monad Transformer`. 

#### [Chapter 26 - Monad transformers](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers)

 - MaybeT, EitherT/ExceptT, ReaderT, StateT
 
 - [Lexically Inner is Structurally Outer](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers#lexically-inner-is-structurally-outer)
 
 - [MonadTrans](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers#monadtrans)
 
 - [MonadIO](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers#monadio)

 - Whoa!!! This chapter is loaded with great materials. Great job by the authors explaining and going thru it step-by-step.

#### [Chapter 27 - Non-Strictness](https://github.com/giocosmiano/haskell-programming/tree/master/ch27-non-strictness)

 - Laziness, Non-Strict

 - Thunk, Sprint

 - Forcing Sharing
 
 - Bang Patterns
 
 - Strict and StrictData

#### [Chapter 28 - Basic Libraries](https://github.com/giocosmiano/haskell-programming/tree/master/ch28-basic-libraries)
 
 - Benchmarking with [criterion](https://hackage.haskell.org/package/criterion)

 - [Profiling - Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)

 - Map, Set, Sequence, Vector, String types

 - [Demystifying DList](http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/)   

 - [A Sort of Difference](https://archive.is/20140131124629/http://web.archive.org/web/20080918101635/comonad.com/reader/2008/a-sort-of-difference/)

#### [Chapter 29 - IO](https://github.com/giocosmiano/haskell-programming/tree/master/ch29-io)

 - MVar

 - IO’s Functor, Applicative, and Monad

 - [Brent Yorgey - IO actions are like recipes. "Recipe for a cake doesn't give us a cake but some instructions for how to make a
  cake."](https://www.cis.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html)

#### [Chapter 30 - When Things Go Wrong](https://github.com/giocosmiano/haskell-programming/tree/master/ch30-when-things-go-wrong)

 - Exceptions, try/catch, throw/throwIO, custom exception, async exception

#### [Chapter 31 - Final Project](https://github.com/giocosmiano/haskell-programming/tree/master/ch31-final-project)

 - [finger deamon](http://www.rajivshah.com/Case_Studies/Finger/Finger.htm)

### Referenced frameworks/libraries
 - [wreq - An easy-to-use HTTP client library](https://hackage.haskell.org/package/wreq)
 - [scotty - Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp](https://hackage.haskell.org/package/scotty)
 - [hspec - A Testing Framework for Haskell](https://hackage.haskell.org/package/hspec)
 - [checkers - Check properties on standard classes and data structures](https://hackage.haskell.org/package/checkers)
 - [pandoc - Conversion between markup formats](https://hackage.haskell.org/package/pandoc)
 - [aeson - Fast JSON parsing and encoding](https://hackage.haskell.org/package/aeson)
 - [cassava - A CSV parsing and encoding library](https://hackage.haskell.org/package/cassava)
 - [parsers - Parsing combinators](https://hackage.haskell.org/package/parsers)
 - [trifecta - modern parser combinator library with convenient diagnostics](https://hackage.haskell.org/package/trifecta-1.5.2)
 - [raw-strings-qq - Raw string literals for Haskell](https://hackage.haskell.org/package/raw-strings-qq)
 - [criterion - Robust, reliable performance measurement and analysis](https://hackage.haskell.org/package/criterion)
 - [containers - Assorted concrete container types](https://hackage.haskell.org/package/containers)
 - [vector - Efficient Arrays](https://hackage.haskell.org/package/vector)
 - [array - Mutable and immutable arrays](http://hackage.haskell.org/package/array)
 - [text - An efficient packed Unicode text type](http://hackage.haskell.org/package/text)
 - [dlist - Difference lists](http://hackage.haskell.org/package/dlist)
 - [twitter-conduit - Twitter API package with conduit interface and Streaming API support](http://hackage.haskell.org/package/twitter-conduit)
 - [http-client - An HTTP client engine](http://hackage.haskell.org/package/http-client)
 - [network - Low-level networking interface](https://hackage.haskell.org/package/network)
 - [sqlite-simple - Mid-Level SQLite client library](https://hackage.haskell.org/package/sqlite-simple)
 - [optparse-applicative - Utilities and combinators for parsing command line options](https://hackage.haskell.org/package/optparse-applicative)

### For further reading
 - [Why Functional Programming Matters - by John Hughes](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)
 - [What I Wish I Knew When Learning Haskell - by Stephen Diehl](http://dev.stephendiehl.com/hask/#monads)
 - [Haskell for all - by Gabriel Gonzalez](http://www.haskellforall.com/)
 - [How to desugar Haskell code - by Gabriel Gonzalez](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html)
 - [What a Monad is not](https://wiki.haskell.org/What_a_Monad_is_not)
 - [Real World Haskell - by Bryan O'Sullivan](http://book.realworldhaskell.org/read/)
 - [Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html)
 - [Write You a Haskell - by Stephen Diehl](http://dev.stephendiehl.com/fun/index.html)
 - [Haskell Basics - by Stephen Diehl](http://dev.stephendiehl.com/fun/001_basics.html)
 - [School of Haskell](https://www.schoolofhaskell.com/)
 - [School of Haskell - Starting with Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell)
 - [School of Haskell - To Infinity and Beyond](https://www.schoolofhaskell.com/user/school/to-infinity-and-beyond)
 - [School of Haskell - Simple Examples](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples)
 - [School of Haskell - Pick of the Week](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week)
 - [Typeclassopedia - by Brent Yorgey](https://wiki.haskell.org/Typeclassopedia)
 - Introduction to Haskell - by Brent Yorgey    
   - [Introduction to Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell)   
   - [CIS 194 Introduction to Haskell Fall 2016](http://www.cis.upenn.edu/~cis194/fall16/)   
   - [CIS 194 Introduction to Haskell Spring 2013](http://www.cis.upenn.edu/~cis194/spring13/lectures.html)   

### IDE/IntelliJ Plugins
 - [HaskForce](http://haskforce.com/)
 - [Atom Haskell ghc-mod](https://atom-haskell.github.io/core-packages/haskell-ghc-mod/)
 - [Stack Install ghc-mod](https://github.com/DanielG/ghc-mod/wiki/Installing)
 - [Stack install ghc-mod failure fix](https://stackoverflow.com/questions/50948485/stack-install-ghc-mod-fails-with-dependencies-conflicts-on-osx-10-13-4)
 - [stylish-haskell - Haskell code prettifier](http://hackage.haskell.org/package/stylish-haskell)
 - [hindent - Extensible Haskell pretty printer](https://hackage.haskell.org/package/hindent)
 - [hlint - Source code suggestions](https://hackage.haskell.org/package/hlint)
 - [haskell-docs - Program to find and display the docs and type of a name](https://hackage.haskell.org/package/haskell-docs)

### [GHC - Glasgow Haskell Compiler](https://www.haskell.org/ghc/)

 - [Version 8.0.2](https://www.haskell.org/ghc/download_ghc_8_0_2.html)
   - Use this version, with `ghc-mod v5.8.0.0`, so that [HaskForce](http://haskforce.com/) works as a plugin in IntelliJ

### [Stack - Cross-platform program for developing Haskell projects](https://www.haskellstack.org/)

 - [User Guide](https://docs.haskellstack.org/en/stable/GUIDE)
 - [Build command](https://docs.haskellstack.org/en/stable/build_command)
 - [Versions and installations](https://docs.haskellstack.org/en/stable/faq)
 - [How to Script with Stack](https://haskell-lang.org/tutorial/stack-script)
 - [How to Play with Stack](https://haskell-lang.org/tutorial/stack-play)
 - [How to Build with Stack](https://haskell-lang.org/tutorial/stack-build)

```bash
$ stack exec -- ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.0.2

$ stack exec -- ghc-mod --version
ghc-mod version 5.8.0.0 compiled by GHC 8.0.2

$ stack install scotty

$ stack install pandoc

$ stack exec -- pandoc --version
pandoc.EXE 2.2.1
Compiled with pandoc-types 1.17.5.4, texmath 0.11.1.2, skylighting 0.7.5
Default user data directory: C:\Users\Gio\AppData\Roaming\pandoc
```

  - Creating executable file (make sure there's a `Main` module)
     
```bash
$ cd "C:\Users\Gio\Documents\_haskell\haskell-programming\ch30-when-things-go-wrong"

$ stack ghc -- WritingWithException.hs -o WritingWithException.exe
[1 of 1] Compiling Main             ( WritingWithException.hs, WritingWithException.o )
Linking WritingWithException.exe ...

$ ./WritingWithException.exe
wrote to file
```

  - Running the script without creating an executable file. Can't create an executable without `Main` module.
     
```bash
$ cd "C:\Users\Gio\Documents\_haskell\haskell-programming\ch29-io"

$ stack VigenereCipherExercises.hs -e ALLY VigenereCipherExercises.hs           VigenereCipherExercises.encrypt.log

$ stack VigenereCipherExercises.hs -d ALLY VigenereCipherExercises.encrypt.log  VigenereCipherExercises.decrypt.log

$ stack ghc -- VigenereCipherExercises.hs -o VigenereCipherExercises.exe
<no location info>: error:
    output was redirected with -o, but no output will be generated
because there is no Main module.
```

  - [path](https://docs.haskellstack.org/en/stable/GUIDE/#path)
  - [env](https://docs.haskellstack.org/en/stable/GUIDE/#exec)
  - [repl](https://docs.haskellstack.org/en/stable/GUIDE/#ghci-the-repl)
  - [ghc/runghc](https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc)
  - [script interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)
  - [useful features](https://docs.haskellstack.org/en/stable/GUIDE/#fun-features)
  - [power user commands](https://docs.haskellstack.org/en/stable/GUIDE/#power-user-commands)
  - [unpack - to download package source](https://docs.haskellstack.org/en/stable/GUIDE/#existing-projects)
     
```bash
$ stack path

$ stack exec env

$ stack ghci

$ stack unpack scotty
```














 
