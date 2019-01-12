### This is my personal repository learning `Haskell` from [Haskell Programming](http://haskellbook.com/)

 - I came to this `Haskell` journey because of my curiosity in FP that began mid-2016 when I was working on a UI story/task in
   [Angular](https://angular.io/) and used these JS libraries, [RamdaJS](https://ramdajs.com/), [RxJS](http://reactivex.io/) and
   [ReduxJS](https://redux.js.org/), that are primarily developed with FP concepts in mind, such as immutability, composition, high-order
   function etc.

   Not until mid-2018 that I picked up this book to learn. I can say that the authors did an excellent job writing this, shout-out to them.
   I'm having fun doing the chapter exercises as they force me to think, connect the dots and perform [diagram chasing](https://en.wikipedia.org/wiki/Commutative_diagram)         

### Notes about my journey in working through chapter exercises

 - Functional programming is a function of data transformation and composition. This concept helps me in a big way, reminding me to get back
   to this simple pattern `f x = y` every time I get lost where to apply the function on layers of functorial structures, i. e. structure within
   structure, of `Functor`, `Applicative` and `Monad`. 
   - `f x = y`
     - `f` - function
     - `x` - input such as Integer, String or an [algebraic data structure](https://en.wikipedia.org/wiki/Algebraic_data_type) such as Product, Employee etc
     - `y` - output such as Integer, String or an [algebraic data structure](https://en.wikipedia.org/wiki/Algebraic_data_type) such as Product, Employee etc

 - Functions are evaluated and eventually reduced into a value called [Beta reduction](https://wiki.haskell.org/Beta_reduction) 

 - Function, by default, is curried meaning it only accepts 1-argument and returns either
   - the reduced value
   - OR another function that will be further applied, and so on, that will eventually reduced into a value   

 - Functions are data because they will eventually get evaluated, and reduced into a value. See [Chapter 22 - Reader](https://github.com/giocosmiano/haskell-programming#chapter-22---reader)  

 - Always helpful to use **_`:t`_** for type, **_`:k`_** for kind or **_`:i`_** info to get more details   

#### Chapter 1 - All You Need is Lambda
 - My background is primarily OOP, and have a very good understanding of FP working with [Scala](https://www.scala-lang.org/) and
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
 - Polymorphism
 - Type inference

#### [Chapter 6 - Type classes](https://github.com/giocosmiano/haskell-programming/tree/master/ch06-typeclasses)
 - In OOP such as Java, this is similar to defining `interface`. `Haskell` data types doesn't have any function
   associated with it unlike Java wherein functions are automatically inherited from
   [Object](https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html).
   - e.g. Java Object class has toString, equals, hashCode, clone etc. 
 - Type classes Eq, Num, Ord, Enum, Show, Read
 - Type class inheritance

#### [Chapter 7 - More functional patterns](https://github.com/giocosmiano/haskell-programming/tree/master/ch07-more-functional-patterns)
 - Anonymous functions
 - Pattern matching
 - Case expressions
 - Guards
 - Function composition
 - Point-free style

#### [Chapter 8 - Recursion](https://github.com/giocosmiano/haskell-programming/tree/master/ch08-recursion)
 - [Bottom](https://en.wikipedia.org/wiki/Bottom_type)
 - Factorial! and Fibonacci numbers

#### [Chapter 9 - Lists](https://github.com/giocosmiano/haskell-programming/tree/master/ch09-lists)
 - Pattern matching on lists
 - List comprehensions
 - Spines and non-strict evaluation
 - Zipping lists

#### [Chapter 10 - Folding lists](https://github.com/giocosmiano/haskell-programming/tree/master/ch10-folding-lists)
 - Recursive patterns
 - [foldr vs foldl](https://wiki.haskell.org/Fold)
   - This chapter helped a lot as it preps me for [Chapter 20 - Foldable](https://github.com/giocosmiano/haskell-programming#chapter-20---foldable)
     and [Chapter 21 - Traversable](https://github.com/giocosmiano/haskell-programming#chapter-21---traversable)

#### [Chapter 11 - Algebraic data types](https://github.com/giocosmiano/haskell-programming/tree/master/ch11-algebraic-datatypes)
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
   mentioned in the book that it's **NOT** possible to compose `monad` but rather create a `monad transformer`.

   - I've updated [SkiFreeExercises](https://github.com/giocosmiano/haskell-programming/blob/master/ch21-traversable/SkiFreeExercises.hs)
     implementing `ST monad transformer` but still don't know how to make `ST` into `S` structure. I'll see some more on `Chapter 26 - Monad Transfomers`     

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

   - Essentially, a structure that has layer(s) of structure in it

   - `Functors` and `Applicatives` are both closed under composition, which means we can compose two `functors`
     (or two `applicatives`) and return another `functor` (or `applicative`, as the case may be). This is not true
     of `monads`, however; when we compose two `monads`, the result is **`NOT`** necessarily another `monad`.

```haskell
newtype SomeType f g a = SomeType { getSomeType :: f (g a) } deriving (Eq, Show)
```

 - Monad Transformers

   - Because we can't compose two `monads` together and create a new `Monad`, we need `Monad Transformer` to reduce
     the polymorphism and get concrete information about one of the `monads` that we’re working with, to make the
     `join` happen   

 - IdentityT

   - Reminder to myself - come back to this chapter when you get lost in `Monad Transformer`
     [diagram chasing](https://en.wikipedia.org/wiki/Commutative_diagram#Diagram_chasing) because it is covered in
     great details. Great job by the authors explaining and going thru it step-by-step.

#### [Chapter 26 - Monad transformers](https://github.com/giocosmiano/haskell-programming/tree/master/ch26-monad-transformers)

 - MaybeT, EitherT, ReaderT, StateT

### For further reading
 - [What I Wish I Knew When Learning Haskell - by Stephen Diehl](http://dev.stephendiehl.com/hask/#monads)
 - [Haskell for all - by Gabriel Gonzalez](http://www.haskellforall.com/)
 - [How to desugar Haskell code - by Gabriel Gonzalez](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html)
 - [What a Monad is not](https://wiki.haskell.org/What_a_Monad_is_not)
 - [Real World Haskell - by Bryan O'Sullivan](http://book.realworldhaskell.org/read/)
 - [School of Haskel](https://www.schoolofhaskell.com/)
 - Lenses
   - [Lenses - by Gabriel Gonzalez](http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers_28.html)
   - [A Little Lens Starter Tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
   - [Taking a Close look at Lenses](https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses)















 
