## This is my personal repository learning `Haskell` from this excellent book [Haskell Programming, by Christopher Allen And Julie Moronuki](http://haskellbook.com/)

### Notes on my journey from working on chapter exercises

 - Functions are evaluated and reduced into a value called [Beta reduction](https://wiki.haskell.org/Beta_reduction) 

 - Functional programming is a function of data transformation. This concept helps me in a big way into understanding
   `Functor`, `Applicative` and eventually `Monad` 
   - `f x = y`
     - `f` - function
     - `x` - input such as Integer, Bool, String or even complex [algebraic data structure](https://en.wikipedia.org/wiki/Algebraic_data_type)
       such as Product, Employee etc
     - `y` - output such as Integer, Bool, String or even complex [algebraic data structure](https://en.wikipedia.org/wiki/Algebraic_data_type)
       such as Product, Employee etc

 - Functions, by default is curried, meaning it only accepts 1-argument and returns either
   - the reduced value
   - OR another function that will be further applied and eventually will reduced into a value   

 - Functions are data because they're high-order function. See `Chapter 22 - Reader`  

#### Chapter 1 - All You Need is Lambda
 - I have an OOP background, and have a very good understanding of FP working with [Scala](https://www.scala-lang.org/) and
   [ES6 high order functions](https://eloquentjavascript.net/05_higher_order.html) but this chapter provides
   a good foundation on understanding of [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

```haskell
Haskell λ > let addAndMultiply = \x -> \y -> \z -> x + y * z

Haskell λ > addAndMultiply 3 5 7
38
```

#### Chapter 2 - Hello, Haskell!
 - Introduction to the following `Haskell` subjects
   - Function and evaluation
   - Infix operators
   - Associativity and precedence
   - Expression components such as `let` and `where`

#### Chapter 3 - Strings
 - Printing strings
 - Concatenation functions

#### Chapter 4 - Basic data types
 - Built-in data types
 - Integral vs Int, Integer and Word
 - Bool and comparing values
 - Tuples and Lists

#### Chapter 5 - Types
 - [Currying](https://wiki.haskell.org/Currying)
 - Polymorphism
 - Type inference

#### Chapter 6 - Type classes
 - In OOP such as Java, this is similar to defining `interface`. `Haskell` data types doesn't have any function
   associated with it unlike Java wherein functions are automatically inherited from
   [Object](https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html).
   - e.g. Java Object class has toString, equals, hashCode, clone etc. 
 - Type classes Eq, Num, Ord, Enum, Show, Read
 - Type class inheritance

#### Chapter 7 - More functional patterns
 - Anonymous functions
 - Pattern matching
 - Case expressions
 - Guards
 - Function composition
 - Point-free style

#### Chapter 8 - Recursion
 - [Bottom](https://en.wikipedia.org/wiki/Bottom_type)
 - Factorial! and Fibonacci numbers

#### Chapter 9 - Lists
 - Pattern matching on lists
 - List comprehensions
 - Spines and non-strict evaluation
 - Zipping lists

#### Chapter 10 - Folding lists
 - Recursive patterns
 - [foldr vs foldl](https://wiki.haskell.org/Fold)
   - This chapter helps me a great deal as it preps me for `Chapter 20 - Foldable` and `Chapter 21 - Traversable`

#### Chapter 11 - Algebraic data types
 - Data and type constructors
 - Type constructors and kinds
 - Data constructors and values
 - Data constructor arities
 - newtype, Sum types, Product types
 - Function type is exponential
 - Higher-kinded data types

#### Chapter 12 - Signaling adversity
 - Nothing, or Just Maybe
 - Either left or right, but not both
 - higher-kindedness

#### Chapter 13 - Building projects
 - Modules
 - Making packages with [Stack](https://docs.haskellstack.org)
 - Building a project

#### Chapter 14 - Testing
 - [QuickCheck](http://hackage.haskell.org/package/QuickCheck)
 - Arbitrary instances

#### Chapter 15 - Monoid, Semigroup
 - Algebra and Laws

 - use of `newtype`
   - To signal intent: using newtype makes it clear that you only intend for it to be a
     wrapper for the underlying type. The newtype cannot eventually grow into a more
     complicated sum or product type, while a normal datatype can

   - To improve type safety: avoid mixing up many values of the same representation,
     such as Text or Integer

   - To add different type class instances to a type that is otherwise unchanged
     representationally, such as with Sum and Product

#### Chapter 16 - Functor
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

#### Chapter 17 - Applicative
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

#### Chapter 18 - Monad
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

```haskell
Haskell λ > getLine >>= putStrLn
hello world
hello world
```

#### Chapter 19 - Applying structure
 - Monoid
 - Functor
 - Applicative
 - Monad

#### Chapter 20 - Foldable
 - Revenge of the monoids
 - Foldable is a type class of data structures that can be folded to a summary value.

```haskell
Haskell λ > import Data.Functor.Constant

Haskell λ > foldr (*) 3 (Constant 5)
3

Haskell λ > foldr (*) 1 [1,2,3,4,5]
120
```

#### Chapter 21 - Traversable
 - Laws
 - traverse, sequenceA
 - Traversable allows you to transform elements inside the structure like a `Functor`, producing `Applicative` effects
   along the way, and lift those potentially multiple instances of `Applicative` structure outside of the traversable
   structure. It is commonly described as a way to traverse a data structure, mapping a function inside a structure
   while accumulating the applicative contexts along the way.
```haskell
Haskell λ > import Data.Functor.Identity

Haskell λ > traverse (Identity . (+1)) [1, 2, 3]
Identity [2,3,4]

Haskell λ > runIdentity $ traverse (Identity . (+1)) [1, 2, 3]
[2,3,4]

Haskell λ > sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
```

#### Chapter 22 - Reader
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

#### Chapter 23 - State
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

#### Chapter 24 - Parser combinators
 - Haskell’s parsing ecosystem
 - Marshalling from an AST to a datatype
 - A parser is a function that takes some textual input (it could be a String, or another datatype such as
   ByteString or Text) and returns some structure as an output. That structure might be a tree, for example,
   or an indexed map of locations in the parsed data. Parsers analyze structure in conformance with rules
   specified in a grammar, whether it’s a grammar of a human language, a programming language, or a format
   such as JSON.
 - A parser combinator is a higher-order function that takes parsers as input and returns a new parser as output.
















 
