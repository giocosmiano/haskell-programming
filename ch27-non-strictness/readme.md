### Non-Strictness
 - Technically `Haskell` is only obligated to be non-strict, not lazy. A truly lazy language memoizes, or holds in memory, the results
   of all the functions it does evaluate. Implementations of `Haskell`, such as GHC Haskell, are only obligated to be non-strict such
   that they have the same behavior with respect to [bottom](https://wiki.haskell.org/Bottom); they are not required to take a particular
   approach to how the program executes or how efficiently it does so. The essence of non-strictness is that you can have an expression
   which results in a value, even if [bottom or infinite data lurks within](https://wiki.haskell.org/Bottom)

### Outside in, inside out
 - Strict languages evaluate inside out; non-strict languages like `Haskell` evaluate outside in. Outside in means that evaluation
   proceeds from the outermost parts of expressions and works inward based on what values are forced.

 - In `Haskell`, we evaluate expressions when we need them rather than when they are first referred to or constructed. This is one
   of the ways in which non-strictness makes `Haskell` expressive, we can refer to values before we’ve done the work to create
   them.

### Thunk, Sprint
 - `Thunk` is used to reference suspended computations that might be performed or computed at a later point in the program. 

 - The `sprint` command allows us to show what has been evaluated already by printing in the REPL. An underscore is used to represent
   values that haven’t been evaluated yet.

```haskell
Haskell λ > let myList = [1, 2, undefined] :: [Integer]
myList :: [Integer]

Haskell λ > :sprint myList
myList = [1,2,_]

Haskell λ > let myList2 = [1, 2, undefined]
myList2 :: Num t => [t]

Haskell λ > :sprint myList2
myList2 = _

Haskell λ > let myList3 = [1, 2, id 1] :: [Integer]
myList3 :: [Integer]

Haskell λ > :sprint myList3
myList3 = [1,2,_]

Haskell λ > let myList4 = myList3 ++ undefined
myList4 :: [Integer]

Haskell λ > :sprint myList4
myList4 = _
``` 

### Forcing Sharing
 - We can force sharing by giving the expression a name. The most common way of doing this is with `let`. 

 - Why the `let` expression? We want sharing here so that running a monadic action indefinitely doesn’t leak memory.
   The sharing here causes GHC to overwrite the thunk as it runs each step in the evaluation, which is quite handy. Otherwise,
   it would keep constructing new thunks indefinitely and that would be very unfortunate.

```haskell
Haskell λ > import Control.Monad

Haskell λ > :t forever
forever :: Monad m => m a -> m b
forever a = let a' = a >> a' in a'
```

### For further reading
 - [Chapter 2. Basic Parallelism: Parallel and Concurrent Programming in Haskell - by Simon Marlow](https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/)
 - [The Incomplete Guide to Lazy Evaluation in Haskell](https://hackhands.com/guide-lazy-evaluation-haskell/)
 - [Lazy evaluation illustrated for Haskell divers - by Takenobu Tani](https://github.com/takenobu-hs/lazy_evaluation)
 - [Core Type - Glasgow Haskell Compiler](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType#Caseexpressions)
 - [Laziness - Haskell Wiki](https://en.wikibooks.org/wiki/Haskell/Laziness#Thunks_and_Weak_head_normal_form)
 - [Weak Head Normal Form](https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form)
