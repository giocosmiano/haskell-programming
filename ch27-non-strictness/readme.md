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

Haskell λ > :sprint myList3
myList4 = _
``` 

### For further reading
 - [Streaming logging - by Gabriel Gonzalez](http://www.haskellforall.com/2014/02/streaming-logging.html)
 - [Amb](https://wiki.haskell.org/Amb)
   - [McCarthy's Ambiguous Operator](http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2005/10/11/amb-operator/)
 - [A Simple Reader Monad Example](https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html)
 - [Reader Monad Transformer - by Carlo Hamalainen](https://carlo-hamalainen.net/2014/03/05/note-to-self-reader-monad-transformer/)
 - [The ReaderT Design Pattern - by FPComplete](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)

