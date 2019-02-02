### IO
 - Haskell expressions which aren’t in `IO` will always return the same result regardless of what order they are evaluated in; we lose
   this guarantee and others besides once `IO` is introduced.

 - Essentially, `IO` primarily exists to give us a way to order operations and to disable some of the sharing about non-strictness.
   GHC is ordinarily free to do a lot of reordering of operations, delaying of evaluation, sharing of named values, duplicating code
   via in-lining, and other optimizations in order to increase performance. The main thing the `IO` type does is turn off most of those
   abilities.

 - Because GHC can normally reorder operations, this is disabled in `IO` (as in ST). `IO` actions are instead enclosed within nested
   lambdas — nesting is the only way to ensure that actions are sequenced within a pure lambda calculus. In fact, the reason we have
   `Monad` is because it was a means of abstracting away the nested lambda noise that underlies `IO`.

 - `IO` type doesn't guarantee anything. Values of type `IO a` are not an `a`; they’re a description of how we might get an `a`.
   Something of type `IO String` is not a computation that, if evaluated, will result in a `String`; it’s a description of how we might
   get that `String` from the “real world,” possibly performing effects along the way.

   - [Brent Yorgey - IO actions are like recipes. "Recipe for a cake doesn't give us a cake but some instructions for how to make a
  cake."](https://www.cis.upenn.edu/~cis194/spring13/lectures/08-IO.html)
 
### MVar

 - The `MVar` type is a means of synchronizing shared data in Haskell. To give a very cursory overview, the `MVar` can hold one value
   at a time. You put a value into it; it holds onto it until you take that value out.
   [Then and only then can you put another cat in the box.](https://twitter.com/argumatronic/status/631158432859488258)

### IO’s Functor, Applicative, and Monad

```haskell
fmap  ::    (a -> b) -> IO a        -> IO b

(<*>) :: IO (a -> b) -> IO a        -> IO b

(>>=) :: IO  a       -> (a -> IO b) -> IO b

join  :: IO (IO a)   -> IO a
```

```haskell
Haskell λ > import System.Random

Haskell λ > import Control.Monad

Haskell λ > fmap (+1) (randomIO :: IO Int)
1106618420421875278

Haskell λ > (++) <$> getLine <*> getLine
hello
world
"helloworld"

Haskell λ > (+) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)
-4550545069234707

Haskell λ > let embedInIO = return :: a -> IO a

Haskell λ > let str = "hello world"

Haskell λ > embedInIO (print str)

Haskell λ > join $ embedInIO (print str)
"hello world"
```

### For further reading
 - [Referential Transparency - Haskell Wiki](https://wiki.haskell.org/Referential_transparency)   
 - [IO Inside - Haskell Wiki](https://wiki.haskell.org/IO_inside)
 - [Unraveling the mystery of the IO monad - by Edward Yang](http://blog.ezyang.com/2011/05/unraveling-the-mystery-of-the-io-monad/)
 - [Primitive Haskell - by Michael Snoyman](https://haskell-lang.org/tutorial/primitive-haskell)   
 - [Evaluation Order and State Tokens - by Michael Snoyman](https://wiki.haskell.org/Evaluation_order_and_state_tokens)   
 - [Tackling the Awkward Squad - by Simon Peyton Jones](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf)   
 - [Notions of computation and monads - by Eugenio Moggi](http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf)   
 - [IO - Introduction to Haskell - by Brent Yorgey](http://www.cis.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html)   
