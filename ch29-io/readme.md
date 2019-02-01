### IO
 - Haskell expressions which aren’t in `IO` will always return the same result regardless of what order they are evaluated in; we lose
   this guarantee and others besides once `IO` is introduced. GHC can normally reorder operations. This is disabled in `IO` (as in ST).
   `IO` actions are instead enclosed within nested lambdas nesting is the only way to ensure that actions are sequenced within
   a pure lambda calculus.

 - `IO` primarily exists to give us a way to order operations and to disable some of the sharing about non-strictness. GHC is ordinarily
   free to do a lot of reordering of operations, delaying of evaluation, sharing of named values, duplicating code via inlining,
   and other optimizations in order to increase performance. The main thing the `IO` type does is turn off most of those abilities.

### For further reading
 - [Referential Transparency - Haskell Wiki](https://wiki.haskell.org/Referential_transparency)   
 - [IO Inside - Haskell Wiki](https://wiki.haskell.org/IO_inside)
 - [Unraveling the mystery of the IO monad - by Edward Yang](http://blog.ezyang.com/2011/05/unraveling-the-mystery-of-the-io-monad/)
 - [Primitive Haskell - by Michael Snoyman](https://haskell-lang.org/tutorial/primitive-haskell)   
 - [Evaluation Order and State Tokens - by Michael Snoyman](https://wiki.haskell.org/Evaluation_order_and_state_tokens)   
 - [Tackling the Awkward Squad - by Simon Peyton Jones](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf)   
 - [Notions of computation and monads - by Eugenio Moggi](http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf)   
 - [IO - Introduction to Haskell - by Brent Yorgey](http://www.cis.upenn.edu/~cis194/spring13/lectures/08-IO.html)   
