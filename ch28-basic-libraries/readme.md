### Benchmarking with Criterion
 - `Weak head normal form` evaluates to the first data constructor. That means that if our outermost data constructor is a Maybe,
   it’s only going to evaluate enough to find out if it’s a `Nothing` or a `Just` — if there is a `Just a`, it won’t count the cost
   of evaluating the `a` value.

 - Using `nf` would mean we wanted to include the cost of fully evaluating the `a` as well as the first data constructor.

 - The key when determining whether we want `whnf` or `nf` is to think about what we’re trying to benchmark and if reaching the first
   data constructor will do all the work we’re trying to measure or not. In general, ask yourself, “when I have reached the first data
   constructor, have I done most or all of the work that matters?”

### Referenced frameworks/libraries
 - [criterion - Robust, reliable performance measurement and analysis](https://hackage.haskell.org/package/criterion)
 - [containers - Assorted concrete container types](https://hackage.haskell.org/package/containers)
   - [containers - Introduction and Tutorial](https://haskell-containers.readthedocs.io/en/latest/)
 - [vector - Efficient Arrays](https://hackage.haskell.org/package/vector)
 - [array - Mutable and immutable arrays](http://hackage.haskell.org/package/array)
 - [text - An efficient packed Unicode text type](http://hackage.haskell.org/package/text)
 - [dlist - Difference lists](http://hackage.haskell.org/package/dlist)

### For further reading
 - [Criterion Library - by Bryan O'Sullivan](http://www.serpentine.com/criterion/)   
   - [Criterion Tutorial](http://www.serpentine.com/criterion/tutorial.html)   
 - [Profiling - Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)
 - [Difference lists](https://github.com/spl/dlist)   
   - [Demystifying DList](http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/)   
   - [24 Days of Hackage - DList](https://ocharles.org.uk/blog/posts/2012-12-14-24-days-of-hackage-dlist.html)
   - [A Sort of Difference](https://archive.is/20140131124629/http://web.archive.org/web/20080918101635/comonad.com/reader/2008/a-sort-of-difference/)
   - [Constructing a list in a Monad](https://www.joachim-breitner.de/blog/620-Constructing_a_list_in_a_Monad)   
   - [Why are difference lists more efficient than regular concatenation?](http://stackoverflow.com/questions/13879260/why-are-difference-lists-more-efficient-than-regular-concatenation/13879693#13879693)   
